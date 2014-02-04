open Batteries
open Common

module Json = Yojson.Safe

let get_assoc json =
  match json with
  | `Assoc x -> x
  | _ -> raise Not_found

let get_list json =
  match json with
  | `List l -> l
  | _ -> raise Not_found

let get_string json =
  match json with
  | `String l -> l
  | _ -> raise Not_found

let get_int json =
  match json with
  | `Int l -> l
  | _ -> raise Not_found

let get_linenumber (`Result result) = 
  try Some (get_int (List.assoc "n" result))
  with Not_found -> None

let get_tinyg str =
  let a = get_assoc (Json.from_string str) in
  try `Result (get_assoc (List.assoc "r" a))
  with Not_found -> 
    try `Status (get_assoc (List.assoc "sr" a))
    with Not_found ->
      try `Queue_report (get_int (List.assoc "qr" a))
      with Not_found -> `Other a

type result = (string * Json.json) list

type linenumber = int

type gcode_request = (string * (result -> unit))

type rt = {
  rt_line_callbacks : (linenumber, result -> unit) Hashtbl.t;
  rt_send_gcode : gcode_request -> unit; (* when you want a G-code request enqueued, call this. *)
}

let add_line_callback rt n fn =
  match n with
  | None -> failwith "fill in code"
  | Some n -> Hashtbl.add rt.rt_line_callbacks n fn

let debug = false
let verbose = false

let xon = Char.chr 17
let xoff = Char.chr 19

let queue_threshold = 20 (* once there are less than n slots available, don't send new commands *)

let activate future x = 
  future#set (`Ok x)

type ('b) tinyg_session = {
  ts_exited           : (unit -> unit);                    (* function to signal that the thread has finished *)
  ts_sigint_triggered : ((<get : unit option; ..>) as 'b); (* future that signals that an interrupt has been requested *)
  ts_fd               : Unix.file_descr;                   (* the open fd *)
  ts_signal_fd        : Unix.file_descr;                   (* fd usd to indicate there is data in the task_queue *)
  ts_task_queue       : (rt -> unit) Queue.t Protect.t;    (* incoming external tasks requiring synchronization *)
  ts_common_options   : Common.common_options;             (* common configuration *)
}

let receiver (ts : (_) tinyg_session) =
  let queue_full = ref false in
  let command_queue : gcode_request Queue.t = Queue.create () in
  let lines_sent = ref 0 in
  let rt = {
    rt_line_callbacks = Hashtbl.create 1024;
    rt_send_gcode = fun x -> Queue.add x command_queue;
  } in
  let write_queue = Queue.create () in
  let buf = String.create 1024 in
  let lb = LineBuffer.create () in
  let last_linenumber = ref 0 in
  let enable_send = ref true in
  let rec flush_queue () =
    if !enable_send && not !queue_full && not (Queue.is_empty command_queue) then
      let gcode, callback = Queue.take command_queue in
      incr lines_sent;
      let linenumber = !lines_sent in
      let msg = Printf.sprintf "{\"gc\":\"%s N%d\"}\r\n" gcode linenumber in
      if verbose then Printf.printf "-> %s%!" msg;
      add_line_callback rt (Some linenumber) callback;
      List.iter
	(fun c -> Queue.add c write_queue)
	(String.explode msg);
      flush_queue ()
  in
  let rec feed_lines () =
    let do_send = not (Queue.is_empty write_queue) && !enable_send in
    let timeout = 
      if do_send
      then 1.0 /. (float ts.ts_common_options.co_send_bps /. 8.0)
      else (-1.0)
    in
    let (rd, _, _) = Unix.select [ts.ts_fd; ts.ts_signal_fd] [] [] timeout in
    if ts.ts_sigint_triggered#get = Some () then
      ( ( try ignore (Unix.write ts.ts_fd "!" 0 1);
	  with _ -> () );
	`Aiee )
    else (
      if do_send then
	let c = Queue.take write_queue in
	buf.[0] <- c;
	let n = Unix.write ts.ts_fd buf 0 1 in
	if n <= 0 then
	  raise End_of_file
	else ();
	  else ();
        match () with
        | _ when List.mem ts.ts_fd rd ->
          let n = Unix.read ts.ts_fd buf 0 (String.length buf) in
          if n = 0 then
        raise End_of_file;
          for c = 0 to n - 1 do
        match buf.[c] with
        | ch when ch = xon ->
          if verbose then Printf.printf "*** XON ***\n%!";
          enable_send := true
        | ch when ch = xoff ->
          if verbose then Printf.printf "*** XOFF ***\n%!";
          enable_send := false
        | _ -> ()
          done;
          let strings = LineBuffer.append_substring lb buf 0 n in
          List.iter
        (fun str ->
          if verbose then Printf.printf "<-%s\n%!" str;
          match get_tinyg str with
          | `Result result as full_result ->
            ( match get_linenumber full_result with
            | Some linenumber ->
              let callback = Hashtbl.find rt.rt_line_callbacks linenumber in
              Hashtbl.remove rt.rt_line_callbacks linenumber;
              callback result
            | None ->
              Printf.printf "Response without linenumber\n%!"
            )
          | `Status status ->
            let current_linenumber =
              try Some (get_int (List.assoc "line" status))
              with Not_found -> None in
            ( match current_linenumber with
            | None -> ()
            | Some _current_linenumber -> ()
            )
          | `Queue_report r ->
            if debug then Printf.printf "**** Queue report: %d\n%!" r;
            queue_full := r < queue_threshold;
            flush_queue ()
          | `Other _ ->
            ()
        )
        strings;
          feed_lines ()
        | _ when List.mem ts.ts_signal_fd rd ->
          let n = Unix.read ts.ts_signal_fd buf 0 1 in
          if n = 0 then
            `Aiee
          else
            let task = Protect.access ts.ts_task_queue Queue.take in
            task rt;
            flush_queue ();
            feed_lines ()
        | _ ->
          feed_lines ()
        )
  in
  ignore (Unix.write ts.ts_fd "%~" 0 2);
  let `Aiee = feed_lines () in
  Printf.printf "Receiver finished\n%!";
  ts.ts_exited ()

type t = {
  fd	     : Unix.file_descr;
  signal_fd  : Unix.file_descr option ref Protect.t;
  task_queue : (rt -> unit) Queue.t Protect.t;
}

let request t fn =
  Protect.access t.signal_fd @@ function
  | { contents = None } ->
    fn (`Aborted)
  | { contents = Some fd } ->
    Protect.access t.task_queue (Queue.add (fun rt -> fn (`Ok rt)));
    ignore (Unix.write fd "1" 0 1)

let send_gcode t gcode =
  let activation = new Future.t in
  request t (function
  | `Ok rt -> rt.rt_send_gcode (gcode, activate activation)
  | `Aborted -> activation#set `Aborted
  );
  activation

let strip_comments str = Pcre.replace ~pat:"\\(.*\\)*" str

let strip_whitespace str = Pcre.replace ~pat:"[\t ]" str

let get_gcode lines =
  lines |> Enum.map (strip_comments %> strip_whitespace) |> Enum.filter ((<>) "") |> List.of_enum

let string_of_tm { Unix.tm_sec = sec;
                   tm_min = min;
                   tm_hour = hour;
                   tm_mday = mday;
                   tm_mon = mon;
                   tm_year = year } =
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d"
    (year + 1900)
    (mon + 1)
    (mday)
    (hour)
    (min)
    (sec)

let string_of_time t =
  string_of_tm (Unix.localtime t)

let human_eta seconds =
  let parts = [(60, "s"); (60, "m"); (60, "h"); (24, "d")] in
  let left, segs =
    List.fold_left
      (fun (left, segs) (unit_size, unit_name) ->
	if left > 0
	then
	  (left / unit_size, (Printf.sprintf "%d%s" (left mod unit_size) unit_name :: segs))
	else (left, segs)
      )
      (seconds, [])
      parts
  in
  if left > 0
  then "n/a"
  else String.concat " " segs
      

let upload sigint_triggered common_options file start_from_line =
  Serial.with_serial (common_options.co_device, common_options.co_bps) @@ fun fd ->
    let input_gcode = get_gcode (Enum.skip (start_from_line - 1) (File.lines_of file))  in
    let input_nlines = List.length input_gcode in
    let signal_fds = Unix.pipe () in
    let task_queue = Protect.create (Mutex.create ()) (Queue.create ()) in
    let exited = new Future.t in
    let thread = Thread.create receiver {
      ts_exited	  = exited#set;
      ts_sigint_triggered = sigint_triggered;
      ts_common_options   = common_options;
      ts_fd		  = fd;
      ts_signal_fd	  = fst signal_fds;
      ts_task_queue	  = task_queue;
    } in
    let t = { fd; signal_fd = Protect.create (Mutex.create ()) (ref (Some (snd signal_fds))); task_queue } in
    let ready = new Future.t in
    let t0 = Unix.gettimeofday () in
    let update_status n =
      let open ANSITerminal in
      let progress = float n /. float input_nlines in
      move_bol ();
      let time_left = (Unix.gettimeofday () -. t0) /. progress in
      let time_finished = t0 +. time_left in
      printf [Bold; green; on_default] "%2.1f%%" (100.0 *. progress);
      printf [default] " complete, %d/%d, ETA " n input_nlines;
      printf [Bold; green; on_default] "%s (%s)" (human_eta (int_of_float time_left)) (string_of_time time_finished);
      erase Eol;
    in
    let last_line_sent = ref None in
    let rec feed_lines input linenumber =
      match input with
      | [] -> 
	last_line_sent := None;
	Printf.printf "Done!\n%!";
	ready#set ();
      | command::rest ->
	(send_gcode t command)#add_callback @@ function
	| `Ok r -> 
	  last_line_sent := Some linenumber;
	  update_status linenumber;
	  feed_lines rest (linenumber + 1)
	| `Aborted -> 
	  Printf.printf "Meh, error sending request!\n%!";
	  ready#set ()
    in
    feed_lines input_gcode start_from_line;
    Future.wait [ready; exited];
    ( match !last_line_sent with
    | None -> ()
    | Some line -> Printf.printf "\nLast line sent: %d\n" line );
    Protect.access t.signal_fd (
      function
      | { contents = None } -> assert false;
      | { contents = Some fd } as sfd ->
	Unix.close fd;
	sfd := None
    );
    Thread.join thread;
    ()
