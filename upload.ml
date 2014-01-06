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

let xon = Char.chr 17
let xoff = Char.chr 19

let activate future x = 
  future#set (`Ok x)

let receiver (fd, signal_fd, task_queue) =
  let slots_available = ref 1 in
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
    if !slots_available > 0 && not (Queue.is_empty command_queue) then
      let gcode, callback = Queue.take command_queue in
      incr lines_sent;
      let linenumber = !lines_sent in
      let msg = Printf.sprintf "{\"gc\":\"%s N%d\"}\r\n" gcode linenumber in
      Printf.printf "-> %s%!" msg;
      add_line_callback rt (Some linenumber) callback;
      List.iter
	(fun c -> Queue.add c write_queue)
	(String.explode msg);
      (* decr slots_available; -- disabled for now *)
      flush_queue ()
  in
  let rec feed_lines () =
    let do_send = not (Queue.is_empty write_queue) && !enable_send in
    let timeout = 
      if do_send
      then 1.0 /. (115200.0 /. 8.0)
      else (-1.0)
    in
    let (rd, _, _) = Unix.select [fd; signal_fd] [] [] timeout in
    if do_send then
      let c = Queue.take write_queue in
      buf.[0] <- c;
      Printf.printf "->%c\n%!" c;
      let n = Unix.write fd buf 0 1 in
      if n <= 0 then
	raise End_of_file
      else ();
    else ();
    match () with
    | _ when List.mem fd rd ->
      let n = Unix.read fd buf 0 (String.length buf) in
      if n = 0 then
	raise End_of_file;
      for c = 0 to n - 1 do
	match buf.[c] with
	| ch when ch = xon ->
	  Printf.printf "*** XON ***\n%!";
	  enable_send := true
	| ch when ch = xoff ->
	  Printf.printf "*** XOFF ***\n%!";
	  enable_send := false
	| _ -> ()
      done;
      let strings = LineBuffer.append_substring lb buf 0 n in
      List.iter
	(fun str -> 
	  Printf.printf "<-%s\n%!" str;
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
	    | Some current_linenumber ->
	      let delta = current_linenumber - !last_linenumber in
	      slots_available := !slots_available + delta;
	      Printf.printf "%d new slots, %d in total\n%!" delta !slots_available;
	      flush_queue ()
	    )
	  | `Other _ ->
	    ()
	)
	strings;
      feed_lines ()
    | _ when List.mem signal_fd rd ->
      let n = Unix.read signal_fd buf 0 1 in
      if n = 0 then
	`Aiee
      else 
	let task = Protect.access task_queue Queue.take in
	task rt;
	flush_queue ();
	feed_lines ()
    | _ ->
      feed_lines ()
  in
  let `Aiee = feed_lines () in
  Printf.printf "Receiver finished\n%!"

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

let upload common_options file =
  Serial.with_serial (common_options.co_device, common_options.co_bps) @@ fun fd ->
    let input_gcode = get_gcode (File.lines_of file)  in
    let signal_fds = Unix.pipe () in
    let task_queue = Protect.create (Mutex.create ()) (Queue.create ()) in
    let thread = Thread.create receiver (fd, fst signal_fds, task_queue) in
    let t = { fd; signal_fd = Protect.create (Mutex.create ()) (ref (Some (snd signal_fds))); task_queue } in
    let ready = new Future.t in
    let rec feed_lines input =
      match input with
      | [] -> 
	Printf.printf "Done!\n%!";
	ready#set ();
      | command::rest ->
	(send_gcode t command)#add_callback @@ function
	| `Ok r -> 
	  feed_lines rest
	| `Aborted -> 
	  Printf.printf "Meh, error sending request!\n%!";
	  ready#set ()
    in
    feed_lines input_gcode;
    ready#wait ();
    Unix.sleep 5;
    Protect.access t.signal_fd (
      function
      | { contents = None } -> assert false;
      | { contents = Some fd } as sfd ->
	Unix.close fd;
	sfd := None
    );
    Thread.join thread;
    ()
