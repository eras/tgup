open Batteries
open Common

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
  let a = get_assoc (Yojson.Safe.from_string str) in
  try `Result (get_assoc (List.assoc "r" a))
  with Not_found -> `Status a

type result = (string * Yojson.Safe.json) list

type linenumber = int

type rt = {
  rt_line_callbacks : (linenumber, result -> unit) Hashtbl.t;
}

let receiver (fd, signal_fd, task_queue) =
  let rt = { rt_line_callbacks = Hashtbl.create 1024 } in
  let buf = String.create 1024 in
  let lb = LineBuffer.create () in
  let rec feed_lines () =
    let (rd, _, _) = Unix.select [fd; signal_fd] [] [] (-1.0) in
    match () with
    | _ when List.mem fd rd ->
      let n = Unix.read fd buf 0 (String.length buf) in
      if n = 0 then
	raise End_of_file;
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
	    ()
	)
	strings;
      feed_lines ()
    | _ when List.mem signal_fd rd ->
      let n = Unix.read signal_fd buf 0 1 in
      if n = 0 then
	() (* exit *)
      else 
	let task = Protect.access task_queue Queue.take in
	task rt;
	feed_lines ()
    | _ -> ()
  in
  feed_lines ();
  Printf.printf "Receiver finished\n%!"

let add_line_callback rt n fn =
  match n with
  | None -> failwith "fill in code"
  | Some n -> Hashtbl.add rt.rt_line_callbacks n fn

type t = {
  fd	     : Unix.file_descr;
  signal_fd  : Unix.file_descr option ref Protect.t;
  lines_sent : int ref;
  task_queue : (rt -> unit) Queue.t Protect.t;
}

let request t fn =
  Protect.access t.signal_fd @@ function
  | { contents = None } ->
    fn (`Aborted)
  | { contents = Some fd } ->
    Protect.access t.task_queue (Queue.add (fun rt -> fn (`Ok rt)));
    ignore (Unix.write fd "1" 0 1)

let activate future x = 
  future#set (`Ok x)

let send_gcode t gcode =
  let activation = new Future.t in
  request t (function
  | `Ok rt ->
    incr t.lines_sent;
    let linenumber = !(t.lines_sent) in
    let msg = Printf.sprintf "{\"gc\":\"%s N%d\"}\r\n" gcode linenumber in
    Printf.printf "-> %s%!" msg;
    add_line_callback rt (Some linenumber) (activate activation);
    let _n = Unix.write t.fd msg 0 (String.length msg) in
    ()
  | `Aborted ->
    activation#set `Aborted
  );
  activation

let strip_comments str = Pcre.replace ~pat:"\\(.*\\)*" str

let strip_whitespace str = Pcre.replace ~pat:"[\t ]" str

let get_gcode lines =
  lines |> Enum.map (strip_comments %> strip_whitespace) |> Enum.filter ((<>) "") |> List.of_enum

let upload common_options file =
  with_serial (common_options.co_device, common_options.co_bps) @@ fun fd ->
    let input_gcode = get_gcode (File.lines_of file)  in
    let signal_fds = Unix.pipe () in
    let task_queue = Protect.create (Mutex.create ()) (Queue.create ()) in
    let thread = Thread.create receiver (fd, fst signal_fds, task_queue) in
    let t = { fd; signal_fd = Protect.create (Mutex.create ()) (ref (Some (snd signal_fds))); lines_sent = ref 0; task_queue } in
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
