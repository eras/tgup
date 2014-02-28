open Batteries
open Common

type handled_token = int

type 'a result =
| ResultOK of 'a
| ResultDequeued

type request_finish_state = unit result

(* First a unit -> receive_handler function is used to get a function
   that handles each input.

   The handling function returns both a continuation for handling following
   lines and another function, that is used to handle the "ok" *)
type receive_handler = Cont of (string -> (receive_handler * receive_finish))
and receive_finish = request_finish_state -> unit

let register_of_char = function
  | 'X' -> `X
  | 'Y' -> `Y
  | 'Z' -> `Z
  | 'E' -> `E
  | 'F' -> `F
  | x -> failwith ("register_of_char: unknown register " ^ String.make 1 x)

type status_tinyg = {
  x    : float;
  y    : float;
  z    : float;
  feed : float;
  vel  : float;
  coor : int;
  dist : int;
  momo : int;
  stat : int;
  line : int;
}

let xon = Char.chr 17
let xoff = Char.chr 19

let queue_threshold = 20 (* once there are less than n slots available, don't send new commands *)

type io_thread_state = {
  mutable received_ack	      : int;
  mutable line_callbacks      : (unit -> (receive_handler * receive_finish)) BatDeque.t;
  status_report_tinyg	      : status_tinyg Hook.t;
}

type write_message =
| WriteChar of char
| WriteNotify of (request_finish_state -> unit)

type internal_state = {
  get_next_handler	      : (unit -> (receive_handler * receive_finish) option);
  status_tinyg		      : status_tinyg option ref;
  io_thread_state	      : io_thread_state Protect.t;
  write_queue		      : write_message  Queue.t;
  mutable next_write_time     : float;
  mutable xon                 : bool;
  mutable queue_full          : bool;
}

type ext_requests = (internal_state -> unit) Queue.t Protect.t

type t = { 
  fd			      : Unix.file_descr;
  mutable line		      : int;
  receiver		      : io_thread_state Protect.t;
  status_report_tinyg	      : status_tinyg Hook.t;

  control_write               : Unix.file_descr; (* used for passing requests to the IO thread *)
  ext_requests                : ext_requests;
}

type handler_bottomend = (receive_handler * receive_finish)

type 'a handler_kind =
(* handlerLine is given a function to call once a response is formed;
   for that it returns the function that handles input *)
| HandlerLine of (('a result -> unit) -> (unit -> handler_bottomend))
| HandlerFuture of 'a result Future.t

type 'a request = unit -> ((t -> unit) * ('a handler_kind))

let rec ignore_loop : receive_handler =
  Cont (fun _str -> (ignore_loop, fun _ -> Printf.printf "Ignoring finish after loop\n%!"))

module ParseStatusTinyG = struct
  let get sr =
    let open Json in
    let x    = sr +> "posx" in
    let y    = sr +> "posy" in
    let z    = sr +> "posz" in
    let feed = sr +> "feed" in
    let vel  = sr +> "vel" in
    let coor = sr +> "coor" in
    let dist = sr +> "dist" in
    let momo = sr +> "momo" in
    let stat = sr +> "stat" in
    let line = sr +> "line" in
    (x, y, z, feed, vel, coor, dist, momo, stat, line)
end

let parse_status_tinyg sr =
  let open Json in
  let open ParseStatusTinyG in
  let (x, y, z, feed, vel, coor, dist, momo, stat, line) = get sr in
  let f = function
    | None -> assert false
    | Some x -> get_float x
  in
  let i = function
    | None -> assert false
    | Some x -> get_int x
  in
  if List.mem None [x; y; z; feed; vel; coor; dist; momo; stat; line]
  then failwith ("Failed to retrieve TinYG location from " ^ to_string (Option.get sr))
  else { x = f x; y = f y; z = f z; feed = f feed; vel = f vel; coor = i coor; dist = i dist; momo = i momo; stat = i stat; line = i line; }

let updated_status_tinyg status sr =
  let open Json in
  let open ParseStatusTinyG in
  let (x, y, z, feed, vel, coor, dist, momo, stat, line) = get sr in
  let f default = function
    | None -> default
    | Some x -> get_float x
  in
  let i default = function
    | None -> default
    | Some x -> get_int x
  in
  { x	 = f status.x x;
    y	 = f status.y y;
    z	 = f status.z z;
    feed = f status.feed feed;
    vel	 = f status.vel vel;
    coor = i status.coor coor;
    dist = i status.dist dist;
    momo = i status.momo momo;
    stat = i status.stat stat;
    line = i status.line line; }

let process_sr internal_state sr =
  match !(internal_state.status_tinyg) with
  | None -> ()
  | Some status -> 
    let updated_status = updated_status_tinyg status sr in
    internal_state.status_tinyg := Some updated_status;
    Hook.issue (Protect.access internal_state.io_thread_state (fun state -> state.status_report_tinyg)) updated_status

let process_qr internal_state qr =
  let open Json in
  internal_state.queue_full <- Option.default false (Option.map (fun x -> get_int x < queue_threshold) qr)
    
let process_initial_sr internal_state sr =
  let status = parse_status_tinyg sr in
  internal_state.status_tinyg := Some status;
  Hook.issue (Protect.access internal_state.io_thread_state (fun state -> state.status_report_tinyg)) status  

let process_r internal_state handler r =
  let handler =
    match handler with
    | None -> internal_state.get_next_handler ()
    | Some handler -> Some handler
  in
  ( match handler with
  | None -> None
  | Some (Cont f, _finish) -> 
    let open Json in
    let (_handler, finish) = f (to_string r) in
    Protect.access internal_state.io_thread_state (
      fun st ->
	st.received_ack <- st.received_ack + 1;
    );
    finish (ResultOK ());
    None
  )

let process_json internal_state handler str =
  let open Json in
  let json =
    try Some (from_string str)
    with _ -> None
  in
  let response_kind = 
    match json +> "r", json +> "sr", json +> "qr" with
    | Some r, _, _  -> `R r
    | _, Some sr, _ -> `SR sr
    | _, _, Some qr -> `QR qr
    | _		    -> `None
  in
  match response_kind with
  | `None ->
    (* don't handle these at the moment *) 
    Printf.fprintf stderr "Cnc.reader.loop.process_json: cannot parse %s\n%!" str;
    handler
  | `SR sr ->
    process_sr internal_state (Some sr);
    handler
  | `QR qr ->
    process_qr internal_state (Some qr);
    handler
  | `R r ->
    let sr = lazy (Some r +> "sr") in
    if !(internal_state.status_tinyg) = None && Lazy.force sr <> None then
      process_initial_sr internal_state (Lazy.force sr);
    process_r internal_state handler r

let rec handle_rds io_thread_state (get_next_handler : unit -> (receive_handler * receive_finish) option) lb buf internal_state cnc_fd handler cont =
  let n = Unix.read cnc_fd buf 0 (String.length buf) in
  if n > 0 
  then (
    for c = 0 to String.length buf - 1 do
      match buf.[c] with
      | ch when ch = xon -> internal_state.xon <- true;
      | ch when ch = xoff -> internal_state.xon <- false;
      | _ -> ()
    done;
    let strs = LineBuffer.append_substring lb buf 0 n in
    let handler =
      List.fold_left
	(fun (handler : (receive_handler * receive_finish) option) str ->
	  Printf.printf "CNC<-%s\n%!" str;
	  let handler : (receive_handler * receive_finish) option =
	    match str with
	    | str when Pcre.pmatch ~pat:"{" str ->
	      process_json internal_state handler str
	    | str when Pcre.pmatch ~pat:"ok" str ->
	      (match handler with
	      | None -> BatOption.may (fun (_, finish) -> finish (ResultOK ())) (get_next_handler ())
	      | Some (_, finish) -> finish (ResultOK ()));
	      Protect.access io_thread_state (
		fun st ->
		  st.received_ack <- st.received_ack + 1;
	      );
	      None
	    | "start" ->
	      Protect.access io_thread_state (
		fun st -> 
		  st.received_ack <- 1;
		       (* TODO: call some error handler for remaining messages *)
		  st.line_callbacks <- BatDeque.empty;
	      );
	      None
	    | str ->
	      let handler =
		match handler with
		| None -> get_next_handler ()
		| Some handler -> Some handler
	      in
	      match handler with
	      | None -> None
	      | Some (Cont f, _finish) -> Some (f str)
	  in
	  handler
	)
	handler
	strs
    in
    cont handler
  ) else ()


let handle_control control_fd ext_requests internal_state (cont : unit -> _) =
  let buf = String.make 1 ' ' in
  ignore (Unix.read control_fd buf 0 1); (* TODO: check for return value *)
  let req = Protect.access ext_requests Queue.take in
  req internal_state;
  cont ()

let handle_write cnc_fd internal_state (cont : unit -> _) =
  match Queue.take internal_state.write_queue with
  | WriteChar ch ->
    let str = String.make 1 ch in
    ignore (Unix.write cnc_fd str 0 1);	(* TODO: check for return value *)
    internal_state.next_write_time <- internal_state.next_write_time +. (1.0 /. (38400.0 /. 8.0));
    cont ()
  | WriteNotify f ->
    f (ResultOK ());
    cont ()

let io_thread (control_fd, cnc_fd, io_thread_state, ext_requests) =
  let buf = String.create 1024 in
  let lb = LineBuffer.create () in
  let rec get_next_handler () : (receive_handler * receive_finish) option =
    let handler =
      Protect.access io_thread_state (
	fun st ->
	  match BatDeque.front st.line_callbacks with
	    | None -> (fun () -> None)
	    | Some ((handler : unit -> (receive_handler * receive_finish)), remaining) ->
		st.line_callbacks <- remaining;
		fun () -> Some (handler ())
      ) ()
    in
      handler
  in
  let internal_state = {
    io_thread_state  = io_thread_state;
    get_next_handler = get_next_handler;
    status_tinyg     = ref None;
    write_queue	     = Queue.create ();
    next_write_time  = 0.0;
    xon              = true;
    queue_full       = false;
  } in
  let rec loop (handler : (receive_handler * receive_finish) option) =
    let (want_rws, timeout) =
      if Queue.is_empty internal_state.write_queue || not internal_state.xon || internal_state.queue_full
      then ([], None)
      else
	let now = Unix.gettimeofday () in
	if now < internal_state.next_write_time
	then ([], Some (internal_state.next_write_time -. now))
	else ([cnc_fd], None)
    in
    let (rds, wrs, exn) = Unix.select [cnc_fd; control_fd] want_rws [] (Option.default ~-.1.0 timeout) in
    match () with
    | _ when List.mem cnc_fd rds -> handle_rds io_thread_state get_next_handler lb buf internal_state cnc_fd handler loop
    | _ when List.mem control_fd rds -> handle_control control_fd ext_requests internal_state (fun () -> loop handler)
    | _ when List.mem cnc_fd wrs -> handle_write cnc_fd internal_state (fun () -> loop handler)
    | _ -> loop handler
  in
    loop None

let connect device bps =
  let fd = Unix.openfile device [Unix.O_RDWR] 0 in
  let tio = Unix.tcgetattr fd in
  let tio = { 
    tio with 
      Unix.c_clocal	= false;
      c_obaud		= bps;
      c_ibaud		= bps;
      c_csize		= 8;
      c_cstopb		= 1;
      c_inlcr           = false;
      c_icrnl		= false;
      c_opost           = false;
      c_isig		= false;
      c_icanon		= false;
      c_echo		= false;
      c_vtime           = 1;
      c_vmin		= 1;
  } in
  let _ = Unix.tcsetattr fd Unix.TCSANOW tio in
  let status_report_tinyg = Hook.create () in
  let ext_requests = Protect.create (Mutex.create ()) (Queue.create ()) in
  let receiver =
    Protect.create (Mutex.create ())
      { received_ack    = 1;
	line_callbacks	= BatDeque.empty;
	status_report_tinyg; }
  in
  let (control_read, control_write) = Unix.pipe () in
  let _ = Thread.create io_thread (control_read, fd, receiver, ext_requests) in
    { fd;
      line	      = 1;
      receiver;
      control_write;
      status_report_tinyg;
      ext_requests; }

let name_of_axis = function
  | `X -> "X"
  | `Y -> "Y"
  | `Z -> "Z"

let external_request (t : t) f =
  Protect.access t.ext_requests @@ fun reqs ->
    Queue.add f reqs;
    ignore (Unix.write t.control_write "X" 0 1)	(* TODO: check for return value *)

let enqueue_flush (rs : internal_state) =
  while not (Queue.is_empty rs.write_queue) do
    match Queue.take rs.write_queue with
    | WriteChar _ -> ()
    | WriteNotify f -> f ResultDequeued
  done

let enqueue_notify (rs : internal_state) f =
  Queue.add (WriteNotify f) rs.write_queue

let enqueue_str (rs : internal_state) str =
  List.iter
    (fun c -> Queue.add (WriteChar c) rs.write_queue)
    (String.explode str);
  let now = Unix.gettimeofday() in
  rs.next_write_time <- max rs.next_write_time now

let unit_of_request_finish_state f = function
  | ResultOK () -> f (ResultOK ())
  | ResultDequeued -> f ResultDequeued

let send_raw_noresponse str future : unit request =
  fun () ->
    ((fun t ->
      external_request t (fun rs ->
        enqueue_str rs str;
        (* TODO: why does this need to use set_if_unset?! *)
        enqueue_notify rs (unit_of_request_finish_state (fun x -> ignore (future#set_if_unset x))))
     ),
     HandlerFuture future
    )

let send_flush future : unit request =
  fun () ->
    ((fun t ->
      external_request t (fun rs -> 
        enqueue_flush rs;
        enqueue_str rs "!%";
        (* TODO: why does this need to use set_if_unset?! *)
        enqueue_notify rs (unit_of_request_finish_state (fun x -> ignore (future#set_if_unset x)));
      )
     ),
     HandlerFuture future
    )

let send_str mk_msg (handle_response : (('a result -> unit) -> (unit -> handler_bottomend))) : 'a request =
  fun () ->
    (( fun t ->
      let msg = mk_msg t.line in
      let _ = Printf.printf "->CNC: %s\n%!" msg in
      let msg = msg ^ "\n" in
      external_request t (fun rs -> enqueue_str rs msg);
      t.line <- t.line + 1 ),
     HandlerLine handle_response
    )

let not1 f x = not (f x)

let send_json msg = send_str @@ fun line ->
  let open Json in
  let msg : json =
    (* annotate "gc", if it exists, with a line number *)
    let has_gc = function ("gc", `String _) -> true | _ -> false in
    match msg with
    | `Assoc xs when List.exists has_gc xs ->
      let (_, gc) = (List.find has_gc xs) in
      let gc = get_string gc in
      let gc = Printf.sprintf "N%d %s" line gc in
      `Assoc (("gc", `String gc)::List.filter (not1 has_gc) xs)
    | _ -> msg
  in
  Json.to_string msg

let send_gcode msg = send_json (`Assoc ["gc", `String msg])

let unit_response (respond : unit result -> unit) =
  let rec loop _str = (Cont loop, function ResultOK () -> respond (ResultOK ()) | ResultDequeued -> respond ResultDequeued) in
  fun () -> (Cont loop, respond)

let foldl_response f v0 (respond : 'a -> unit) =
  let rec loop v str =
    (Cont (loop (f v str)), function ResultOK () -> respond v | ResultDequeued -> respond ResultDequeued)
  in
  fun () -> (Cont (loop v0), fun () -> respond v0)

(* actually this just retrieves the last string *)
let single_string_response (respond : string result -> unit) =
  let rec loop str = 
    (Cont loop, function ResultOK () -> respond (ResultOK str) | ResultDequeued -> respond ResultDequeued) in
  fun () -> (Cont loop,
             function
             | ResultOK () -> respond (ResultOK "")
             | ResultDequeued -> respond ResultDequeued)

let json_response (respond : Json.json option result -> unit) =
  let rec loop str = 
    (Cont loop,
     function
     | ResultOK () -> respond (ResultOK (Some (Json.from_string str)))
     | ResultDequeued -> respond ResultDequeued) in
  fun () -> (Cont loop,
             function
             | ResultOK () -> respond (ResultOK None)
             | ResultDequeued -> respond ResultDequeued)

let home axis =
  send_gcode ("G28 " ^ String.concat " " (List.map (fun axis -> name_of_axis axis ^ "0") axis)) unit_response

let wait_status_tinyg predicate =
  let future = new Future.t in
  let hook_id = ref None in
  let evaluate ret status =
    match predicate status with
    | None -> ret false
    | Some status ->
      (fun x -> ignore (future#set_if_unset x)) (ResultOK status);
      Option.may Hook.unhook !hook_id;
      ret true
  in
  fun () ->
    ((fun t ->
      external_request t (fun internal_state ->
        let setup_hook () =
          hook_id := Some (Hook.hook t.status_report_tinyg (evaluate (const ())))
        in
        match !(internal_state.status_tinyg) with
        | None -> setup_hook ()
        | Some status ->
          if not (evaluate identity status)
          then setup_hook()
      );
     ),
     HandlerFuture future
    )

let set_step_speed speed = send_gcode ("G1 F" ^ string_of_float speed) unit_response

let string_of_axis axis =
  axis 
  |> List.map (function
		 | `X x -> "X", x
		 | `Y y -> "Y", y
		 | `Z z -> "Z", z
	      ) 
  |> List.map (uncurry (Printf.sprintf "%s%.3f"))
  |> String.concat " "

let wrap_response : _ -> _ -> ('a -> unit) -> unit -> handler_bottomend =
  fun input output respond ->
    input (fun msg -> respond (output msg))

let flush_queue : unit request = 
  let future = new Future.t in
  future#add_persistent_callback (function
  | ResultOK () -> ()
  | ResultDequeued -> ()
  );
  send_flush future

let raw_gcode str = send_gcode str unit_response

let travel axis = send_gcode ("G0 " ^ string_of_axis axis) unit_response

let feed axis = send_gcode ("G1 " ^ string_of_axis axis) unit_response
      
let set_position axis = send_gcode ("G92 " ^ string_of_axis axis) unit_response

let set_absolute = send_gcode ("G90") unit_response

let set_feed_rate rate = send_gcode ("F" ^ string_of_float rate) unit_response

let set_relative = send_gcode ("G91") unit_response

let set_acceleration axis = send_gcode ("M201 " ^ string_of_axis axis) unit_response

let status_tinyg =
  let process = function
    | ResultOK r ->
      let open Json in
      ResultOK (parse_status_tinyg (r +> "sr"))
    | ResultDequeued -> ResultDequeued
  in
  send_json (`Assoc [("sr", `String "")]) (wrap_response json_response process)

let where =
  let process = function
    | ResultDequeued -> ResultDequeued
    | ResultOK str ->
    (* X:0.00Y:0.00Z:0.00E:0.00 Count X:0.00Y:0.00Z:0.00 *)
      let ofs = ref 0 in
      let len = String.length str in 
      let get () = 
        if !ofs >= len
        then failwith "invalid response"
        else str.[!ofs]
      in
      let next () = 
        ofs := !ofs + 1;
      in
      let eof () = !ofs >= len in
      let float_chars = BatString.explode "0123456789-." in
      let rec loop collected = function
        | `WaitRegister ->
	  if eof () 
	  then collected
	  else
	    ( match get () with   
	    | ' ' -> next (); loop collected `WaitRegister
	    | 'C' -> collected
	    | ch -> next (); loop collected (`WaitColon ch) )
        | `WaitColon register ->
	  let ch = get () in
	  if ch != ':' 
	  then failwith "invalid response, expected :"
	  else (
	    next ();
	    loop collected (`WaitFloat (register, []))
	  )
        | `WaitFloat (register, digits) ->
	  if eof ()
	  then 
	    let value = float_of_string (BatString.implode (List.rev digits)) in
	    ((register, value)::collected)
	  else
	    let ch = get () in
	    if List.mem ch float_chars
	    then (
	      next ();
	      loop collected (`WaitFloat (register, (ch::digits)))
	    )
	    else 
	      let value = float_of_string (BatString.implode (List.rev digits)) in
	      loop ((register, value)::collected) `WaitRegister
      in
      let regs = List.rev (loop [] `WaitRegister) in
      ResultOK (List.assoc 'X' regs,
                List.assoc 'Y' regs,
                List.assoc 'Z' regs)
  in
  send_gcode "M114" (wrap_response single_string_response process)

let motors_off =
  send_gcode "M84" unit_response

let synchronize =
  send_gcode "M400" unit_response

let set_power state =
  send_gcode (if state then "M80" else "M81") unit_response

let set_port port value =
  send_gcode (Printf.sprintf "M42 P%d S%d" port value) unit_response

let async t (request : 'a request) (callback : 'a result -> unit) =
  let (issue, handler) = request () in
  ( match handler with
  | HandlerLine handler -> 
    let handler = handler callback in
    Protect.access t.receiver (
      fun st ->
        st.line_callbacks <- BatDeque.snoc st.line_callbacks handler;
    );
  | HandlerFuture future -> 
    future#add_persistent_callback callback
  );
  issue t

let wait : 'a. t -> 'a request -> 'a result = fun t request ->
  let sync = Event.new_channel () in
  let respond response = Event.sync (Event.send sync response) in
    async t request respond;
    Event.sync (Event.receive sync)

let ignore t request = async t request ignore

let status_report_tinyg t = 
  ignore t status_tinyg;
  t.status_report_tinyg

