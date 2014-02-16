open Batteries
open Common

type handled_token = int

(* First a unit -> receive_handler function is used to get a function
   that handles each input.

   The handling function returns both a continuation for handling following
   lines and another function, that is used to handle the "ok" *)
type receive_handler = Cont of (string -> (receive_handler * receive_finish))
and receive_finish = unit -> unit

type receiver_state = {
  mutable received_ack		: int;
  mutable line_callbacks	: (unit -> (receive_handler * receive_finish)) BatDeque.t;
}

let register_of_char = function
  | 'X' -> `X
  | 'Y' -> `Y
  | 'Z' -> `Z
  | 'E' -> `E
  | 'F' -> `F
  | x -> failwith ("register_of_char: unknown register " ^ String.make 1 x)

type t = { 
  fd		: Unix.file_descr;
  mutable line	: int;
  receiver	: receiver_state Protect.t;
}

type 'a request = unit -> ((t -> unit) * (('a -> unit) -> unit -> (receive_handler * receive_finish)))

let rec ignore_loop : receive_handler =
  Cont (fun _str -> (ignore_loop, fun () -> Printf.printf "Ignoring finish after loop\n%!"))

let reader (fd, state) =
  let buf = String.create 1024 in
  let lb = LineBuffer.create () in
  let rec get_next_handler () : (receive_handler * receive_finish) option =
    let handler =
      Protect.access state (
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
  let rec loop (handler : (receive_handler * receive_finish) option) =
    let n = Unix.read fd buf 0 (String.length buf) in
      if n > 0 
      then (
	let strs = LineBuffer.append_substring lb buf 0 n in
	let process_json handler str =
	  let open Json in
	  let json =
	    try Some (from_string str)
	    with _ -> None
	  in
	    match json +> "r" with
	    | None ->
	      (* don't handle these at the moment *) 
	      Printf.fprintf stderr "Cnc.reader.loop.process_json: cannot parse %s\n%!" str;
	      handler
	    | Some r ->
	      let handler =
		match handler with
		| None -> get_next_handler ()
		| Some handler -> Some handler
	      in
	      ( match handler with
	      | None -> None
	      | Some (Cont f, _finish) -> 
		let (_handler, finish) = f (to_string r) in
		Protect.access state (
		  fun st ->
		    st.received_ack <- st.received_ack + 1;
		);
		finish ();
		None
	      )
	  in
	let handler =
	  List.fold_left
	    (fun (handler : (receive_handler * receive_finish) option) str ->
	       Printf.printf "CNC<-%s\n%!" str;
	       let handler : (receive_handler * receive_finish) option =
		 match str with
		 | str when Pcre.pmatch ~pat:"{" str ->
		   process_json handler str
		 | str when Pcre.pmatch ~pat:"ok" str ->
		   (match handler with
		   | None -> BatOption.may (fun (_, finish) -> finish ()) (get_next_handler ())
		   | Some (_, finish) -> finish ());
		   Protect.access state (
		     fun st ->
		       st.received_ack <- st.received_ack + 1;
		   );
		   None
		 | "start" ->
		   Protect.access state (
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
	  loop handler
      ) else ()
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
  let receiver =
    Protect.create (Mutex.create ())
      { received_ack    = 1;
	line_callbacks	= BatDeque.empty }
  in
  let _ = Thread.create reader (fd, receiver) in
    { fd; line = 1; 
      receiver; }

let name_of_axis = function
  | `X -> "X"
  | `Y -> "Y"
  | `Z -> "Z"

let send_str mk_msg (handle_response : ('a -> unit) -> unit -> receive_handler * receive_finish) : 'a request =
  fun () ->
    (( fun t ->
      let msg = mk_msg t.line in
      let _ = Printf.printf "->CNC: %s\n%!" msg in
      let msg = msg ^ "\n" in
      ignore (Unix.write t.fd msg 0 (String.length msg));
      t.line <- t.line + 1 ),
     handle_response
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

let unit_response (respond : unit -> unit) =
  let rec loop _str = (Cont loop, respond) in
    fun () -> (Cont loop, respond)

let foldl_response f v0 (respond : 'a -> unit) =
  let rec loop v str = (Cont (loop (f v str)), fun () -> respond v) in
    fun () -> (Cont (loop v0), fun () -> respond v0)

(* actually this just retrieves the last string *)
let single_string_response (respond : string -> unit) =
  let rec loop str = 
    (Cont loop, fun () -> respond str) in
    fun () -> (Cont loop, fun () -> respond "")

let json_response (respond : Json.json option -> unit) =
  let rec loop str = 
    (Cont loop, fun () -> respond (Some (Json.from_string str))) in
    fun () -> (Cont loop, fun () -> respond None)

let home axis =
  send_gcode ("G28 " ^ String.concat " " (List.map (fun axis -> name_of_axis axis ^ "0") axis)) unit_response

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

let travel axis = send_gcode ("G0 " ^ string_of_axis axis) unit_response

let feed axis = send_gcode ("G1 " ^ string_of_axis axis) unit_response
      
let set_position axis = send_gcode ("G92 " ^ string_of_axis axis) unit_response

let set_absolute = send_gcode ("G90") unit_response

let set_feed_rate rate = send_gcode ("F" ^ string_of_float rate) unit_response

let set_relative = send_gcode ("G91") unit_response

let set_acceleration axis = send_gcode ("M201 " ^ string_of_axis axis) unit_response

let wrap_response input output =
  fun respond ->
    input (fun msg -> respond (output msg))

type tinyg_where = {
  x    : float;
  y    : float;
  z    : float;
  feed : float;
  vel  : float;
  coor : int;
  dist : int;
  momo : int;
}

let where_tinyg =
  let process json =
    let open Json in
    let sr   = json +> "sr" in
    let x    = sr +> "posx" in
    let y    = sr +> "posy" in
    let z    = sr +> "posz" in
    let feed = sr +> "feed" in
    let vel  = sr +> "vel" in
    let coor = sr +> "coor" in
    let dist = sr +> "dist" in
    let momo = sr +> "momo" in
    let f = function
      | None -> assert false
      | Some x -> get_float x
    in
    let i = function
      | None -> assert false
      | Some x -> get_int x
    in
    if List.mem None [x; y; z; feed; vel; coor; dist; momo]
    then failwith ("Failed to retrieve TinYG location from " ^ to_string (Option.get json))
    else { x = f x; y = f y; z = f z; feed = f feed; vel = f vel; coor = i coor; dist = i dist; momo = i momo }
  in
  send_json (`Assoc [("sr", `String "")]) (wrap_response json_response process)

let where =
  let process str =
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
      (List.assoc 'X' regs,
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

let async t (request : 'a request) (callback : 'a -> unit) =
  let (issue, handler) = request () in
  let handler = handler callback in
    Protect.access t.receiver (
      fun st ->
	st.line_callbacks <- BatDeque.snoc st.line_callbacks handler;
    );
    issue t

let wait : 'a. t -> 'a request -> 'a = fun t request ->
  let sync = Event.new_channel () in
  let respond response = Event.sync (Event.send sync response) in
    async t request respond;
    Event.sync (Event.receive sync)

let ignore t request = async t request ignore
