open Batteries
open Common

type status_tinyg = {
  x    : float;				(* x *)
  y    : float;				(* y *)
  z    : float;				(* z *)
  feed : float;				(* f *)
  vel  : float;				(* velocity *)
  coor : int;				(* coordinate system, 1 being the default *)
  dist : int;				(* distance mode, 1 meaning absolute and 2 relative *)
  momo : int;				(* machine motion mode, 0 meaning "Stop" *)
  stat : int;				(* status? 3 means program finished (no pending operations). *)
  line : int;				(* line number *)
}

module CharQueue : sig
  type t

  val create : unit -> t
  val time_of_next : t -> float option
  val get : t -> char
  val add : t -> string -> unit
end = struct
  type t = {
    q         : char Queue.t;
    time_next : float;
  }

  let create () = {
    q         = Queue.create ();
    time_next = 0.0;
  }

  let time_of_next t =
    if Queue.is_empty t.q
    then None
    else Some t.time_next

  let get t = Queue.take t.q

  let add t str =
    List.iter
      (fun c -> Queue.add c t.q)
      (String.explode str)
end

type request_if = <
    write_str : string -> unit;
  >

(* state internal to the thread *)
type internal = {
  i_cnc_fd          : Unix.file_descr;
  i_write_queue     : CharQueue.t;
  i_xon             : bool;
  i_queue_full      : bool;
}

type 'a request = internal -> request_if -> ('a * internal)

module RequestEnv = struct
  type env = (internal * request_if)
  type response = internal
end

module Reqs = ExtRequest.Channel(RequestEnv)

type 'a result =
| ResultOK of 'a
| ResultDequeued

(* the external interface *)
type t = {
  t_internal : internal;
  t_reqs     : Reqs.t;
}

let req_send_str str =
  fun internal request_if ->
    request_if#write_str str;
    ((), internal)

let handle_reqs reqs internal loop =
  let interface = object
    method write_str str = CharQueue.add internal.i_write_queue str
  end in
  match Reqs.process reqs (internal, interface) with
  | Ok internal -> loop internal
  | Bad exn ->
    Printf.eprintf "Cnc.handle_reqs: handler threw an exception: %s\n%!" (Printexc.to_string exn);
    raise exn

let thread reqs internal =
  let rec loop internal =
    let (want_rws, timeout) =
      let now = Unix.gettimeofday () in
      match CharQueue.time_of_next internal.i_write_queue with
      | None -> ([], None)
      | Some time when time <= now -> ([internal.i_cnc_fd], None)
      | Some time -> ([], Some (time -. now))
    in
    let (rds, wrs, exn) = Unix.select [internal.i_cnc_fd; Reqs.get_read_fd reqs] want_rws [] (Option.default ~-.1.0 timeout) in
    match () with
    (* | _ when List.mem internal.i_cnc_fd rds -> handle_rds io_thread_state get_next_handler lb buf internal_state internal.i_cnc_fd handler loop *)
    | _ when List.mem (Reqs.get_read_fd reqs) rds -> handle_reqs reqs internal loop
    (* | _ when List.mem internal.i_cnc_fd wrs -> handle_write internal.i_cnc_fd internal_state (fun () -> loop handler) *)
    | _ -> loop internal
  in
  ignore (`Exit = loop internal)

let connect device bps =
  let i_cnc_fd = Unix.openfile device [Unix.O_RDWR] 0 in
  let tio = Unix.tcgetattr i_cnc_fd in
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
  let _ = Unix.tcsetattr i_cnc_fd Unix.TCSANOW tio in
  let t_reqs = Reqs.create () in
  let t_internal = {
    i_cnc_fd;
    i_write_queue     = CharQueue.create ();
    i_xon             = false;
    i_queue_full      = false;
  } in
  let _ = Thread.create (fun () -> thread t_reqs t_internal) () in
  let t = { t_internal; t_reqs } in
  t

(** [async t request callback] asynchronously sends in a request. The callback will be called when a response arrives. *)
let async : t -> 'a request -> ('a result -> unit) -> unit = fun t request callback ->
  Reqs.async t.t_reqs (
    fun (env, request_if) ->
      let (value, env) = request env request_if in
      callback (ResultOK value);
      ((), Ok env)
  )

(** [wait t request] synchronously sends in a request *)
let wait : t -> 'a request -> 'a result = fun t request ->
  let response_chan = Event.new_channel () in
  async t request (
    fun response -> Event.sync (Event.send response_chan response)
  );
  Event.sync (Event.receive response_chan)

(** [ignore t request] is the same as [async r request], but there is
    no callback function (nor a value can be retrieved) *)
let ignore : t -> 'a request -> unit = fun t request -> async t request ignore

(** Send a raw request (without newline); a line number N will be added *)
let raw_gcode : string -> unit request = fun gcode ->
  req_send_str (gcode ^ "\n")

(** [name_of_axis] returns the name (ie. "X") of an axis *)
let name_of_axis : [< `X | `Y | `Z ] -> string = fun _ -> failwith "not implemented"

(** [set_relative] makes the device to go into the absolute coordinate
    mode. Note! feed rate must be set before callling this! *)
let set_absolute : unit request = fun _ -> failwith "not implemented"

(** [set_relative] makes the device to go into the relative coordinate
    mode. Note! feed rate must be set before callling this! *)
let set_relative : unit request = fun _ -> failwith "not implemented"

(** [set_position position] sets the device position (no actual moving is done) *)
let set_position : [< `X of float | `Y of float | `Z of float ] list -> unit request = fun _ -> failwith "not implemented"

(** Marlin: [home axis] performs the homing sequence *)
let home : [< `X | `Y | `Z ] list -> unit request = fun _ -> failwith "not implemented"

(** [feed axis_moves] issues a G1 for traveling while feeding to a
    certain location (or by certain amount if in relative mode *)
let feed : [< `X of float | `Y of float | `Z of float ] list -> unit request = fun _ -> failwith "not implemented"

(** [travel axis_moves] issues a G0 for traveling to a certain
    location (or by certain amount if in relative mode *)
let travel : [< `X of float | `Y of float | `Z of float ] list -> unit request = fun _ -> failwith "not implemented"

(** Marlin: [set_acceleration axis_accelerations] sets the acceleration values for listed axis *)
let set_acceleration : [< `X of float | `Y of float | `Z of float ] list -> unit request = fun _ -> failwith "not implemented"

(** [set_step_speed] and [set_feed_rate] sets the F value for feeding. Remove one of these.. 
    
    set_step_speed uses G1 to do it.
*)
let set_step_speed : float -> unit request = fun _ -> failwith "not implemented"
let set_feed_rate : float -> unit request = fun _ -> failwith "not implemented"

(** Allows one to be informed when status is updated on TinyG. Note:
    this call is currently synchronous (for determining the initial
    position). *)
let status_report_tinyg : t -> status_tinyg Hook.t = fun _ -> failwith "not implemented"

(** Marlin: [where] requests the position *)
let where : (float * float * float) request = fun _ -> failwith "not implemented"

(** Waits until the evaluation function returns a value. *)
let wait_status_tinyg : (status_tinyg -> 'a option) -> 'a request = fun _ -> failwith "not implemented"

(** TinyG: [status_tinyg] requests the status information from TinyG (along with the position) *)
let status_tinyg : status_tinyg request = fun _ -> failwith "not implemented"

(** Marlin: [motors_off] sends M84 to turn off the power to the motors immediately *)
let motors_off : unit request = fun _ -> failwith "not implemented"

(** Marlin: [synchronize] sends the command M400 *)
let synchronize : unit request = fun _ -> failwith "not implemented"

(** Marlin: [set_port powerstate] turns on or off the power *)
let set_power : bool -> unit request = fun _ -> failwith "not implemented"

(** Marlin: [set_port portnumber value] sets a port to a certain value *)
let set_port : int -> int -> unit request = fun _ -> failwith "not implemented"

(** Starts feed hold mode ("!") and flushes send queue *)
let feed_hold : unit request = fun _ -> failwith "not implemented"

(** Resumes TinyG *)
let feed_resume : unit request = fun _ -> failwith "not implemented"
