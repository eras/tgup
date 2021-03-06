type 'a request

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

type t

type 'a result =
| ResultOK of 'a
| ResultDequeued

(** [connect "/dev/ttyUSB0" 115200] connects to a CNC device *)
val connect : string -> int -> t

(** [name_of_axis] returns the name (ie. "X") of an axis *)
val name_of_axis : [< `X | `Y | `Z ] -> string

(** [set_relative] makes the device to go into the absolute coordinate
    mode. Note! feed rate must be set before callling this! *)
val set_absolute : unit request

(** [set_relative] makes the device to go into the relative coordinate
    mode. Note! feed rate must be set before callling this! *)
val set_relative : unit request

(** [set_position position] sets the device position (no actual moving is done) *)
val set_position : [< `X of float | `Y of float | `Z of float ] list -> unit request

(** Marlin: [home axis] performs the homing sequence *)
val home : [< `X | `Y | `Z ] list -> unit request

(** [feed axis_moves] issues a G1 for traveling while feeding to a
    certain location (or by certain amount if in relative mode *)
val feed : [< `X of float | `Y of float | `Z of float ] list -> unit request

(** [travel axis_moves] issues a G0 for traveling to a certain
    location (or by certain amount if in relative mode *)
val travel : [< `X of float | `Y of float | `Z of float ] list -> unit request

(** Marlin: [set_acceleration axis_accelerations] sets the acceleration values for listed axis *)
val set_acceleration : [< `X of float | `Y of float | `Z of float ] list -> unit request

(** [set_step_speed] and [set_feed_rate] sets the F value for feeding. Remove one of these.. 
    
    set_step_speed uses G1 to do it.
*)
val set_step_speed : float -> unit request
val set_feed_rate : float -> unit request

(** Allows one to be informed when status is updated on TinyG. Note:
    this call is currently synchronous (for determining the initial
    position). *)
val status_report_tinyg : t -> status_tinyg Hook.t

(** Marlin: [where] requests the position *)
val where : (float * float * float) request

(** Waits until the evaluation function returns a value. *)
val wait_status_tinyg : (status_tinyg -> 'a option) -> 'a request

(** TinyG: [status_tinyg] requests the status information from TinyG (along with the position) *)
val status_tinyg : status_tinyg request

(** Marlin: [motors_off] sends M84 to turn off the power to the motors immediately *)
val motors_off : unit request

(** Marlin: [synchronize] sends the command M400 *)
val synchronize : unit request

(** Marlin: [set_port powerstate] turns on or off the power *)
val set_power : bool -> unit request

(** Marlin: [set_port portnumber value] sets a port to a certain value *)
val set_port : int -> int -> unit request

(** Send a raw request (without newline); a line number N will be added *)
val raw_gcode : string -> unit request

(** Starts feed hold mode ("!") and flushes send queue *)
val feed_hold : unit request

(** Resumes TinyG *)
val feed_resume : unit request

(** [wait t request] synchronously sends in a request *)
val wait : t -> 'a request -> 'a result

(** [async t request callback] asynchronously sends in a request. The callback will be called when a response arrives. *)
val async : t -> 'a request -> ('a result -> unit) -> unit

(** [ignore t request] is the same as [async r request], but there is
    no callback function (nor a value can be retrieved) *)
val ignore : t -> 'a request -> unit
