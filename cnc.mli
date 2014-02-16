type 'a request

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

type t
val connect : string -> int -> t
val name_of_axis : [< `X | `Y | `Z ] -> string
val set_absolute : unit request
val set_relative : unit request
val set_position : [< `X of float | `Y of float | `Z of float ] list -> unit request
val home : [< `X | `Y | `Z ] list -> unit request
val feed : [< `X of float | `Y of float | `Z of float ] list -> unit request
val travel : [< `X of float | `Y of float | `Z of float ] list -> unit request
val set_feed_rate : float -> unit request
val set_acceleration : [< `X of float | `Y of float | `Z of float ] list -> unit request
val set_step_speed : float -> unit request
val where : (float * float * float) request
val where_tinyg : tinyg_where request
val motors_off : unit request
val synchronize : unit request

val set_power : bool -> unit request
val set_port : int -> int -> unit request

val wait : t -> 'a request -> 'a
val ignore : t -> 'a request -> unit
val async : t -> 'a request -> ('a -> unit) -> unit
