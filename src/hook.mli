type 'a t

type hook_id

val create : unit -> 'a t

val clone : 'a t -> 'a t

val issue : 'a t -> 'a -> unit

val hook : 'a t -> ('a -> unit) -> hook_id

val unhook : hook_id -> unit
