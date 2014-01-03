type 'a t
val make : Mutex.t -> 'a -> 'a t
val access : 'a t -> ('a -> 'b) -> 'b

