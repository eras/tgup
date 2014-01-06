type 'a t
val create : Mutex.t -> 'a -> 'a t
val access : 'a t -> ('a -> 'b) -> 'b

