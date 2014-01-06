type 'a t
val create : ?cond:Condition.t -> Mutex.t -> 'a -> 'a t
val access : 'a t -> ('a -> 'b) -> 'b

(* [wait t condition f] waits condition to be true and then acts like 'access' *)
val wait_access : 'a t -> ('a -> bool) -> ('a -> 'b) ->'b
