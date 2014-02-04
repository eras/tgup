type never
type dependency = unit -> never (* used to keep a reference around, the function isn't actually called *)

class ['a] t :
  object
    method add_callback : ('a -> unit) -> unit
    method get		: 'a option
    method set		: 'a -> unit
    method set_if_unset	: 'a -> bool	(* returns true if value was set *)
    method wait		: unit -> 'a
    method add_dependency : dependency -> unit
  end

type ('a, 'b) future_cb = (< add_callback : ('a -> unit) -> unit; .. > as 'b)

val map : ('a -> 'b) -> ('a, _) future_cb -> 'b t

val wait : ('a, _) future_cb list -> 'a
