class ['a] t :
  object
    method add_callback : ('a -> unit) -> unit
    method get		: 'a option
    method set		: 'a -> unit
    method wait		: unit -> 'a
  end

val map : ('a -> 'b) -> < add_callback : ('a -> unit) -> unit; .. > -> 'b t
