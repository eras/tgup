class ['a] t =
object
  val mutable value : 'a option = None
  val mutable callbacks : ('a -> unit) list = []
  method get = value
  method set (x : 'a) =
    assert (value = None);
    value <- Some x;
    List.iter (fun cb -> cb x) callbacks
  method add_callback (cb : 'a -> unit) =
    match value with
    | None -> callbacks <- cb::callbacks
    | Some x -> cb x
end

let map f x =
  let future = new t in
  x#add_callback (fun x -> future#set (f x));
  future
