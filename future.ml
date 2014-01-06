class ['a] t =
object
  val mutex = Protect.create (Mutex.create ()) ()
  val mutable value : 'a option = None
  val mutable callbacks : ('a -> unit) list = []
  method get = value
  method set (x : 'a) =
    let cbs =
      Protect.access mutex @@ fun () ->
	assert (value = None);
	value <- Some x;
	callbacks
    in
    List.iter (fun cb -> cb x) cbs;
  method add_callback (cb : 'a -> unit) =
    (Protect.access mutex @@ fun () ->
      match value with
      | None -> callbacks <- cb::callbacks; ignore
      | Some x -> (fun () -> cb x)) ()
end

let map f x =
  let future = new t in
  x#add_callback (fun x -> future#set (f x));
  future
