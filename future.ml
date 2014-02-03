open Batteries

class ['a] t =
object
  val mutex = Protect.create (Mutex.create ()) ()
  val mutable value : 'a option = None
  val mutable callbacks : (('a -> unit), unit) BatInnerWeaktbl.t = BatInnerWeaktbl.create 1
  method get = Protect.access mutex @@ fun () -> value
  method wait () =
    Protect.wait_access mutex (fun () -> value <> None) @@ fun () ->
      match value with
      | None -> assert false
      | Some x -> x
  method set (x : 'a) =
    let cbs =
      Protect.access mutex @@ fun () ->
	assert (value = None);
	value <- Some x;
	callbacks
    in
    BatInnerWeaktbl.iter (fun cb () -> cb x) cbs;
  method add_callback (cb : 'a -> unit) =
    (Protect.access mutex @@ fun () ->
      match value with
      | None -> BatInnerWeaktbl.add callbacks cb (); ignore
      | Some x -> (fun () -> cb x)) ()
end

type ('a, 'b) future_cb = (< add_callback : ('a -> unit) -> unit; .. > as 'b)

let map f t =
  let future = new t in
  t#add_callback (fun x -> future#set (f x));
  future

let wait ts =
  let future = new t in
  List.iter 
    (fun t ->
      t#add_callback (fun x -> future#set x);
    )
    ts;
  future#wait ()
