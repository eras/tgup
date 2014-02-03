open Batteries

module Weaktbl = BatInnerWeaktbl

class ['a] t =
object
  val mutex = Protect.create (Mutex.create ()) ()
  val mutable value : 'a option = None
  val mutable callbacks : (('a -> unit), unit) Weaktbl.t = Weaktbl.create 1
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
    Weaktbl.iter (fun cb () -> cb x) cbs;
  method set_if_unset (x : 'a) =
    let was_set, cbs =
      Protect.access mutex @@ fun () ->
	if value = None then (
	  value <- Some x;
	  (true, callbacks)
	) else (
	  (false, Weaktbl.create 0)
	)
    in
    Weaktbl.iter (fun cb () -> cb x) cbs;
    was_set
  method add_callback (cb : 'a -> unit) =
    (Protect.access mutex @@ fun () ->
      match value with
      | None -> Weaktbl.add callbacks cb (); ignore
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
