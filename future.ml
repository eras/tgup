open Batteries

module Weaktbl = BatInnerWeaktbl

type never
type dependency = unit -> never (* used to keep a reference around, the function isn't actually called *)

class ['a] t =
object
  val mutex = Protect.create (Mutex.create ()) ()
  val mutable value : 'a option = None
  val mutable callbacks : (('a -> unit), unit) Weaktbl.t = Weaktbl.create 1
  val mutable dependencies : dependency list = []
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
	dependencies <- [];
	value <- Some x;
	callbacks
    in
    Weaktbl.iter (fun cb () -> cb x) cbs;
  method set_if_unset (x : 'a) =
    let was_set, cbs =
      Protect.access mutex @@ fun () ->
	if value = None then (
	  dependencies <- [];
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
  method add_dependency dep =
    Protect.access mutex @@ fun () ->
      if value = None then
	dependencies <- dep::dependencies;
end

type ('a, 'b) future_cb = (< add_callback : ('a -> unit) -> unit; .. > as 'b)

let map f t =
  let future = new t in
  let callback = fun x -> future#set (f x) in
  future#add_dependency (fun () -> callback (assert false); assert false);
  t#add_callback callback;
  future

let wait ts =
  let future = new t in
  let callback = (fun x -> ignore (future#set_if_unset x)) in
  future#add_dependency (fun () -> callback (assert false); assert false);
  List.iter 
    (fun t ->
      t#add_callback callback;
    )
    ts;
  future#wait ()
