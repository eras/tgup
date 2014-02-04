open Batteries

module Weaktbl = BatInnerWeaktbl

type never
type dependency = unit -> never (* used to keep a reference around, the function isn't actually called *)

class ['a] t =
object
  val mutex = Protect.create (Mutex.create ()) ()
  val mutable value : 'a option = None
  val mutable callbacks : (('a -> unit), unit) Weaktbl.t = Weaktbl.create 1
  val mutable persistent_callbacks : ('a -> unit) list = [] 
  val mutable dependencies : dependency list = []
  method get = Protect.access mutex @@ fun () -> value
  method wait () =
    Protect.wait_access mutex (fun () -> value <> None) @@ fun () ->
      match value with
      | None -> assert false
      | Some x -> x
  method set (x : 'a) =
    let (weak_cbs, persistent_cbs) =
      Protect.access mutex @@ fun () ->
	assert (value = None);
	dependencies <- [];
	value <- Some x;
	let vs = (callbacks, persistent_callbacks) in
	persistent_callbacks <- [];
	vs
    in
    Weaktbl.iter (fun cb () -> cb x) weak_cbs;
    List.iter (fun cb -> cb x) persistent_cbs;
  method set_if_unset (x : 'a) =
    let was_set, (weak_cbs, persistent_cbs) =
      Protect.access mutex @@ fun () ->
	if value = None then (
	  dependencies <- [];
	  value <- Some x;
	  let vs = (true, (callbacks, persistent_callbacks)) in
	  persistent_callbacks <- [];
	  vs
	) else (
	  (false, (Weaktbl.create 0, []))
	)
    in
    Weaktbl.iter (fun cb () -> cb x) weak_cbs;
    List.iter (fun cb -> cb x) persistent_cbs;
    was_set
  method add_persistent_callback (cb : 'a -> unit) : unit =
    (Protect.access mutex @@ fun () ->
      match value with
      | None -> persistent_callbacks <- cb::persistent_callbacks; ignore
      | Some x -> (fun () -> cb x)) ()
  method add_callback (cb : 'a -> unit) : dependency =
    (Protect.access mutex @@ fun () ->
      match value with
      | None -> Weaktbl.add callbacks cb (); const ((fun () -> cb (assert false); assert false) : dependency)
      | Some x -> (fun () -> cb x; (fun _ -> assert false))) ()
  method add_dependency dep =
    Protect.access mutex @@ fun () ->
      if value = None then
	dependencies <- dep::dependencies;
end

type ('a, 'b) future_cb = (< add_callback : ('a -> unit) -> dependency; add_persistent_callback : ('a -> unit) -> unit; .. > as 'b)

let map f t =
  let future = new t in
  future#add_dependency (t#add_callback (fun x -> future#set (f x)));
  future

let wait ts =
  let future = new t in
  let callback = (fun x -> ignore (future#set_if_unset x)) in
  List.iter 
    (fun t ->
      future#add_dependency (t#add_callback callback)
    )
    ts;
  future#wait ()
