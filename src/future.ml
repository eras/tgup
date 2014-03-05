open Batteries

module Weaktbl = BatInnerWeaktbl

exception FutureAlreadySet

type never
type dependency = unit -> never (* used to keep a reference around, the function isn't actually called *)

class type ['a] readonly =
object
  method add_callback : ('a -> unit) -> dependency
  method add_persistent_callback : ('a -> unit) -> unit
  method get		: 'a option
  method wait		: unit -> 'a
  method add_dependency : dependency -> unit
end

class ['a] t =
object (self : 'self)
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
  method private set_with_fallback : 'fallback_retval. 'a -> (bool -> (unit -> 'fallback_retval)) -> 'fallback_retval =
    fun x fallback ->
      (Protect.access mutex @@ fun () ->
	if value <> None then
	  fallback false
	else (
	  dependencies <- [];
	  value <- Some x;
	  let (weak_cbs', persistent_cbs') = (callbacks, persistent_callbacks) in
	  persistent_callbacks <- [];
	  fun () ->
	    Weaktbl.iter (fun cb () -> cb x) weak_cbs';
	    List.iter (fun cb -> cb x) persistent_cbs';
	    fallback true ()
	)) ()
  method set_if_unset (x : 'a) =
    self#set_with_fallback x @@ function
    | false -> const false
    | true -> const true
  method set (x : 'a) =
    self#set_with_fallback x @@ function
    | false -> raise FutureAlreadySet
    | true -> const ()
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
  (future :> _ readonly)

let wait ts =
  let future = new t in
  let callback = (fun x -> ignore (future#set_if_unset x)) in
  List.iter 
    (fun t ->
      future#add_dependency (t#add_callback callback)
    )
    ts;
  future#wait ()
