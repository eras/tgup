(* Copyright 2014 Erkki Seppälä <flux@modeemi.fi>

   Licensed under the MIT license.

   Futures are a mechanism for performing interprocess message
   passing. A future represents a value that may or may not be
   currently set, but may be set in the future. Once a value has been
   assigned to a Future, it will not be changed. (Changing it again
   will either throw an exception or be a no-op, depending on which
   way it was done.)
*)

type never
type dependency = unit -> never (* used to keep a reference around, the function isn't actually called *)

exception FutureAlreadySet

class type ['a] readonly =
object
  (* [add_callback cb] adds function cb to be called when a value is
     assigned (it may be called immediately if the value is already
     set). The returned 'dependency' should be stored if is not OK for
     the callback to be forgotten when the last reference to it is lost. *)
  method add_callback : ('a -> unit) -> dependency

  (* [add_persistent_callback cb] work similarly as add_callback, except
     the callback will not be garbage collected. *)
  method add_persistent_callback : ('a -> unit) -> unit

  (* [get] gets the current value of the future. It may be None or Some x. *)
  method get		: 'a option

  (* [wait] synchronously waits for a value to be set to the future and
     returns with the value once it has been set. It may also return
     immediately if the future already has a value. *)
  method wait		: unit -> 'a

  (* [add_dependency dependency] adds a dependency value the future can
     hold on to prevents its dependencies (ie. the callback it has
     provided #add_callback to) from being garbage collected if there
     are no longer references to them. *)
  method add_dependency : dependency -> unit
end

class ['a] t :
object
  inherit ['a] readonly
  (* [set x] sets a value to the future. Once a value has been set it
     cannot be unset or reset to another value. Attempting to do that
     will raise [FutureAlreadySet]. *)
  method set		: 'a -> unit

  (* [set_if_unset x] is similar to set, except it will simply return true
     if the value was already set, otherwise it will return false. *)
  method set_if_unset	: 'a -> bool
end

(* The callback-aspect of the future class *)
type ('a, 'b) future_cb = (< add_callback : ('a -> unit) -> dependency; add_persistent_callback : ('a -> unit) -> unit; .. > as 'b)

(* [map f future] creates a new future that maps the value of a given function with the
   given functino. Throwing an exception here may pop up at unexpected places.. *)
val map : ('a -> 'b) -> ('a, _) future_cb -> 'b readonly

(* [wait futures] waits the first future to be active and returns that value. Note: It can be
   non-deterministic which value is set if two futures are active simulatenously or upon entrance. *)
val wait : ('a, _) future_cb list -> 'a
