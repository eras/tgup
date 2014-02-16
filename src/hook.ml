type id = Id of int

type 'a t = {
  callbacks : (id, 'a -> unit) Hashtbl.t Protect.t;
}

(* Actually contains only a removal function.. *)
type hook_id = unit -> unit

let mk_id () = Id (Oo.id (object end))

let create () = { callbacks = Protect.create (Mutex.create ()) (Hashtbl.create 1) }

let clone t =
  { callbacks =
      Protect.access t.callbacks @@ fun callbacks ->
	Protect.create (Mutex.create ())
	  (Hashtbl.fold
	     (fun k v cbs ->
	       Hashtbl.add cbs k v;
	       cbs)
	     callbacks
	     (Hashtbl.create (Hashtbl.length callbacks)))
  }

let issue t value =
  let cbs = Protect.access t.callbacks @@ fun callbacks -> Hashtbl.fold (fun _ f cbs -> f::cbs) callbacks [] in
  List.iter
    (fun x -> x value)
    cbs

let hook t f =
  let id = mk_id () in
  Protect.access t.callbacks (fun callbacks ->
    Hashtbl.add callbacks id f;
  );
  let remove () =
    Protect.access t.callbacks (fun callbacks ->
      Hashtbl.remove callbacks id;
    );
  in
  remove
    
let unhook id = id ()
