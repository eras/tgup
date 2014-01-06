open Batteries

type 'a t = ('a * Mutex.t * Condition.t)
let create ?cond mutex value = (value, mutex, Option.default (Condition.create ()) cond)
let access (value, mutex, condition) f = 
  Mutex.lock mutex;
  let v = wrap f value in
  Condition.broadcast condition;
  Mutex.unlock mutex;
  ok v
let wait_access (value, mutex, condition) p f =
  access (value, mutex, condition) @@ fun value ->
    let rec wait () =
      match wrap p value with
      | Ok true -> ()
      | Ok false ->
	Condition.wait condition mutex;
	wait ()
      | Bad exn -> raise exn
    in
    wait ();
    f value
