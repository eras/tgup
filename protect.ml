open Batteries

type 'a t = ('a * Mutex.t)
let make mutex value = (value, mutex)
let access (value, mutex) f = 
  Mutex.lock mutex;
  let v = wrap f value in
  Mutex.unlock mutex;
  ok v
