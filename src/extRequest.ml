open Batteries

module type Request = sig
  type env
  type response
end

module Channel (E : Request) : sig
  type t
  type 'a request = (E.env -> ('a * E.response))

  val create : unit -> t

  val process : t -> E.env -> E.response (* may rise End_of_file? *)

  val get_read_fd : t -> Unix.file_descr

  val sync : t -> 'a request -> ('a, exn) result

  val async : t -> 'a request -> unit
end = struct
  type 'a request = (E.env -> ('a * E.response))

  type t = {
    wr : Unix.file_descr;
    rd : Unix.file_descr;

    queue : (E.env -> E.response) Queue.t;
    mutex : Mutex.t;
  }

  let create () = 
    let (rd, wr) = Unix.pipe () in
    let queue = Queue.create () in
    let mutex = Mutex.create () in
    Unix.set_nonblock rd;
    { rd; wr; queue; mutex }

  let get_read_fd t = t.rd

  let rec try_read fd buf ofs len =
    try Unix.read fd buf ofs len
    with Unix.Unix_error (Unix.EINTR, _, _) -> try_read fd buf ofs len

  let rec try_write fd buf ofs len =
    try Unix.write fd buf ofs len
    with Unix.Unix_error (Unix.EINTR, _, _) -> try_write fd buf ofs len

  let process t env =
    let buf = String.make 1 ' ' in 
    match try_read t.rd buf 0 1 with
    | 0 -> raise End_of_file
    | 1 -> 
      Mutex.lock t.mutex;
      let req = Queue.take t.queue in
      Mutex.unlock t.mutex;
      req env
    | _ -> assert false

  let send t f =
    Mutex.lock t.mutex;
    Queue.add f t.queue;
    Mutex.unlock t.mutex;
    let buf = String.make 1 ' ' in 
    ignore (try_write t.wr buf 0 1)

  let sync t request =
    let ev = Event.new_channel () in
    send t (
      fun env -> 
        match wrap request env with
        | Ok (x, response) ->
          Event.sync (Event.send ev (Ok x));
          (response : E.response)
        | Bad exn ->
          Event.sync (Event.send ev (Bad exn));
          (response : E.response)
    );
    Event.sync (Event.receive ev)
      
  let async t request = send t (fun env -> snd (request env))
end
