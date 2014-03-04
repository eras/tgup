open Batteries
open Common

module CharQueue : sig
  type t

  val create : unit -> t
  val time_of_next : t -> float option
  val get : t -> char
  val add : t -> string -> unit
end = struct
  type t = { 
    q         : char Queue.t;
    time_next : float;
  }

  let create () = {
    q         = Queue.create ();
    time_next = 0.0;
  }

  let time_of_next t = 
    if Queue.is_empty t.q
    then None
    else Some t.time_next
    
  let get t = Queue.take t.q

  let add t str =
    List.iter
      (fun c -> Queue.add c t.q)
      (String.explode str)
end

(* state internal to the thread *)
type internal = {
  i_cnc_fd          : Unix.file_descr;
  i_write_queue     : CharQueue.t;
  i_xon             : bool;
  i_queue_full      : bool;
}

module RequestEnv = struct
  type env = internal
end

module Reqs = ExtRequest.Channel(RequestEnv)

type t = {
  t_internal : internal;
  t_reqs     : Reqs.t;
}

let handle_reqs reqs internal loop =
  Reqs.process reqs internal;
  loop ()

let thread reqs internal =
  let rec loop handler =
    let (want_rws, timeout) =
      let now = Unix.gettimeofday () in
      match CharQueue.time_of_next internal.i_write_queue with
      | None -> ([], None)
      | Some time when time <= now -> ([internal.i_cnc_fd], None)
      | Some time -> ([], Some (time -. now))
    in
    let (rds, wrs, exn) = Unix.select [internal.i_cnc_fd; Reqs.get_read_fd reqs] want_rws [] (Option.default ~-.1.0 timeout) in
    match () with
    (* | _ when List.mem internal.i_cnc_fd rds -> handle_rds io_thread_state get_next_handler lb buf internal_state internal.i_cnc_fd handler loop *)
    | _ when List.mem (Reqs.get_read_fd reqs) rds -> handle_reqs reqs internal loop
    (* | _ when List.mem internal.i_cnc_fd wrs -> handle_write internal.i_cnc_fd internal_state (fun () -> loop handler) *)
    | _ -> loop handler
  in
  loop ()

let connect device bps =
  let i_cnc_fd = Unix.openfile device [Unix.O_RDWR] 0 in
  let tio = Unix.tcgetattr i_cnc_fd in
  let tio = { 
    tio with 
      Unix.c_clocal	= false;
      c_obaud		= bps;
      c_ibaud		= bps;
      c_csize		= 8;
      c_cstopb		= 1;
      c_inlcr           = false;
      c_icrnl		= false;
      c_opost           = false;
      c_isig		= false;
      c_icanon		= false;
      c_echo		= false;
      c_vtime           = 1;
      c_vmin		= 1;
  } in
  let _ = Unix.tcsetattr i_cnc_fd Unix.TCSANOW tio in
  let t_reqs = Reqs.create () in
  let t_internal = {
    i_cnc_fd;
    i_write_queue     = CharQueue.create ();
    i_xon             = false;
    i_queue_full      = false;
  } in
  let _ = Thread.create (fun () -> thread t_reqs t_internal) () in
  let t = { t_internal; t_reqs } in
  t
    
