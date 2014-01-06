open Batteries

let strip = Pcre.replace ~pat:"[\r\n]*$"

let lockfile_of_device_name device_name =
  let device_base = (Pcre.extract ~pat:"([^/]*)$" ~full_match:false device_name).(0) in
  let lockfile = Printf.sprintf "/var/lock/LCK..%s" device_base in
  lockfile

let lock_serial device_name =
  let lockfile = lockfile_of_device_name device_name in
  let locking_pid = 
    try Some (int_of_string (strip (input_file lockfile)))
    with Sys_error _ -> None
  in
  let locked =
    match locking_pid with
    | Some pid ->
      ( try Unix.kill pid 0; true
	with Unix.Unix_error (Unix.ESRCH, _, _) -> false )
    | None -> false
  in
  (* should also check if the lock file is older than the process in question? *)
  if locked then (
    Printf.eprintf "%s is locked by pid %d\n%!" device_name (Option.get locking_pid);
    false
  ) else (
    File.with_file_out ~mode:[`create; `trunc] (* ~perm:(File.unix_perm 0644) *) lockfile @@ fun f ->
      Printf.fprintf f "%d\n%!" (Unix.getpid ());
      true
  )

let unlock_serial device_name = Unix.unlink (lockfile_of_device_name device_name)

let open_serial (device, bps) =
  let fd = Unix.openfile device [Unix.O_RDWR] 0 in
  let tio = Unix.tcgetattr fd in
  let tio = { 
    tio with 
      Unix.c_clocal = false;
      c_obaud	    = bps;
      c_ibaud	    = bps;
      c_csize	    = 8;
      c_cstopb      = 1;
      c_inlcr       = false;
      c_icrnl	    = false;
      c_opost       = false;
      c_isig	    = false;
      c_icanon      = false;
      c_echo	    = false;
      c_vtime       = 0;
      c_vmin	    = 1;
      c_ixon	    = false; (* let our software deal with this *)
      c_ixoff	    = false; (* well, TinyG doesn't support this anyway? *)
  } in
  let _ = Unix.tcsetattr fd Unix.TCSANOW tio in
  fd

let with_serial ((device_name, bps) as open_args) f =
  if lock_serial device_name then
    let doit () =
      let fd = open_serial open_args in
      let v = wrap f fd in
      Unix.close fd;
      ok v
    in
    let v = wrap doit () in
    unlock_serial device_name;
    ok v
  else
    failwith "Serial port is locked"

