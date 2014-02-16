open Batteries
open Cmdliner
open Common

let version = "0.0.1"

let help_subcommands = [
  `S "COMMON OPTIONS";
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
  `P "Use `$(mname) help patterns' for help on patch matching."; `Noblank;
  `P "Use `$(mname) help environment' for help on environment variables.";
  `S "BUGS"; `P "Check bug reports at http://bugs.example.org.";]

let common_opts co_device co_bps co_send_bps = { co_device; co_bps; co_send_bps }

let common_opts_t = 
  let docs = "COMMON OPTIONS" in 
  let device = 
    let doc = "Set device to use." in
    Arg.(value & opt string "/dev/ttyUSB0" & info ["d"; "device"] ~docs ~doc)
  in
  let bps = 
    let doc = "Set bps to use." in
    Arg.(value & opt int 115200 & info ["b"; "bps"] ~docs ~doc)
  in
  let send_bps = 
    let doc = "Set bps to use for sending characters (by using delaying)." in
    Arg.(value & opt int 38400 & info ["s"; "send-bps"] ~docs ~doc)
  in
  Term.(pure common_opts $ device $ bps $ send_bps)

let source =
  let doc = "Source data" in  
  Arg.(required & pos 0 (some file) None & info [] ~doc)

module Arg =
struct
  include Arg

  let map f_parser f_printer (parser, printer) =
    let parser' str =
      match parser str with
      | (`Error _) as error -> error
      | `Ok x -> 
	match f_parser x with
	| (`Error _) as error -> error
	| (`Ok x) as ok -> ok
    in
    let printer' fmt v =
      printer fmt (f_printer v)
    in
    (parser', printer')

  let check f error =
    map 
      (fun v ->
	if not (f v)
	then `Error error
	else `Ok v)
      identity

  let positive = check (fun x -> x >= 1) "Argument must be >= 1"

  let ok x = `Ok x
end

let affine_matrix_of_list_parser xs =
  match xs with
  | e00::e01::e02::e10::e11::e12::[] -> `Ok (Gg.M3.v e00 e01 e02 e10 e11 e12 0.0 0.0 1.0)
  | _ -> `Error "Invalid number of elements (needs 6)"

let list_of_matrix m3 =
  Gg.M3.([e00 m3; e01 m3; e02 m3; e10 m3; e11 m3; e12 m3])

let start_from_line =
  let doc = "Start transmitting beginning from this line (first line is 1)" in
  Arg.(value & opt (positive int) 1 & info ["#"; "first-line"] ~doc)

let camera_matrix =
  let doc = "Define camera matrix" in
  Arg.(value & opt (some @@ map affine_matrix_of_list_parser list_of_matrix @@ list float) None & info ["camera-matrix"] ~doc)

let default_prompt = 
  let doc = "A standalone G-code uploader for TinyG" in 
  let man = help_subcommands in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_opts_t)),
  Term.info "tgup" ~version ~sdocs:"COMMON OPTIONS" ~doc ~man

let cmd_upload sigint_triggered = 
  Term.(pure (Upload.upload sigint_triggered) $ common_opts_t $ source $ start_from_line),
  Term.info "upload" ~version

let cmd_align sigint_triggered = 
  Term.(pure (Align.align sigint_triggered) $ common_opts_t $ camera_matrix),
  Term.info "align" ~version

let sigint_triggered = new Future.t

let sigint_handler _ = ignore (sigint_triggered#set_if_unset ())

let main () = 
  ignore (Sys.signal Sys.sigint (Sys.Signal_handle sigint_handler));
  match Term.eval_choice default_prompt [cmd_upload sigint_triggered; cmd_align sigint_triggered] with `Error _ -> exit 1 | _ -> exit 0

let _ = main ()
