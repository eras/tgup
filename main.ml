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

let map_converter f (parser, printer) =
  let parser' str =
    match parser str with
    | (`Error _) as error -> error
    | `Ok x ->  f x
  in
  (parser', printer)

let check f error =
  map_converter (
    fun v ->
      if not (f v)
      then `Error error
      else `Ok v
  )

let positive = check (fun x -> x >= 1) "Argument must be >= 1"

let start_from_line =
  let doc = "Start transmitting beginning from this line (first line is 1)" in
  Arg.(value & opt (positive int) 1 & info ["#"; "first-line"] ~doc)

let default_prompt = 
  let doc = "A standalone G-code uploader for TinyG" in 
  let man = help_subcommands in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_opts_t)),
  Term.info "tgup" ~version ~sdocs:"COMMON OPTIONS" ~doc ~man

let cmd_upload sigint_triggered = 
  Term.(pure (Upload.upload sigint_triggered) $ common_opts_t $ source $ start_from_line),
  Term.info "upload" ~version

let sigint_triggered = new Future.t

let sigint_handler _ = sigint_triggered#set ()

let main () = 
  ignore (Sys.signal Sys.sigint (Sys.Signal_handle sigint_handler));
  match Term.eval_choice default_prompt [cmd_upload sigint_triggered] with `Error _ -> exit 1 | _ -> exit 0

let _ = main ()
