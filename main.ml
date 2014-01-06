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

let default_prompt = 
  let doc = "A standalone G-code uploader for TinyG" in 
  let man = help_subcommands in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_opts_t)),
  Term.info "tgup" ~version ~sdocs:"COMMON OPTIONS" ~doc ~man

let cmd_upload = 
  Term.(pure Upload.upload $ common_opts_t $ source),
  Term.info "upload" ~version

let main () = 
  Sys.catch_break true;
  match Term.eval_choice default_prompt [cmd_upload] with `Error _ -> exit 1 | _ -> exit 0

let _ = main ()
