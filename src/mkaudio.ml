open Cmdliner

(* Command definitions *)
let help_secs = [
  `S "MORE HELP";
  `P "Use `$(mname) $(i,command) --help' for help on a single command.";
  `Noblank;
]

let default_cmd =
  let doc = "mkaudio" in
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ pure ())),
  Term.info "mkaudio" ~version:"0.1" ~doc ~man

let cmds = [
]

let () =
  Printexc.record_backtrace true;
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | `Version | `Help -> ()
  | `Ok () -> ()
