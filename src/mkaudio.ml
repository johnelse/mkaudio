open Cmdliner

(* man page definition. *)
let help man_format cmds topic =
  match topic with
  | None -> `Help (`Pager, None)
  | Some topic ->
    let topics = "topics" :: cmds in
    let conv, _ = Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" ->
      List.iter print_endline topics;
      `Ok (Result.Ok ())
    | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
    | `Ok t ->
      let page = (topic, 7, "", "", ""), [`S topic; `P "Say something"] in
      Manpage.print man_format Format.std_formatter page;
      `Ok (Result.Ok ())

let help_secs = [
  `S "MORE HELP";
  `P "Use `$(mname) $(i,command) --help' for help on a single command.";
  `Noblank;
]

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about mkaudio" in
  let man = [
    `S "DESCRIPTION";
    `P "Prints help about mkaudio commands."
  ] @ help_secs in
  Term.(ret (pure help $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

(* Argument definitions. *)
let channels =
  let doc = "The number of channels to use when creating the audio file." in
  Arg.(value & opt int 2 & info ["channels"] ~docv:"CHANNELS" ~doc)

let sample_rate =
  let doc = "The sample rate to use when creating the audio file." in
  Arg.(value & opt int 44100 & info ["samplerate"] ~docv:"SAMPLERATE" ~doc)

let duration =
  let doc = "The duration of the created file in seconds." in
  Arg.(value & opt (some float) None & info ["duration"] ~docv:"DURATION" ~doc)

let tempo =
  let doc = "The tempo of the created file in beats per minute." in
  Arg.(value & opt (some float) None & info ["tempo"] ~docv:"TEMPO" ~doc)

let beats =
  let doc = "The duration of the created file in beats." in
  Arg.(value & opt (some int) None & info ["beats"] ~docv:"BEATS" ~doc)

let frequency =
  let doc = "The frequency of the generated sound" in
  Arg.(value & opt float 440. & info ["frequency"] ~docv:"FREQUENCY" ~doc)

let output_file =
  let doc = "The file to write." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTPUTFILE" ~doc)

(* Command definitions. *)
let saw_cmd =
  let doc = "write an audio file containing a saw wave." in
  let man = [
    `S "DESCRIPTION";
    `P "Write an audio file containing a saw wave.";
  ] @ help_secs in
  Term.(pure Commands.saw
    $ channels
    $ sample_rate
    $ duration
    $ tempo
    $ beats
    $ frequency
    $ output_file),
  Term.info "saw" ~doc ~man

let sine_cmd =
  let doc = "write an audio file containing a sine wave." in
  let man = [
    `S "DESCRIPTION";
    `P "Write an audio file containing a sine wave.";
  ] @ help_secs in
  Term.(pure Commands.sine
    $ channels
    $ sample_rate
    $ duration
    $ tempo
    $ beats
    $ frequency
    $ output_file),
  Term.info "sine" ~doc ~man

let square_cmd =
  let doc = "write an audio file containing a square wave." in
  let man = [
    `S "DESCRIPTION";
    `P "Write an audio file containing a square wave.";
  ] @ help_secs in
  Term.(pure Commands.square
    $ channels
    $ sample_rate
    $ duration
    $ tempo
    $ beats
    $ frequency
    $ output_file),
  Term.info "square" ~doc ~man

let white_noise_cmd =
  let doc = "write an audio file containing white noise." in
  let man = [
    `S "DESCRIPTION";
    `P "Write an audio file containing white noise.";
  ] @ help_secs in
  Term.(pure Commands.white_noise
    $ channels
    $ sample_rate
    $ duration
    $ tempo
    $ beats
    $ output_file),
  Term.info "white-noise" ~doc ~man

let default_command =
  let doc = "mkaudio" in
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ pure ())),
  Term.info "mkaudio" ~version:"0.1" ~doc ~man

let commands = [
  help_cmd;
  saw_cmd;
  sine_cmd;
  square_cmd;
  white_noise_cmd;
]

let () =
  Printexc.record_backtrace true;
  match Term.eval_choice default_command commands with
  | `Error _ -> exit 1
  | `Ok (Result.Ok ()) | `Version | `Help -> ()
  | `Ok (Result.Error msg) ->
    print_endline msg;
    exit 1
