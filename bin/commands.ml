open Mkaudio_libs
open Fun

let to_steps = function
  | Some beats -> Some (beats * 4)
  | None -> None

let parse_duration_opt = function
  | Some str ->
    Time.parse_duration str >|= (fun duration -> Some duration)
  | None -> Result.Ok None

let get_samples ~sample_rate ~duration ~tempo ~beats =
  parse_duration_opt duration
  >>= fun duration ->
    Time.calculate_samples
      ~sample_rate ~duration ~tempo ~steps:(to_steps beats)

let saw channels sample_rate gain duration tempo beats frequency output_file =
  get_samples ~sample_rate ~duration ~tempo ~beats
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.saw ~volume:gain sample_rate frequency) in
    Wav.write ~channels ~sample_rate ~samples ~generator ~output_file

let sine channels sample_rate gain duration tempo beats frequency output_file =
  get_samples ~sample_rate ~duration ~tempo ~beats
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.sine ~volume:gain sample_rate frequency) in
    Wav.write ~channels ~sample_rate ~samples ~generator ~output_file

let square channels sample_rate gain duration tempo beats frequency output_file =
  get_samples ~sample_rate ~duration ~tempo ~beats
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.square ~volume:gain sample_rate frequency) in
    Wav.write ~channels ~sample_rate ~samples ~generator ~output_file

let triangle channels sample_rate gain duration tempo beats frequency output_file =
  get_samples ~sample_rate ~duration ~tempo ~beats
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.triangle ~volume:gain sample_rate frequency) in
    Wav.write ~channels ~sample_rate ~samples ~generator ~output_file

let white_noise channels sample_rate gain duration tempo beats output_file =
  get_samples ~sample_rate ~duration ~tempo ~beats
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.white_noise ~volume:gain sample_rate) in
    Wav.write ~channels ~sample_rate ~samples ~generator ~output_file

let beat channels sample_rate gain tempo kick snare hihat repeats output_file =
  let repeats = max 1 repeats in
  Beat.parse_patterns ~kick ~snare ~hihat
  >>= fun steps ->
    Time.calculate_samples
      ~sample_rate ~duration:None ~tempo ~steps:(Some (List.length steps))
  >>= fun samples ->
    let step_length = samples / (List.length steps) in
    let buffer = Audio.create channels step_length in
    let wav = new Audio.IO.Writer.to_wav_file channels sample_rate output_file in
    let rec add_steps buffer offset = function
      | [] -> ()
      | beat :: rest -> begin
        Audio.clear buffer;
        if beat.Beat.kick
        then begin
          let kick = Generators.kick sample_rate gain in
          kick#fill_add buffer
        end;
        if beat.Beat.snare
        then begin
          let snare = Generators.snare sample_rate gain in
          snare#fill_add buffer
        end;
        if beat.Beat.hihat
        then begin
          let hihat = Generators.hihat sample_rate gain in
          hihat#fill_add buffer
        end;
        wav#write buffer;
        add_steps buffer (offset + 1) rest
      end
    in
    for _ = 1 to repeats do
      add_steps buffer 0 steps
    done;
    Result.Ok (wav#close)
