let (>>=) value f =
  match value with
  | Result.Ok result -> f result
  | Result.Error _ as error -> error

let (>|=) value f =
  match value with
  | Result.Ok result -> Result.Ok (f result)
  | Result.Error _ as error -> error

let saw channels sample_rate duration tempo beats frequency output_file =
  Libs.calculate_samples sample_rate duration tempo (Libs.to_sixteenths beats)
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.saw sample_rate frequency) in
    Libs.write_wav channels sample_rate samples generator output_file

let sine channels sample_rate duration tempo beats frequency output_file =
  Libs.calculate_samples sample_rate duration tempo (Libs.to_sixteenths beats)
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.sine sample_rate frequency) in
    Libs.write_wav channels sample_rate samples generator output_file

let square channels sample_rate duration tempo beats frequency output_file =
  Libs.calculate_samples sample_rate duration tempo (Libs.to_sixteenths beats)
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.square sample_rate frequency) in
    Libs.write_wav channels sample_rate samples generator output_file

let white_noise channels sample_rate duration tempo beats output_file =
  Libs.calculate_samples sample_rate duration tempo (Libs.to_sixteenths beats)
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.white_noise sample_rate) in
    Libs.write_wav channels sample_rate samples generator output_file

let kick_gen sample_rate =
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0.001, 0.3, 0., 1.) in
  let sine = new Audio.Mono.Generator.sine sample_rate 60. in
  let kick = new Audio.Mono.Generator.adsr adsr sine in
  new Audio.Generator.of_mono kick

let snare_gen sample_rate =
  let lpf =
    new Audio.Mono.Effect.biquad_filter sample_rate `Low_pass 2000. 2. in
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0., 0.08, 0., 1.) in
  let noise = new Audio.Mono.Generator.white_noise sample_rate in
  let filtered = new Audio.Mono.Generator.chain noise lpf in
  let snare = new Audio.Mono.Generator.adsr adsr filtered in
  new Audio.Generator.of_mono snare

let hihat_gen sample_rate =
  let hpf =
    new Audio.Mono.Effect.biquad_filter sample_rate `High_pass 8000. 2. in
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0., 0.05, 0., 1.) in
  let noise = new Audio.Mono.Generator.white_noise sample_rate in
  let filtered = new Audio.Mono.Generator.chain noise hpf in
  let snare = new Audio.Mono.Generator.adsr adsr filtered in
  new Audio.Generator.of_mono snare

let beat channels sample_rate tempo kick snare hihat output_file =
  Libs.parse_beat_patterns kick snare hihat
  >>= fun beats -> Libs.calculate_samples sample_rate None tempo (Some (List.length beats))
  >>= fun samples ->
    let beat_length = samples / (List.length beats) in
    let buffer = Audio.create channels samples in
    let rec add_beats buffer offset = function
      | [] -> ()
      | beat :: rest -> begin
        if beat.Libs.kick
        then begin
          let kick_gen' = kick_gen sample_rate in
          kick_gen'#fill_add buffer (offset * beat_length) beat_length
        end;
        if beat.Libs.snare
        then begin
          let snare_gen' = snare_gen sample_rate in
          snare_gen'#fill_add buffer (offset * beat_length) beat_length
        end;
        if beat.Libs.hihat
        then begin
          let hihat_gen' = hihat_gen sample_rate in
          hihat_gen'#fill_add buffer (offset * beat_length) beat_length
        end;
        add_beats buffer (offset + 1) rest
      end
    in
    add_beats buffer 0 beats;
    let wav = new Audio.IO.Writer.to_wav_file channels sample_rate output_file in
    wav#write buffer 0 (beat_length * (List.length beats));
    Result.Ok (wav#close)
