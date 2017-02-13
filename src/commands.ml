let (>>=) value f =
  match value with
  | Result.Ok result -> f result
  | Result.Error _ as error -> error

let calculate_samples sample_rate duration tempo beats =
  match duration, tempo, beats with
  | Some duration, None, None ->
    Result.Ok
      ((float_of_int sample_rate) *. duration
      |> int_of_float)
  | None, Some tempo, Some beats ->
    Result.Ok
      ((float_of_int (sample_rate * beats * 60)) /. tempo
      |> int_of_float)
  | None, None, None
  | Some _, _, _ ->
    Result.Error
      "You either need to specify the duration or the tempo and number of beats"
  | None, _, _ ->
    Result.Error "You need to specify both the tempo and number of beats"

let write_wav channels sample_rate samples generator output_file =
  let wav = new Audio.IO.Writer.to_wav_file channels sample_rate output_file in
  let buffer_length = 1024 in
  let buffer = Audio.create channels buffer_length in
  for i = 0 to samples / buffer_length - 1 do
    generator#fill buffer 0 buffer_length;
    wav#write buffer 0 buffer_length
  done;
  Result.Ok (wav#close)

let sine channels sample_rate duration tempo beats frequency output_file =
  calculate_samples sample_rate duration tempo beats
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.sine sample_rate frequency) in
    write_wav channels sample_rate samples generator output_file

let square channels sample_rate duration tempo beats frequency output_file =
  calculate_samples sample_rate duration tempo beats
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.square sample_rate frequency) in
    write_wav channels sample_rate samples generator output_file
