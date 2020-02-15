let write ~channels ~sample_rate ~samples ~generator ~output_file =
  let wav = new Audio.IO.Writer.to_wav_file channels sample_rate output_file in
  let buffer_length = 1024 in
  let buffer = Audio.create channels buffer_length in

  let rec write samples_left =
    let to_write = min samples_left buffer_length in
    if to_write > 0 then begin
      generator#fill buffer 0 to_write;
      wav#write buffer 0 to_write;
      write (samples_left - to_write)
    end
  in

  write samples;
  Result.Ok (wav#close)
