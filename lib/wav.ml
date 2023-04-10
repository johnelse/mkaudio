let write ~channels ~sample_rate ~samples ~generator ~output_file =
  let wav = new Mm.Audio.IO.Writer.to_wav_file channels sample_rate output_file in

  let buffer_length = 1024 in
  let buffer = Mm.Audio.create channels buffer_length in

  let rec write samples_left =
    if samples_left > 0 then begin
      let samples_to_write = min samples_left buffer_length in
      generator#fill buffer 0 samples_to_write;
      wav#write buffer 0 samples_to_write;

      write (samples_left - samples_to_write)
    end
  in

  write samples;
  Result.Ok (wav#close)
