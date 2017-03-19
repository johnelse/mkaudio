let (>>=) value f =
  match value with
  | Result.Ok result -> f result
  | Result.Error _ as error -> error

let (>|=) value f =
  match value with
  | Result.Ok result -> Result.Ok (f result)
  | Result.Error _ as error -> error

let to_sixteenths = function
  | Some beats -> Some (beats * 4)
  | None -> None

let calculate_samples sample_rate duration tempo sixteenths =
  match duration, tempo, sixteenths with
  | Some duration, None, None ->
    Result.Ok
      ((float_of_int sample_rate) *. duration
      |> int_of_float)
  | None, Some tempo, Some sixteenths ->
    Result.Ok
      ((float_of_int (sample_rate * sixteenths * 60 / 4)) /. tempo
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

type beat = {
  kick: bool;
  snare: bool;
  hihat: bool;
}

let get_length_if_equal =
  let rec get_length_if_equal' length_opt = function
    | [] -> length_opt
    | None :: rest -> get_length_if_equal' length_opt rest
    | Some str :: rest -> begin
      match length_opt with
      | None -> get_length_if_equal' (Some (String.length str)) rest
      | Some length ->
        if length = String.length str
        then get_length_if_equal' length_opt rest
        else None
    end
  in
  get_length_if_equal' None

let is_true = function
  | '1' | 'x' -> true
  | _ -> false

let get_beat index = function
  | Some pattern ->
    if index < 0 || index >= (String.length pattern)
    then false
    else is_true (String.get pattern index)
  | None -> false

let parse_beat_patterns kick snare hihat =
  match get_length_if_equal [kick; snare; hihat]
  with
  | None ->
    Result.Error
      "There must be at least one drum pattern; all must be the same length"
  | Some length -> begin
    let rec compile_beats acc index =
      if index < length
      then begin
        let acc = {
          kick = get_beat index kick;
          snare = get_beat index snare;
          hihat = get_beat index hihat;
        } :: acc
        in
        compile_beats acc (index + 1)
      end
      else acc
    in
    Result.Ok (
      compile_beats [] 0
      |> List.rev
    )
  end

