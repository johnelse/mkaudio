let calculate sample_rate duration tempo sixteenths =
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
