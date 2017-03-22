open OUnit2

let test_duration _ =
  assert_equal
    (Samples.calculate
      ~sample_rate:48000 ~duration:(Some 1.) ~tempo:None ~sixteenths:None)
    (Result.Ok 48000)

let test_tempo_and_sixteenths _ =
  assert_equal
    (Samples.calculate
      ~sample_rate:48000 ~duration:None
      ~tempo:(Some 120.) ~sixteenths:(Some 16))
    (Result.Ok 96000)

let test_missing_args _ =
  assert_bool "check missing args produce an error" (
    match
      Samples.calculate
        ~sample_rate:48000 ~duration:None
        ~tempo:None ~sixteenths:None
    with
    | Result.Ok _ -> false
    | Result.Error _ -> true
  )

let test_missing_tempo _ =
  assert_bool "check missing tempo produces an error" (
    match
      Samples.calculate
        ~sample_rate:48000 ~duration:None
        ~tempo:None ~sixteenths:(Some 16)
    with
    | Result.Ok _ -> false
    | Result.Error _ -> true
  )

let test_missing_sixteenths _ =
  assert_bool "check missing tempo produces an error" (
    match
      Samples.calculate
        ~sample_rate:48000 ~duration:None
        ~tempo:(Some 240.) ~sixteenths:None
    with
    | Result.Ok _ -> false
    | Result.Error _ -> true
  )

let suite =
  "samples" >::: [
    "test_duration" >:: test_duration;
    "test_tempo_and_sixteenths" >:: test_tempo_and_sixteenths;
    "test_missing_args" >:: test_missing_args;
    "test_missing_tempo" >:: test_missing_tempo;
    "test_missing_sixteenths" >:: test_missing_sixteenths;
  ]
