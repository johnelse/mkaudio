open OUnit2

let suite =
  "base suite" >::: [
    Test_beat.suite;
    Test_samples.suite;
  ]

let () = run_test_tt_main suite
