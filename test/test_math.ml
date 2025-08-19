open OUnit2

let test_add _ =
  assert_equal 4 (Popllang.Math.add 2 2)

let suite =
  "math tests" >::: [
    "add" >:: test_add
  ]

let () = run_test_tt_main suite