open OUnit2

let test_sqr _ =
  assert_equal ~printer:string_of_int 25 (Popllang.Expr.sqr 5)

let suite =
  "expr tests" >::: [
    "sqr" >:: test_sqr
  ]

let () = run_test_tt_main suite