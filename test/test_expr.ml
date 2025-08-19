open OUnit2

let test_sqr _ =
  assert_equal 25 (Popllang.Expr.sqr 5)

  let test_sqr2 _ =
  assert_equal 251 (Popllang.Expr.sqr 5)

let suite =
  "expr tests" >::: [
    "sqr" >:: test_sqr;
    "sqr2" >:: test_sqr2
  ]

let () = run_test_tt_main suite