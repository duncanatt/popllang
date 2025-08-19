let () = 
  let result = Popllang.Math.add 2 3 in
  let _ = Popllang.Langone.Ast.Int 42 in
  (* Assuming Popllang.Expr.sqr is defined and used here *)
  Printf.printf "Result is 2 + 3 = %d\n" result
