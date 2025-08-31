open OUnit2
open Langtwo
open Langtwo.Ast

(* Helper functions *)
let compare_ast (input: string) (expected: Ast.expr): test = 
   "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Run.get_ast input) expected
   
(* let compare_eval (input: string) (expected: Ast.value): test = 
  "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Sem.eval (Run.get_ast input)) expected
 *)

(* Tests *)

let tests: test list = [
  (* Compare AST outputs *)
  compare_ast "1" (Val (Num 1));
  compare_ast "f (a)" (App (Var "f", Var "a"));
  compare_ast "1 <= fun f(y){y + 2} 2"  (BinOp (Leq, Val (Num 1), App (Fun ("f", "y", BinOp (Add, Var "y", Val (Num 2))), Val (Num 2))));
  compare_ast "(f + 1) a b" (App (App (BinOp (Add, Var "f", Val (Num 1)), Var "a"), Var "b"));
  compare_ast "f a b" (App (App (Var "f", Var "a"), Var "b"));
  compare_ast "if true then a else false f" (If (Val (Bool true), Var "a", App (Val (Bool false), Var "f")));
  compare_ast "f (if true then a else b) y"  (App (App (Var "f", If (Val (Bool true), Var "a", Var "b")), Var "y"));
  compare_ast "3 + a (b)" (BinOp (Add, Val (Num 3), App (Var "a", Var "b"))); (* precedence *)
  compare_ast "(3 + a) (b)" (App (BinOp (Add, Val (Num 3), Var "a"), Var "b")); (* precedence *)
  compare_ast 
    "let x = 1
        in let f = fun(y){y + x} 
          in let x = 2
            in f 3"
    (App (FunAnon ("x", App (FunAnon ("f", App (FunAnon ("x", App (Var "f", Val (Num 3))), Val (Num 2))), FunAnon ("y", BinOp (Add, Var "y", Var "x")))), Val (Num 1)));

  (* Syntactic sugar expanded ASTs *)
  (* let *)
  compare_ast "let x = 5 in x + 1" (App (FunAnon ("x", BinOp (Add, Var "x", Val (Num 1))), Val (Num 5)));
  (* let (with multiple bindings) *)
  compare_ast "let x = 5, y = f 1 in x + x" (App (FunAnon ("x", App (FunAnon ("y", BinOp (Add, Var "x", Var "x")), App (Var "f", Val (Num 1)))), Val (Num 5)));
  (* operators: &&, ~, == *)
  compare_ast "true && false" (If (Val (Bool true), If (Val (Bool false), Val (Bool true), Val (Bool false)), If (Val (Bool false), Val (Bool false), Val (Bool false))));
  compare_ast "~true" (If (Val (Bool true), Val (Bool false), Val (Bool true)));
  compare_ast "1 == 2" (If (BinOp (Leq, Val (Num 1), Val (Num 2)), If (BinOp (Leq, Val (Num 2), Val (Num 1)), Val (Bool true), Val (Bool false)), If (BinOp (Leq, Val (Num 2), Val (Num 1)), Val (Bool false), Val (Bool false))));
  compare_ast "1 + 2 == 3 - 4" (If (BinOp (Leq, BinOp (Add, Val (Num 1), Val (Num 2)), BinOp (Sub, Val (Num 3), Val (Num 4))), If (BinOp (Leq, BinOp (Sub, Val (Num 3), Val (Num 4)), BinOp (Add, Val (Num 1), Val (Num 2))), Val (Bool true), Val (Bool false)), If (BinOp (Leq, BinOp (Sub, Val (Num 3), Val (Num 4)), BinOp (Add, Val (Num 1), Val (Num 2))), Val (Bool false), Val (Bool false))));
  (* named functions with multiple arguments *)
  compare_ast "fun f(x){ let y = 3 in x + y }" (Fun ("f", "x", App (FunAnon ("y", BinOp (Add, Var "x", Var "y")), Val (Num 3))));
  compare_ast "fun f(x){ fun g(y){ y (a b c + 2) } yy }" (Fun ("f", "x", App (Fun ("g", "y", App (Var "y", BinOp (Add, App (App (Var "a", Var "b"), Var "c"), Val (Num 2)))), Var "yy")));
  (* anonymous functions as basic constructs *)
  compare_ast "fun(x){x + 1} 3" (App (FunAnon ("x", BinOp (Add, Var "x", Val (Num 1))), Val (Num 3)));
  compare_ast "if true then (fun f(x){3}) s else 2" (If (Val (Bool true), App (Fun ("f", "x", Val (Num 3)), Var "s"), Val (Num 2)));
]

let suite: test =
  "langtwo tests" >::: tests

let () = run_test_tt_main suite
