open OUnit2

open Langoneext
open Langoneext.Ast
(* open Langoneext.Sem *)
(* open Langoneext.Types *)
(* open Langoneext.Lexer *)
(* open Langoneext.Parser *)

(* Helper functions *)
 let compare_ast (input: string) (expected: Ast.expr): test = 
   "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Run.get_ast input) expected
   
let compare_eval (input: string) (expected: Ast.value): test = 
  "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Sem.eval (Run.get_ast input)) expected

let compare_inferred_types (input: string) (expected: Types.typ): test = 
  "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Types.infer (Run.get_ast input) Langoneext.Types.empty_env) expected

let compare_type_check (input: string) (expected: Types.typ): test = 
  "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Types.check (Run.get_ast input) expected Langoneext.Types.empty_env) true


(* Tests *)

let tests: test list = [
  (* Compare AST outputs *)
  compare_ast "2" (Val (Num 2));
  compare_ast "2 + 3" (BinOp (Add, Val (Num 2), Val (Num 3)));
  compare_ast "2 - 3" (BinOp (Sub, Val (Num 2), Val (Num 3)));
  compare_ast "~(2 <= 3)" (UnOp (Not, BinOp (Leq, Val (Num 2), Val (Num 3))));
  compare_ast "(2 + 4) - (5 + 10)" (BinOp (Sub, (BinOp (Add, Val (Num 2), Val (Num 4))), (BinOp (Add, Val (Num 5), Val (Num 10)))));
  compare_ast "1-1 <= 2 + 3 + (~~4 && true)" (BinOp (Leq, BinOp (Sub, Val (Num 1), Val (Num 1)), BinOp (Add, BinOp (Add, Val (Num 2), Val (Num 3)), BinOp (And, UnOp (Not, UnOp (Not, Val (Num 4))), Val (Bool true)))));
  compare_ast "let x = 2 in x" (Let ("x", Val (Num 2), Var "x"));
  compare_ast "let x = (let y = 5 in y) in x + x" (Let ("x", Let ("y", Val (Num 5), Var "y"), BinOp (Add, Var "x", Var "x")));
  compare_ast "let x = let y = 5 in y in x + x" (Let ("x", Let ("y", Val (Num 5), Var "y"), BinOp (Add, Var "x", Var "x")));
  compare_ast "let x = 5 in let y = 6 in y + x" (Let ("x", Val (Num 5), Let ("y", Val (Num 6), BinOp (Add, Var "y", Var "x"))));
  compare_ast "let x = true in let x = 5 in x" (Let ("x", Val (Bool true), Let ("x", Val (Num 5), Var "x")));

  
  (* Compare AST outputs *)
  compare_eval "2" (Num 2);
  compare_eval "2 + 3" (Num 5);
  compare_eval "2 - 3" (Num (-1));
  compare_eval "~(2 <= 3)" (Bool false);
  compare_eval "(2 + 4) - (5 + 10)" (Num (-9));
  compare_eval "(1-1 <= (2 + 3)) && (~~false && true)" (Bool false); 
  compare_eval "let x = 2 in x" (Num 2);
  compare_eval "let x = (let y = 5 in y) in x + x" (Num 10);
  compare_eval "let x = let y = 5 in y in x + x" (Num 10);
  compare_eval "let x = 5 in let y = 6 in y + x" (Num 11);
  compare_eval "let x = true in let x = 5 in x" (Num 5);  
  
  (* Infer types *)
  compare_inferred_types "2" Types.TNum;
  compare_inferred_types "2 + 3" Types.TNum;
  compare_inferred_types "2 - 3" Types.TNum;
  compare_inferred_types "~(2 <= 3)" Types.TBool;
  compare_inferred_types "(2 + 4) - (5 + 10)" Types.TNum;
  compare_inferred_types "(1-1 <= (2 + 3)) && (~~false && true)" Types.TBool;
  compare_inferred_types "let x = 2 in x" Types.TNum;
  compare_inferred_types "let x = (let y = 5 in y) in x + x" Types.TNum;
  compare_inferred_types "let x = let y = 5 in y in x + x" Types.TNum;
  compare_inferred_types "let x = 5 in let y = 6 in y + x" Types.TNum;
  compare_inferred_types "let x = true in let x = 5 in x" Types.TNum;

  (* Type check *)
  compare_type_check "2" Types.TNum;
  compare_type_check "2 + 3" Types.TNum;
  compare_type_check "2 - 3" Types.TNum;
  compare_type_check "~(2 <= 3)" Types.TBool;
  compare_type_check "(2 + 4) - (5 + 10)" Types.TNum;
  compare_type_check "(1-1 <= (2 + 3)) && (~~false && true)" Types.TBool;
  compare_type_check "let x = 2 in x" Types.TNum;
  compare_type_check "let x = (let y = 5 in y) in x + x" Types.TNum;
  compare_type_check "let x = let y = 5 in y in x + x" Types.TNum;
  compare_type_check "let x = 5 in let y = 6 in y + x" Types.TNum;
  compare_type_check "let x = true in let x = 5 in x" Types.TNum;
]

let suite: test =
  "langoneext tests" >::: tests

let () = run_test_tt_main suite
