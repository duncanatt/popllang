open OUnit2
open Langone
open Langone.Ast


(* Helper functions *)

 let compare_ast (input: string) (expected: Ast.expr): test = 
   "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Run.get_ast input) expected
   
let compare_eval (input: string) (expected: Ast.value): test = 
  "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Sem.eval (Run.get_ast input)) expected

let compare_inferred_types (input: string) (expected: Types.typ): test = 
  "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Types.infer (Run.get_ast input)) expected

let compare_type_check (input: string) (expected: Types.typ): test = 
  "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Types.check (Run.get_ast input) expected) true


(* Tests *)

let tests: test list = [
  (* Compare AST outputs *)
  compare_ast "2" (Val (Num 2));
  compare_ast "2 + 3" (BinOp (Add, Val (Num 2), Val (Num 3)));
  compare_ast "2 - 3" (BinOp (Sub, Val (Num 2), Val (Num 3)));
  compare_ast "~(2 <= 3)" (UnOp (Not, BinOp (Leq, Val (Num 2), Val (Num 3))));
  compare_ast "(2 + 4) - (5 + 10)" (BinOp (Sub, (BinOp (Add, Val (Num 2), Val (Num 4))), (BinOp (Add, Val (Num 5), Val (Num 10)))));
  compare_ast "1-1 <= 2 + 3 + (~~4 && true)" (BinOp (Leq, BinOp (Sub, Val (Num 1), Val (Num 1)), BinOp (Add, BinOp (Add, Val (Num 2), Val (Num 3)), BinOp (And, UnOp (Not, UnOp (Not, Val (Num 4))), Val (Bool true)))));

  (* Compare AST outputs *)
  compare_eval "2" (Num 2);
  compare_eval "2 + 3" (Num 5);
  compare_eval "2 - 3" (Num (-1));
  compare_eval "~(2 <= 3)" (Bool false);
  compare_eval "(2 + 4) - (5 + 10)" (Num (-9));
  compare_eval "(1-1 <= (2 + 3)) && (~~false && true)" (Bool false);

  (* Infer types *)
  compare_inferred_types "2" Types.TNum;
  compare_inferred_types "2 + 3" Types.TNum;
  compare_inferred_types "2 - 3" Types.TNum;
  compare_inferred_types "~(2 <= 3)" Types.TBool;
  compare_inferred_types "(2 + 4) - (5 + 10)" Types.TNum;
  compare_inferred_types "(1-1 <= (2 + 3)) && (~~false && true)" Types.TBool;

  (* Type check *)
  compare_type_check "2" Types.TNum;
  compare_type_check "2 + 3" Types.TNum;
  compare_type_check "2 - 3" Types.TNum;
  compare_type_check "~(2 <= 3)" Types.TBool;
  compare_type_check "(2 + 4) - (5 + 10)" Types.TNum;
  compare_type_check "(1-1 <= (2 + 3)) && (~~false && true)" Types.TBool;
]

let suite: test =
  "langone tests" >::: tests

let () = run_test_tt_main suite
