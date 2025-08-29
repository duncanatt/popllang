open OUnit2

open Langthree
open Langthree.Ast
(* open Langthree.Sem *)
(* open Langthree.Types *)
(* open Langthree.Lexer *)
(* open Langthree.Parser *)

(* Helper functions *)
 let compare_ast (input: string) (expected: Ast.comm): test = 
   "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Run.get_ast input) expected
(*    
let compare_eval (input: string) (expected: Ast.value): test = 
  "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Sem.eval (Run.get_ast input)) expected

let compare_inferred_types (input: string) (expected: Types.typ): test = 
  "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Types.infer (Run.get_ast input) Langoneext.Types.empty_env) expected

let compare_type_check (input: string) (expected: Types.typ): test = 
  "[" ^ input ^ "]" >:: fun _ ->  assert_equal (Types.check (Run.get_ast input) expected Langoneext.Types.empty_env) true

let compare_alpha_equivalence (input1: string) (input2: string) (expected: bool): test = 
  "[" ^ input1 ^ " , " ^ input2 ^ "]" >:: fun _ ->  assert_equal (Sem.alpha_equiv (Run.get_ast input1) (Run.get_ast input2)) expected *)

(* Tests *)

let tests: test list = [
  (* Compare AST outputs *)
  compare_ast "2 := 1" (Assign ((Val (Num 2)), (Val (Num 1))));
  compare_ast "2 + 3 := 1" (Assign (BinOp (Add, Val (Num 2), Val (Num 3)), Val (Num 1)));
  compare_ast "2 - 3 := 1" (Assign (BinOp (Sub, Val (Num 2), Val (Num 3)), Val (Num 1)));
  compare_ast "~(2 <= 3) := 1" (Assign (UnOp (Not, BinOp (Leq, Val (Num 2), Val (Num 3))), Val (Num 1)));
  compare_ast "(2 + 4) - (5 + 10) := 2" (Assign (BinOp (Sub, (BinOp (Add, Val (Num 2), Val (Num 4))), (BinOp (Add, Val (Num 5), Val (Num 10)))), Val (Num 2)));
  compare_ast "1-1 <= 2 + 3 + (~~4 && true) := 2" (Assign (BinOp (Leq, BinOp (Sub, Val (Num 1), Val (Num 1)), BinOp (Add, BinOp (Add, Val (Num 2), Val (Num 3)), BinOp (And, UnOp (Not, UnOp (Not, Val (Num 4))), Val (Bool true)))), Val (Num 2)));
  compare_ast "while 3 + 2 do skip" (While (BinOp (Add, Val (Num 3), Val (Num 2)), Skip));
  compare_ast "l3 := !l1; l1 := !l2; l2 := !l3; l3 := 0" (Seq (Seq(Seq(Assign (Val (Loc "l3"), UnOp (DeRef, Val (Loc "l1"))), Assign (Val (Loc "l1"), UnOp (DeRef, Val (Loc "l2")))), Assign (Val (Loc "l2"), UnOp (DeRef, Val (Loc "l3")))), Assign (Val (Loc "l3"), Val (Num 0))));
  
  (* 
  
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

  (* Alpha Equivalence *)
  compare_alpha_equivalence "let x = 5 in (let y = 3 in x + 1)" "let y = 5 in (let x = 3 in y + 1)" true;
  compare_alpha_equivalence "let x = 5 in (let y = 3 in x + 1)" "let x = 5 in (let y = 3 in y + 1)" false;
  compare_alpha_equivalence "let x = 5 in (let y = 3 in x + 1)" "let x = 5 in (let x = 3 in x + 1)" false;
  compare_alpha_equivalence "5" "5" true;
  compare_alpha_equivalence "2" "6" false;
  compare_alpha_equivalence "x" "x" true;
  compare_alpha_equivalence "x" "y" false; *)

]

let suite: test =
  "langthree tests" >::: tests

let () = run_test_tt_main suite
