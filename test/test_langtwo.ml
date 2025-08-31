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
  compare_ast "1" (Val (Num 2));
]

let suite: test =
  "langtwo tests" >::: tests

let () = run_test_tt_main suite
