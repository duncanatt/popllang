let () =  
  let open Langone in
    let _ast = Run.get_ast 
    (* "2" *)
      (* "1-1 <= 2 + 3 + (~~4 && true)"  *)
      (* "~(1 + 2 + 3 <= 5 - 6) && false" *)
      "(2 + 4) - (5 + 10)"
    in
      (* let _ = Sem.eval_verbose ast in *)
      (* let _ = Sem.reduce_verbose ast in *)
      (* let _ = Types.infer_verbose ast in *)
      (* let _ = Sem.reduce_all_verbose ast in *)
      (* let _ = Types.check_verbose ast Types.TNum in *)
      ()

let _langoneext =  
  let open Langoneext in
    let _ast = Run.get_ast 
      (* "let x = (let y = 5 in y) in x + x" *)
      "let x = let y = 5 in y in x + x"
      (* "let x = 5 in let y = 6 in y + x" *)
      (* "let x = true in let x = 5 in x" *)
      (* "(1 + (2 + 5)) + (3 + 4)" *)

    in
    (* let _ = Sem.eval_verbose ast in *)
    (* let _ = Sem.reduce_verbose ast in *)
    (* let _ = Sem.alpha_equiv_verbose ast (Run.get_ast "x + 1") in *)
    (* let _ = Types.infer_verbose ast Types.empty_env in *)
    (* let _ = Sem.reduce_all_verbose ast in *)
    (* let _ = Types.check_verbose ast Types.TNum Types.empty_env in *)
    ()

let _langtwo =  
  let open Langtwo in
    let _ast = Run.get_ast_verbose 
      (* "x && false" *)
      (* "f a b" *)
      (* "let x = 1, y = 2 in (f x y)" *)
      (* "if true then a else false f" *)
      (* "f (if true then a else b) y" *)
      (* "fun(x){x + 1} 3" *)
      (* "if true then (fun f(x){3}) s else 2" *)
      (* "if true then fun(x){ fun f(y){y (a b c + 2)} yy } 4 else true"  *)
      (* "if true then fun(x){x} 4 else true"   *)
       (* "if x then f y else a + b - c" *)
      (* "(3 + a) (b)" *)
      (* "a b c" *)
      "let x = 1
        in let f = fun(y){y + x} 
          in let x = 2
            in f 3"
    in
    ()

let _langthree =  
  let open Langthree in
    let _ast = Run.get_ast

    (* Double value in location l1 *)
    (* "{l1 -> 4} l1 := !l1 + !l1" *)

    (* Example 61 & 66 & 67, Swapping Values *)
    (* "{l1 -> 5, l2 -> 3, l3 -> 0} l3 := !l1; l1 := !l2; l2 := !l3; l3 := 0" *)

    (* Example 62, Sum *)
    (* "{l1 -> 5, l2 -> 0} l2 := 0; while (1 <= !l1) do (l2 := !l1 + !l2; l1 := !l1 - 1) " *)

    (* Example 63, Aliasing *)
    (* "{l1 -> l2, l2 -> 5, l3 -> l2} !l1 := 0; l2 := 1; l3 := !(!l1)" *)

    (* Example 63, Aliasing, v2 *)
    (* "{l1 -> l4, l2 -> 5, l3 -> 0, l4 -> 3} !l1 := 0; l2 := 1; l3 := !(!l1)" *)
    
    (* "skip; skip"  *)
    (* "{l1 -> 3} l1 := 2; skip" *)
    (* "{l1 -> 3, l2 -> 4} l1 := !l2; skip"  *)
    (* "{l1 -> 3, l2 -> 4} while (!l1) <= !l2 do (l1 := !l1 + 1; l2 := !l2 - 1)"   *)
    (* "while true do skip" *)

    (* Ill behaved examples *)
    (* "{l1 -> true} l1 := 5 + !l1" *)
    "{l1 -> l2} l1 := 5 + !(!l2)"
    (* "{l1 -> true, l2 -> 0} while 1 <= !l1 do (l2 := 4 + !l2; l1 := !l1 - 1)" *)
    (* "{l1 -> 3, l2 -> 0, l3 -> 0} l2 := 2 + !l1; l1 := true; l3 := 2 + !l1" *)
    in
    (* let _ = Sem.eval_verbose ast in *)
    (* let _ = Sem.reduce_verbose ast in *)
    (* let _ = Sem.reduce_all_verbose ast in *)
    (* let _ = Types.typecheck_verbose ast in *)
    ()





    (* proper documentation... *)

(** [parse s] parses [s] into an AST. *)
(* let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Popllang.Langone.Parser.prog Popllang.Langone.Lexer.read lexbuf in
  ast *)

(** [eval_big e] is the [e ==> v] relation. *)
(* let rec eval_big (e : expr) : expr = match e with
  | Int _ | Bool _ -> e
  | Var _ -> failwith unbound_var_err
  | Binop (bop, e1, e2) -> eval_bop bop e1 e2
  | Let (x, e1, e2) -> subst e2 (eval_big e1) x |> eval_big
  | If (e1, e2, e3) -> eval_if e1 e2 e3 *)
