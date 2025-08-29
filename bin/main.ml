(* let () =  
  let open Langone in
    let ast = Run.get_ast 
    (* "2" *)
      (* "1-1 <= 2 + 3 + (~~4 && true)"  *)
      (* "~(1 + 2 + 3 <= 5 - 6) && false" *)
      "(2 + 4) - (5 + 10)"
    in
      let _ = Sem.eval_verbose ast in
      let _ = Sem.reduce_verbose ast in
      let _ = Types.infer_verbose ast in
      let _ = Sem.reduce_all_verbose ast in
      let _ = Types.check_verbose ast Types.TNum in
      () *)

let _langoneext =  
  let open Langoneext in
    let ast = Run.get_ast 
      (* "let x = (let y = 5 in y) in x + x" *)
      "let x = let y = 5 in y in x + x"
      (* "let x = 5 in let y = 6 in y + x" *)
      (* "let x = true in let x = 5 in x" *)
      (* "(1 + (2 + 5)) + (3 + 4)" *)

    in
    let _ = Sem.eval_verbose ast in
    (* let _ = Sem.reduce_verbose ast in *)
    (* let _ = Sem.alpha_equiv_verbose ast (Run.get_ast "x + 1") in *)
    (* let _ = Types.infer_verbose ast Types.empty_env in *)
    (* let _ = Sem.reduce_all_verbose ast in *)
    (* let _ = Types.check_verbose ast Types.TNum Types.empty_env in *)
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
