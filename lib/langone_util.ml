module Ast = Langone_ast

(* Pretty prints an operator. *)
let string_of_binop (op: Ast.binop) =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Leq -> "<="
  | And -> "&&"

let string_of_unop (op: Ast.unop) =
  match op with
  | Not -> "~"

let string_of_val (v: Ast.value): string =
  match v with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b

(* Pretty prints an expression. *)
let rec string_of_ast (e: Ast.expr) =
  match e with
  | Val v -> string_of_val v
  | BinOp (op, e1, e2) -> 
    Printf.sprintf "(%s %s %s)" (string_of_ast e1) (string_of_binop op) (string_of_ast e2)
  | UnOp (op, e) ->
    Printf.sprintf "%s%s" (string_of_unop op) (string_of_ast e)