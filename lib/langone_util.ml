(* module Ast = Langone.Ast *)
module Ast = Langone_ast

let string_of_bop (bop: Ast.bop) = 
  match bop with
  | Add -> "+"
  | Mult -> "+"
  | Leq -> "+"

let rec string_of_ast (a: Ast.expr) = 
  match a with
  | Var var -> var
  | Int int -> string_of_int int
  | Bool bool -> string_of_bool bool
  | Binop (bop, expr1, expr2) -> 
      Printf.sprintf "%s %s %s" (string_of_ast expr1) (string_of_bop bop) (string_of_ast expr2)
  | Let (var, expr1, expr2) ->
      Printf.sprintf "let %s = %s in %s" var (string_of_ast expr1) (string_of_ast expr2)
  | If (expr1, expr2, expr3) ->
      Printf.sprintf "if %s then %s else %s" (string_of_ast expr1) (string_of_ast expr2) (string_of_ast expr3)


