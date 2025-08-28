
(* Binary operators. *)
type binop =
  | Add
  | Sub
  | Leq
  | And

(* Unary operator. *)
type unop =
  | Not  

(* Values. *)
type value =
  | Num of int
  | Bool of bool

(* Expressions. *)
type expr =
  | Val of value
  | Var of string 
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Let of string * expr * expr

(* Pretty prints a binary operator. *)
let string_of_binop (op: binop) =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Leq -> "<="
  | And -> "&&"

(* Pretty prints a unary operator. *)
let string_of_unop (op: unop) =
  match op with
  | Not -> "~"

(* Pretty prints a value. *)
let string_of_val (v: value): string =
  match v with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b

(* Pretty prints an expression. *)
let rec string_of_expr (e: expr) =
  match e with
  | Val v -> string_of_val v
  | Var x -> x 
  | BinOp (op, e1, e2) -> 
    Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_binop op) (string_of_expr e2)
  | UnOp (op, e) ->
    Printf.sprintf "%s%s" (string_of_unop op) (string_of_expr e)
  | Let (x, e1, e2) -> 
    Printf.sprintf "let %s = %s in %s" x (string_of_expr e1) (string_of_expr e2)

