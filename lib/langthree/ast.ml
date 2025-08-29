
(* Binary operators. *)
type binop =
  | Add
  | Sub
  | Leq
  | And

(* Unary operator. *)
type unop =
  | Not
  | DeRef

(* Values. *)
type value =
  | Num of int
  | Bool of bool
  | Loc of string

(* Expressions. *)
type expr =
| Val of value
| BinOp of binop * expr * expr
| UnOp of unop * expr

(* Commands *)
type comm =
| Skip
| Seq of comm * comm
| Assign of expr * expr
| While of expr * comm
| If of expr * comm * comm

(* Pretty prints a binary operator. *)
let string_of_binop (op: binop): string =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Leq -> "<="
  | And -> "&&"

(* Pretty prints a unary operator. *)
let string_of_unop (op: unop): string =
  match op with
  | Not -> "~"
  | DeRef -> "!"

(* Pretty prints a value. *)
let string_of_val (v: value): string =
  match v with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Loc l -> l

(* Pretty prints an expression. *)
let rec string_of_expr (e: expr): string =
  match e with
  | Val v -> string_of_val v
  | BinOp (op, e1, e2) -> 
    Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_binop op) (string_of_expr e2)
  | UnOp (op, e) ->
    Printf.sprintf "%s%s" (string_of_unop op) (string_of_expr e)

let rec string_of_comm (c: comm): string =
  match c with
  | Skip -> "skip"
  | Seq (c1, c2) ->
    Printf.sprintf "%s; %s" (string_of_comm c1) (string_of_comm c2)
  | Assign (e1, e2) ->
    Printf.sprintf "%s := %s" (string_of_expr e1) (string_of_expr e2)
  | While (e, c) ->
    Printf.sprintf "while %s do %s" (string_of_expr e) (string_of_comm c)
  | If (e, c1, c2) ->
    Printf.sprintf "if %s then %s else %s" (string_of_expr e) (string_of_comm c1) (string_of_comm c2)

