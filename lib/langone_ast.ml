(* type bop = 
  | Add
  | Mult
  | Leq

type expr =
  | Var of string
  | Int of int
  | Bool of bool  
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr *)

(* Operators *)
type binop =
  | Add
  | Sub
  | Leq
  | And

type unop =
  | Not  

type value =
  | Num of int
  | Bool of bool

(* Expressions. *)
type expr =
  | Val of value
  | BinOp of binop * expr * expr
  | UnOp of unop * expr