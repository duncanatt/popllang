(* module Ast = Langone_ast *)

type typ =
  | TNum
  | TBool

let rec infer (e: Ast.expr): typ =
  match e with
  | BinOp (Add, e1, e2) -> 
    (match (infer e1, infer e2) with
    | (TNum, TNum) -> TNum
    | (_, _) -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | BinOp (Sub, e1, e2) -> 
    (match (infer e1, infer e2) with
    | (TNum, TNum) -> TNum
    | (_, _) -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | BinOp (Leq, e1, e2) -> 
    (match (infer e1, infer e2) with
    | (TNum, TNum) -> TBool
    | (_, _) -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | BinOp (And, e1, e2) -> 
    (match (infer e1, infer e2) with
    | (TBool, TBool) -> TBool
    | (_, _) -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | UnOp (Not, e) ->
    (match (infer e) with
    | TBool -> TBool
    | _ -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | Val (Num _) -> TNum
  | Val (Bool _) -> TBool

let rec check (e: Ast.expr) (t: typ): bool = 
  match (e, t) with
  | (BinOp (Add, e1, e2), TNum) -> 
    (check e1 TNum) && (check e2 TNum)
  | (BinOp (Sub, e1, e2), TNum) -> 
    (check e1 TNum) && (check e2 TNum)
  | (BinOp (Leq, e1, e2), TBool) -> 
    (check e1 TNum) && (check e2 TNum)
  | (BinOp (And, e1, e2), TBool) -> 
    (check e1 TBool) && (check e2 TBool)
  | (UnOp (Not, e), TBool) ->
    check e TBool
  | (Val (Num _), TNum) -> true
  | (Val (Bool _), TBool) -> true
  | _ -> false

let string_of_typ : typ -> string = function
  | TNum -> "num"
  | TBool -> "bool"