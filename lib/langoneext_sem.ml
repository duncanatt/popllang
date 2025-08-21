module Ast = Langoneext_ast
open Ast

let rec subst (x: string) (v: value) (e: expr): expr =
  match e with
  | Val _ -> e
  | Var y -> 
    if y = x then (* Match found: replace value *)  Val v else (* Leave as is *) e 
  | BinOp (op, e1, e2) -> BinOp (op, (subst x v e1), (subst x v e2))
  | UnOp (op, e) -> UnOp (op, (subst x v e))
  | Let (y, e1, e2) -> 
    Printf.printf "Matched LET. Let var=%s\n" y;
    let e3 = if x = y then e2 else (subst x v e2) in
      Let (y, (subst x v e1), e3)
  
let rec eval (e: Ast.expr): Ast.value =
  match e with
  | BinOp (Add, e1, e2) -> 
    (match (eval e1, eval e2) with
    | (Num n1, Num n2) -> Num (n1 + n2)
    | (_, _) -> failwith "You can only add two numerals")
  | BinOp (Sub, e1, e2) -> 
    (match (eval e1, eval e2) with
    | (Num n1, Num n2) -> Num (n1 - n2)
    | (_, _) -> failwith "You can only subtract two numerals")
  | BinOp (Leq, e1, e2) -> 
    (match (eval e1, eval e2) with
    | (Num n1, Num n2) -> Bool (n1 <= n2)
    | (_, _) -> failwith "You can only compare two numerals")
  | BinOp (And, e1, e2) -> 
    (match (eval e1, eval e2) with
    | (Bool b1, Bool b2) -> Bool (b1 && b2)
    | (_, _) -> failwith "You can only compare two numerals")
  | UnOp (Not, e) ->
    (match (eval e) with
    | Bool b -> Bool (not b)
    | _ -> failwith "You can only negate booleans")
  | Let(x, e1, e2) ->
      let v = eval e1 in
        eval (subst x v e2)
  | Var x -> failwith (Printf.sprintf "You can only evaluate closed terms; %s is open" x)
  | Val v -> v