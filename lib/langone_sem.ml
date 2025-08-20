module Ast = Langone_ast
module Util = Langone_util

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
  | Val v -> v

let rec reduce (e: Ast.expr): Ast.expr =
  match e with
  | BinOp (Add, e1, e2) -> 
      (match (e1, e2) with
      | (Val (Num n1), Val (Num n2)) ->  Val (Num (n1 + n2))          (* rAdd1*)
      | (Val _, Val _) -> failwith ("Expression '" ^ (Util.string_of_ast e) ^ "' stuck")
      | (Val v, e2)                  -> BinOp (Add, Val v, (reduce e2))   (* rAdd3*)   
      | _                            -> BinOp (Add, (reduce e1), e2)  (* rAdd2*)
      )  
  | BinOp (Sub, e1, e2) -> 
      (match (e1, e2) with
      | (Val (Num n1), Val (Num n2)) ->  Val (Num (n1 - n2))          (* rSub1*)
      | (Val _, Val _) -> failwith ("Expression '" ^ (Util.string_of_ast e) ^ "' stuck")
      | (Val v, e2)                  -> BinOp (Sub, Val v, (reduce e2))   (* rSub3*)   
      | _                            -> BinOp (Sub, (reduce e1), e2)  (* rSub2*)
      )
  | BinOp (Leq, e1, e2) -> 
    (match (e1, e2) with
      | (Val (Num n1), Val (Num n2)) ->  Val (Bool (n1 <= n2))          (* rLeq1*)
      | (Val _, Val _) -> failwith ("Expression '" ^ (Util.string_of_ast e) ^ "' stuck")
      | (Val v, e2)                  -> BinOp (Leq, Val v, (reduce e2))   (* rLeq3*)   
      | _                            -> BinOp (Leq, (reduce e1), e2)  (* rLeq2*)
      )
  | BinOp (And, e1, e2) -> 
    (match (e1, e2) with
      | (Val (Bool b1), Val (Bool b2)) ->  Val (Bool (b1 && b2))          (* rAnd1*)
      | (Val _, Val _) -> failwith ("Expression '" ^ (Util.string_of_ast e) ^ "' stuck")
      | (Val v, e2)                  -> BinOp (And, Val v, (reduce e2))   (* rAnd3*)   
      | _                            -> BinOp (And, (reduce e1), e2)  (* rAnd2*)
      )
  | UnOp (Not, e) ->
      (match e with
      | Val (Bool b) -> Val (Bool (not b))
      | Val _ -> failwith ("Expression '" ^ (Util.string_of_ast e) ^ "' stuck")
      | _ -> UnOp (Not, reduce e)
      )
  | Val _ -> 
    failwith ("Value '" ^ (Util.string_of_ast e) ^ "' does not reduce")

let rec reduce_all (e: Ast.expr): Ast.expr =
  match e with
  | Val _ -> e
  | _ -> (reduce e) |> reduce_all 
