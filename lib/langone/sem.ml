open Ast

(** [eval e s] evaluates the expression [e] returning a value. *)
let rec eval (e: expr): value =
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

(** [eval_verbose e] evaluates the given expression [e] and prints the evaluation result. It returns the resulting [value] after evaluation. *)
let eval_verbose (e: expr): value =
  let res = eval e in
  let () = Printf.printf "%s evaluates to %s\n" (string_of_expr e) (string_of_val res) in 
  res

(** [reduce e] performs a single-step reduction on the expression [e].

  @raise Failure if the expression cannot be reduced (i.e., it is a value or stuck). *)
let rec reduce (e: expr): expr =
  match e with
  | BinOp (Add, e1, e2) -> 
      (match (e1, e2) with
      | (Val (Num n1), Val (Num n2)) ->  Val (Num (n1 + n2))          (* rAdd1*)
      | (Val _, Val _) -> failwith ("Expression '" ^ (string_of_expr e) ^ "' stuck")
      | (Val v, e2)                  -> BinOp (Add, Val v, (reduce e2))   (* rAdd3*)   
      | _                            -> BinOp (Add, (reduce e1), e2)  (* rAdd2*)
      )  
  | BinOp (Sub, e1, e2) -> 
      (match (e1, e2) with
      | (Val (Num n1), Val (Num n2)) ->  Val (Num (n1 - n2))          (* rSub1*)
      | (Val _, Val _) -> failwith ("Expression '" ^ (string_of_expr e) ^ "' stuck")
      | (Val v, e2)                  -> BinOp (Sub, Val v, (reduce e2))   (* rSub3*)   
      | _                            -> BinOp (Sub, (reduce e1), e2)  (* rSub2*)
      )
  | BinOp (Leq, e1, e2) -> 
    (match (e1, e2) with
      | (Val (Num n1), Val (Num n2)) ->  Val (Bool (n1 <= n2))          (* rLeq1*)
      | (Val _, Val _) -> failwith ("Expression '" ^ (string_of_expr e) ^ "' stuck")
      | (Val v, e2)                  -> BinOp (Leq, Val v, (reduce e2))   (* rLeq3*)   
      | _                            -> BinOp (Leq, (reduce e1), e2)  (* rLeq2*)
      )
  | BinOp (And, e1, e2) -> 
    (match (e1, e2) with
      | (Val (Bool b1), Val (Bool b2)) ->  Val (Bool (b1 && b2))          (* rAnd1*)
      | (Val _, Val _) -> failwith ("Expression '" ^ (string_of_expr e) ^ "' stuck")
      | (Val v, e2)                  -> BinOp (And, Val v, (reduce e2))   (* rAnd3*)   
      | _                            -> BinOp (And, (reduce e1), e2)  (* rAnd2*)
      )
  | UnOp (Not, e) ->
      (match e with
      | Val (Bool b) -> Val (Bool (not b))
      | Val _ -> failwith ("Expression '" ^ (string_of_expr e) ^ "' stuck")
      | _ -> UnOp (Not, reduce e)
      )
  | Val _ -> 
    failwith ("Value '" ^ (string_of_expr e) ^ "' does not reduce")

(** [reduce_verbose e] reduces an expression [e] by one step, prints the reduction, and returns the resulting expression. *)
let reduce_verbose (e: expr): expr =
  let res = reduce e in
    let () = Printf.printf "%s reduces to %s\n" (string_of_expr e) (string_of_expr res) in 
    res

(** [reduce_all e] reduces expression e, step by step, into a final [value] and returns it as an [expression]. *)
let rec reduce_all (e: expr): expr =
  match e with
  | Val _ -> e
  | _ -> (reduce e) |> reduce_all 

(** [reduce_all_verbose e] repeatedly reduces the given expression [e] step by step, printing the expression at each step, until it reaches a [value]. Returns the final value as an [expression].*)
let rec reduce_all_verbose (e: expr): expr =
  match e with
  | Val _ -> (Printf.printf "%s\n" (string_of_expr e)); e
  | _ -> (Printf.printf "%s\n" (string_of_expr e));
         (reduce e) 
          |> reduce_all_verbose 