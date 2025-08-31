(* 
open Ast

(** [subst x v e] substitues name [s] by value [v] in expression [e] *)
let rec subst (x: string) (v: value) (e: expr): expr =
  match e with
  | Val _ -> e
  | Var y -> 
    if y = x then (* Match found: replace value *)  Val v else (* Leave as is *) e 
  | BinOp (op, e1, e2) -> BinOp (op, (subst x v e1), (subst x v e2))
  | UnOp (op, e) -> UnOp (op, (subst x v e))
  | Let (y, e1, e2) -> 
    let e3 = if x = y then e2 else (subst x v e2) in
      Let (y, (subst x v e1), e3)
  
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
  | Let(x, e1, e2) ->
      let v = eval e1 in
        eval (subst x v e2)
  | Var x -> failwith (Printf.sprintf "You can only evaluate closed terms; %s is free" x)
  | Val v -> v

(** [eval_verbose e] evaluates the given expression [e] and prints the evaluation result. It returns the resulting [value] after evaluation. *)
let eval_verbose (e: expr): Ast.value =
  let res = eval e in
  let () = Printf.printf "%s evaluates to %s\n" (Ast.string_of_expr e) (Ast.string_of_val res) in 
  res

(** [reduce e] performs a single-step reduction on the expression [e].

  @raise Failure if the expression cannot be reduced (i.e., it is a value or stuck). *)
let rec reduce (e: expr): expr =
  match e with
  | BinOp (Add, e1, e2) -> 
      (match (e1, e2) with
      | (Val (Num n1), Val (Num n2)) ->  Val (Num (n1 + n2))          (* rAdd1*)
      | (Val _, Val _) -> failwith ("Expression '" ^ (Ast.string_of_expr e) ^ "' stuck")
      | (Val v, e2)                  -> BinOp (Add, Val v, (reduce e2))   (* rAdd3*)   
      | _                            -> BinOp (Add, (reduce e1), e2)  (* rAdd2*)
      )  
  | BinOp (Sub, e1, e2) ->
      (match (e1, e2) with
      | (Val (Num n1), Val (Num n2)) ->  Val (Num (n1 - n2))          (* rSub1*)
      | (Val _, Val _) -> failwith ("Expression '" ^ (Ast.string_of_expr e) ^ "' stuck")
      | (Val v, e2)                  -> BinOp (Sub, Val v, (reduce e2))   (* rSub3*)   
      | _                            -> BinOp (Sub, (reduce e1), e2)  (* rSub2*)
      )
  | BinOp (Leq, e1, e2) -> 
    (match (e1, e2) with
      | (Val (Num n1), Val (Num n2)) ->  Val (Bool (n1 <= n2))          (* rLeq1*)
      | (Val _, Val _) -> failwith ("Expression '" ^ (Ast.string_of_expr e) ^ "' stuck")
      | (Val v, e2)                  -> BinOp (Leq, Val v, (reduce e2))   (* rLeq3*)   
      | _                            -> BinOp (Leq, (reduce e1), e2)  (* rLeq2*)
      )
  | BinOp (And, e1, e2) ->
    (match (e1, e2) with
      | (Val (Bool b1), Val (Bool b2)) ->  Val (Bool (b1 && b2))          (* rAnd1*)
      | (Val _, Val _) -> failwith ("Expression '" ^ (Ast.string_of_expr e) ^ "' stuck")
      | (Val v, e2)                  -> BinOp (And, Val v, (reduce e2))   (* rAnd3*)   
      | _                            -> BinOp (And, (reduce e1), e2)  (* rAnd2*)
      )
  | UnOp (Not, e) ->
    (match e with
    | Val (Bool b) -> Val (Bool (not b))
    | Val _ -> failwith ("Expression '" ^ (Ast.string_of_expr e) ^ "' stuck")
    | _ -> UnOp (Not, reduce e)
    )
  | Let (x, e1, e2) ->
    (match (e1, e2) with
    | (Val v, e2) -> subst x v e2
    | (e1, e2) -> Let (x, reduce e1, e2)
    )
  | Var x -> failwith (Printf.sprintf "You can only reduce closed terms; %s is free" x)
  | Val _ -> failwith ("Value '" ^ (Ast.string_of_expr e) ^ "' does not reduce")

(** [reduce_verbose e] reduces an expression [e] by one step, prints the reduction, and returns the resulting expression. *)
let reduce_verbose (e: expr): expr =
  let res = reduce e in
    let () = Printf.printf "%s reduces to %s\n" (Ast.string_of_expr e) (Ast.string_of_expr res) in 
    res

(** [reduce_all e] reduces expression e, step by step, into a final [value] and returns it as an [expression]. *)
let rec reduce_all (e: expr): expr =
  match e with
  | Val _ -> e
  | _ -> (reduce e) |> reduce_all 

(** [reduce_all_verbose e] repeatedly reduces the given expression [e] step by step, printing the expression at each step, until it reaches a [value]. Returns the final value as an [expression].*)
let rec reduce_all_verbose (e: expr): expr =
  match e with
  | Val _ -> (Printf.printf "%s\n" (Ast.string_of_expr e)); e
  | _ -> (Printf.printf "%s\n" (Ast.string_of_expr e));
         (reduce e) 
          |> reduce_all_verbose 


(** [rename x e_new e] substitute variable name [x] by expression [e_new] (may be a variable) in expression [e]. *)
let rec rename (x: string) (e_new: expr) (e: expr): expr =
  match e with
  | Val _ -> e
  | Var y -> 
    if y = x then (* Match found: replace value *) e_new else (* Leave as is *) e 
  | BinOp (op, e1, e2) -> BinOp (op, (rename x e_new e1), (rename x e_new e2))
  | UnOp (op, e) -> UnOp (op, (rename x e_new e))
  | Let (y, e1, e2) -> 
    let e3 = if x = y then e2 else (rename x e_new e2) in
      Let (y, (rename x e_new e1), e3)
  

(** [alpha_equiv e_left e_right] checks if two expressions [e_left] and [e_right] are alpha-equivalent. *)
let alpha_equiv (e_left: expr) (e_right: expr): bool =
  let rec alpha_equiv_internal (e_left: expr) (e_right: expr) (counter: int): bool =
  match (e_left, e_right) with
  | (Val v1, Val v2) -> v1 = v2
  | (Var x1, Var x2) -> x1 = x2
  | (BinOp (op1, e1, e2), BinOp (op2, e3, e4)) -> 
    (op1 = op2) && (alpha_equiv_internal e1 e3 counter) && (alpha_equiv_internal e2 e4 counter)
  | (UnOp (op1, e1), UnOp (op2, e2)) ->
    (op1 = op2) && (alpha_equiv_internal e1 e2 counter)
  | (Let (x, e1, e2), Let (y, e3, e4)) ->
    let z = Var ("$" ^ string_of_int counter) in (* unique name, not free in e2 or e4 *)
    let e1_updated = rename x z e2  in
    let e4_updated = rename y z e4  in
      alpha_equiv_internal e1 e3 counter && alpha_equiv_internal e1_updated e4_updated (counter + 1)
  | _ -> false
  in
    alpha_equiv_internal e_left e_right 0

(** [alpha_equiv_verbose e_left e_right] checks if two expressions [e_left] and [e_right] are alpha-equivalent, prints the details.*)
let alpha_equiv_verbose (e_left: expr) (e_right: expr): bool =
  let res = alpha_equiv e_left e_right in
    let () = Printf.printf "Are %s and %s alpha-equivalent? %s\n" (Ast.string_of_expr e_left) (Ast.string_of_expr e_right) (string_of_bool res) in 
    res
     *)