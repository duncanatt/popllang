(** Data types. *)
type typ =
  | TNum
  | TBool

(** [string_of_env env t] pretty prints the types. *)
let string_of_typ : typ -> string = function
  | TNum -> "num"
  | TBool -> "bool"

(** [infer e] infers the type of the expression [e]. It returns the type [typ] that [e] is assigned to, or raises an error using [failwith] if the expression is not well-typed.

  @raise Failure if the expression is not well-typed. *)
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

(** [infer_verbose e] infers the type for expression [e], printing verbose output. *)
let infer_verbose (e: Ast.expr): typ =
  let res = infer e in
    let () = Printf.printf "%s has type %s\n" (Ast.string_of_expr e) (string_of_typ res) in 
    res

(** [check e t] checks whether the expression [e] has type [t]. It returns [true] if [e] has type [t], and [false] otherwise.

  @raise Failure if the expression is not well-typed compared to [t]. *)
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

(** [check_verbose c] type checks program [c] against type [t], printing verbose output. *)
let check_verbose (e: Ast.expr) (t: typ): bool =
  let res = check e t in
    let () = Printf.printf "Is %s of type %s? %s\n" (Ast.string_of_expr e) (string_of_typ t) (string_of_bool res) in 
    res