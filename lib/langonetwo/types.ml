(* 
(** Data types. *)
type typ =
  | TNum
  | TBool

(** Define map from variable names to types. *)
module Env = Map.Make(String)

(* Type synonym for environment with [typ] values. *)
type typenv = typ Env.t

(** [empty_env] creates an empty type environment. *)
let empty_env: typenv = Env.empty

(** [extend x t env] extends the type environment [env] by string [x] mapped to type [t], i.e. [x => t]. *)
let extend (x: String.t) (t: typ) (env: typenv): typenv =
  Env.add x t env 

(** [lookup x env] returns the corresponding type mapped by value [x]. *)
let lookup (x: string) (env: typenv): typ option =
  Env.find_opt x env

(** [string_of_env env t] pretty prints the types. *)
let string_of_typ: typ -> string = function
  | TBool -> "bool"
  | TNum -> "num"

(** [string_of_env env] pretty prints the typing environment [env]. *)
let string_of_env (env: typenv) =
  env 
  |> Env.bindings 
  |> List.map (fun (x, t) -> x ^ " : " ^ string_of_typ t)
  |> String.concat ", "

(** [infer e env] infers the type of the expression [e] under the type environment [env]. It returns the type [typ] that [e] is assigned to, or raises an error using [failwith] if the expression is not well-typed in the given environment.

  [check e t env] checks whether the expression [e] has type [t] under the type environment [env]. It returns [true] if [e] has type [t], and [false] otherwise.
   
  @raise Failure if the expression is not well-typed. *)
let rec infer (e: Ast.expr) (env: typenv): typ =
  match e with
  | BinOp (Add, e1, e2) -> 
    (match (infer e1 env, infer e2 env) with
    | (TNum, TNum) -> TNum
    | (_, _) -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | BinOp (Sub, e1, e2) -> 
    (match (infer e1 env, infer e2 env) with
    | (TNum, TNum) -> TNum
    | (_, _) -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | BinOp (Leq, e1, e2) -> 
    (match (infer e1 env, infer e2 env) with
    | (TNum, TNum) -> TBool
    | (_, _) -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | BinOp (And, e1, e2) -> 
    (match (infer e1 env, infer e2 env) with
    | (TBool, TBool) -> TBool
    | (_, _) -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | UnOp (Not, e) ->
    (match (infer e env) with
    | TBool -> TBool
    | _ -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | Let (x, e1, e2) ->
    let t1 = infer e1 env in
      let env2 = extend x t1 env in
        infer e2 env2
  | Var x ->
    (match lookup x env with 
      | None -> failwith ("Cannot infer type for undefined variable " ^ x)
      | Some t -> t)
  | Val (Num _) -> TNum
  | Val (Bool _) -> TBool

  and check (e: Ast.expr) (t: typ) (env: typenv): bool = 
  (* let rec check (e: Ast.expr) (t: typ) (env: typenv): bool =  *)
    match e with
    | BinOp (Add, e1, e2) -> 
        if t = TNum then
          (check e1 TNum env) && (check e2 TNum env)
        else
          false
    | BinOp (Sub, e1, e2) -> 
        if t = TNum then
          (check e1 TNum env) && (check e2 TNum env)
        else 
          false
    | BinOp (Leq, e1, e2) -> 
      if t = TBool then
        (check e1 TNum env) && (check e2 TNum env)
      else
        false
    | BinOp (And, e1, e2) -> 
      if t = TBool then
        (check e1 TBool env) && (check e2 TBool env)
      else
        false
    | UnOp (Not, e) ->
      if t = TBool then
        check e TBool env
      else
        false
    | Let (x, e1, e2) ->
        let t1 = infer e1 env in
          let env2 = extend x t1 env in
            check e2 t env2
    | Var x ->
      (match lookup x env with 
      | None -> failwith ("Cannot infer type for undefined variable " ^ x)
      | Some t1 -> t1 = t)
    | Val (Num _) -> t = TNum 
    | Val (Bool _) -> t = TBool


(** [infer_verbose e] infers the type for expression [e], printing verbose output. *)
let infer_verbose (e: Ast.expr) (env: typenv): typ =
  let res = infer e env in
    let () = Printf.printf "%s has type %s\n" (Ast.string_of_expr e) (string_of_typ res ) in 
    res

(** [typecheck_verbose c] type checks program [c] against type [t], printing verbose output. *)
let check_verbose (e: Ast.expr) (t: typ) (env: typenv): bool =
  let res = check e t env in
    let () = Printf.printf "Is %s of type %s? %s\n" (Ast.string_of_expr e) (string_of_typ t) (string_of_bool res) in 
    res *)
