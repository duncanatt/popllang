(* module Ast = Langoneext_ast *)

(* Data types. *)
type typ =
  | TNum
  | TBool

(* Define map from variable names to types. *)
module Env = Map.Make(String)

(* Type synonym for environment with typ values. *)
type typenv = typ Env.t

(* Returns empty type environment. *)
let empty: typenv = Env.empty

(* Extends the type environment by the mapping value -> type. *)
let extend (x: String.t) (t: typ) (env: typenv): typenv =
  Env.add x t env 

(* Returns the corresponding type for mapped by value. *)
let lookup (x: string) (env: typenv): typ option =
  Env.find_opt x env

(* Returns the stringified value of a type. *)
let string_of_typ: typ -> string = function
  | TBool -> "bool"
  | TNum -> "num"

let string_of_env (env: typenv) =
  env 
  |> Env.bindings 
  |> List.map (fun (x, t) -> x ^ " : " ^ string_of_typ t)
  |> String.concat ", "

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
    