(* 
  todo: f (x, y, z) is not allowed yet

  Other macros are included:
    - let
    - let (with multiple bindings)
    - operators: &&, ~, ==
    - named functions with multiple arguments
    - anonymous functions as basic constructs
*)


(* Plain Syntax  *)

(** Binary operators. *)
type binop =
  | Add
  | Sub
  | Leq

(** Values. *)
type value =
  | Num of int
  | Bool of bool

(** Expressions. *)
type expr =
  | Val of value
  | Var of string 
  | Fun of string * string * expr
  | FunAnon of string * expr
  | BinOp of binop * expr * expr
  | App of expr * expr
  | If of expr * expr * expr


(* Syntax with syntactic sugaring *)
(* Prefix S_ denotes AST that may include syntactic sugaring *)

(** Unary operator. *)
type s_unop =
  | S_Not  (* sugar *)

(** Binary operators. *)
type s_binop =
  | S_Add
  | S_Sub
  | S_Leq
  | S_And (* sugar *)
  | S_Equal (* sugar *)

(** Expressions. *)
type s_expr =
  | S_Val of value
  | S_Var of string 
  | S_BinOp of s_binop * s_expr * s_expr
  | S_Let of string * s_expr * s_expr  (* sugar *)
  | S_LetMany of (string * s_expr) list * s_expr  (* sugar *)
  | S_UnOp of s_unop * s_expr (* sugar *)
  | S_Fun of string * string * s_expr
  | S_FunAnon of string * s_expr
  | S_FunMany of string * (string list) * s_expr (* sugar *)
  | S_App of s_expr * s_expr
  (* | S_AppMany of (s_expr list) (* sugar *) *)
  | S_If of s_expr * s_expr * s_expr

let rec desugar (e: s_expr): expr =
    match e with
    | S_Val v -> Val v
    | S_Var x -> Var x
    | S_BinOp (op, e1, e2) -> 
        (match op with
        | S_Add -> BinOp (Add, desugar e1, desugar e2)
        | S_Sub -> BinOp (Sub, desugar e1, desugar e2)
        | S_Leq -> BinOp (Leq, desugar e1, desugar e2)
        | S_And -> 
          If (desugar e1, (If ( desugar e2, Val (Bool true), Val (Bool false))), (If (desugar e2, Val (Bool false), Val (Bool false))))
        | S_Equal -> 
          (* e1 <= e2 &&  e2 <= e1 *)
          let e_left = BinOp (Leq, desugar e1, desugar e2) in
          let e_right = BinOp (Leq, desugar e2, desugar e1) in
          If (e_left, (If (e_right, Val (Bool true), Val (Bool false))), (If (e_right, Val (Bool false), Val (Bool false)))))
    | S_UnOp (S_Not, e) ->
      If (desugar e, Val (Bool false), Val (Bool true))
    | S_Let (x, e1, e2) ->
      App ((FunAnon (x, (desugar e2))), (desugar e1))
    | S_LetMany (es, e_final) ->
      (* unfold lets into applications of anonymous functions *)
      let rec unfold_lets (es: (string * s_expr) list) (e_final':s_expr): expr =
       ( match es with
        | (x,e')::es' -> App ((FunAnon (x, (unfold_lets es' e_final))), desugar e')
        | [] -> desugar e_final')
      in
      unfold_lets es e_final
    | S_Fun (name, arg, body) ->
      Fun (name, arg, desugar body)
    | S_FunAnon (arg, body) ->
      FunAnon (arg, desugar body)
    | S_FunMany (f, xs, e') ->
      let rec unfold_fun (f: string) (xs: string list) e'' = 
        match xs with
        | x :: xs' -> FunAnon (x, unfold_fun f xs' e'')
        | [] -> desugar e''
        in
      unfold_fun f xs e'
    | S_App (e1, e2) ->
      App (desugar e1, desugar e2)
    (* | S_AppMany (es) ->
      let rec unfold_app (init: expr) es =
       ( match es with
        | e'::es' ->  unfold_app (App (init, desugar e')) es'
        | [] -> init
        )
      in
      (match es with
      | e1::e2s -> unfold_app (desugar e1) e2s
      | [] -> failwith "S_AppMany requires at least one argument") *)
    | S_If (e1, e2, e3) -> If (desugar e1, desugar e2, desugar e3)

(** Pretty prints a binary operator. *)
let string_of_binop (op: binop) =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Leq -> "<="

(** Pretty prints a value. *)
let string_of_val (v: value): string =
  match v with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b

(** [string_of_expr e] pretty prints an expression [e]. *)
let rec string_of_expr (e: expr) =
  match e with
  | Val v -> string_of_val v
  | Var x -> x 
  | BinOp (op, e1, e2) -> 
    Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_binop op) (string_of_expr e2)
  | If (e1, e2, e3) ->
    Printf.sprintf "if %s then (%s) else (%s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | App (e1, e2) ->
    Printf.sprintf "(%s %s)" (string_of_expr e1) (string_of_expr e2)
  | Fun (name, arg, body) ->
    Printf.sprintf "fun %s(%s){ %s }" name arg (string_of_expr body)
  | FunAnon (arg, body) ->
    Printf.sprintf "fun (%s){ %s }" arg (string_of_expr body)
