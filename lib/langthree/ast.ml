(* Binary operators. *)
type binop =
  | Add
  | Sub
  | Leq
  | And

(* Unary operator. *)
type unop =
  | Not
  | DeRef

type location = string

(* Values. *)
type value =
  | Num of int
  | Bool of bool
  | Loc of location

(* Expressions. *)
type expr =
| Val of value
| BinOp of binop * expr * expr
| UnOp of unop * expr

(* Commands *)
type comm =
| Skip
| Seq of comm * comm
| Assign of expr * expr
| While of expr * comm
| If of expr * comm * comm

(* State *)

(* Define map from locations to values. *)
module Env = Map.Make(String)

(* Type synonym for environment containing values. *)
type state = value Env.t

(* Program consists of commands and some initial states *)
type top_level = 
| Program of comm * state

(* Pretty prints a binary operator. *)
let string_of_binop (op: binop): string =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Leq -> "<="
  | And -> "&&"

(* Pretty prints a unary operator. *)
let string_of_unop (op: unop): string =
  match op with
  | Not -> "~"
  | DeRef -> "!"

(* Pretty prints a value. *)
let string_of_val (v: value): string =
  match v with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Loc l -> l

(* Pretty prints an expression. *)
let rec string_of_expr (e: expr): string =
  match e with
  | Val v -> string_of_val v
  | BinOp (op, e1, e2) -> 
    Printf.sprintf "(%s %s %s)" (string_of_expr e1) (string_of_binop op) (string_of_expr e2)
  | UnOp (op, e) ->
    Printf.sprintf "%s%s" (string_of_unop op) (string_of_expr e)

let rec string_of_comm (c: comm): string =
  match c with
  | Skip -> "skip"
  | Seq (c1, c2) ->
    Printf.sprintf "%s; %s" (string_of_comm c1) (string_of_comm c2)
  | Assign (e1, e2) ->
    Printf.sprintf "%s := %s" (string_of_expr e1) (string_of_expr e2)
  | While (e, c) ->
    Printf.sprintf "while %s do %s" (string_of_expr e) (string_of_comm c)
  | If (e, c1, c2) ->
    Printf.sprintf "if %s then %s else %s" (string_of_expr e) (string_of_comm c1) (string_of_comm c2)


(* State *)

(* Returns empty state. *)
let empty_state: state = Env.empty

(* Extends the state by the mapping location -> value. *)

let state_extend (l: location) (v: value) (s: state): state =
    if Env.exists (fun l' _ -> l' = l) s then 
      failwith ("Cannot extend state for an already defined location " ^ l)
    else
      (* No mapping from l, then add *)
      Env.add l v s

let state_from_list (lst: (location * value) list): state =
  List.fold_left (fun acc (l, v) -> state_extend l v acc) Env.empty lst

(* Extends the state by the mapping location -> value. Only updates if location l exists. *)
let state_update (l: location) (v: value) (s: state): state =
  if Env.exists (fun l' _ -> l' = l) s then 
    (* Found mapping from l, then update *)
    Env.add l v s
  else
    failwith ("Cannot update state for an undefined location" ^ l)

(* Returns the corresponding value. *)
let lookup (l: location) (s: state): value option =
  Env.find_opt l s


(* Pretty prints a state. *)

let string_of_state (s: state) =
  let state_string = 
    s 
    |> Env.bindings 
    |> List.map (fun (x, v) -> x ^ " -> " ^ string_of_val v)
    |> String.concat ", " in
  "{" ^ state_string ^ "}"

let string_of_top_level (p: top_level): string =
  match p with
  | Program (c, s) -> 
    Printf.sprintf "%s %s" (string_of_state s) (string_of_comm c)
