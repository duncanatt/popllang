
(** Data types. *)
type typ =
  | TNum
  | TBool
  | TRef of typ

(** Define map from variable names to types. *)
module Env = Map.Make(String)

(** Type synonym for environment with [typ] values. *)
type typenv = typ Env.t

(** [empty_env] creates an empty type environment. *)
let empty_env: typenv = Env.empty

(** [extend x t env] extends the type environment [env] by the value [x] of type [t], i.e. [x => t]. *)
let extend (x: String.t) (t: typ) (env: typenv): typenv =
  Env.add x t env 

(** [lookup x env] returns the corresponding type for mapped by value [x]. *)
let lookup (x: string) (env: typenv): typ option =
  Env.find_opt x env

(** [string_of_env env t] pretty prints the types. *)
let rec string_of_typ: typ -> string = function
  | TBool -> "bool"
  | TNum -> "num"
  | TRef t -> "ref(" ^ string_of_typ t ^ ")"

(** [string_of_env env] pretty prints the typing environment [env]. *)
let string_of_env (env: typenv) =
  env 
  |> Env.bindings 
  |> List.map (fun (x, t) -> x ^ " : " ^ string_of_typ t)
  |> String.concat ", "


(** [fetch_type_of_loc l s known_locations] returns the type of the location [l] in the state [s]. It uses [known_locations] to detect and prevent cyclic references. *)
let rec fetch_type_of_loc (l: Ast.location) (s: Ast.state) (known_locations: Ast.location list): typ =
  match Ast.lookup l s with
  | None -> failwith ("Cannot find location " ^ l ^ " in state.")
  | Some v -> 
    match v with
    | Ast.Num _ -> TNum
    | Ast.Bool _ -> TBool
    | Ast.Loc l' -> 
      if List.mem l' known_locations then
        failwith ("Cyclic reference detected in location " ^ l ^ " pointing to " ^ l' ^ ".")
      else
        (* References other locations in the state, so has type TRef *)
        TRef (fetch_type_of_loc l' s (known_locations @ [l]))

(** [types_of_loc_in_state s] builds the Sigma environment by mapping each location in the given [state] to its corresponding type. This function infers the Sigma environment from the current state, rather than requiring it as input (which contrasts with the typing rules in Slide 139).
*)
let types_of_loc_in_state (s: Ast.state): typenv =
  Env.fold (fun l v acc -> 
    match v with
    | Ast.Num _ -> extend l TNum acc
    | Ast.Bool _ -> extend l TBool acc
    | Ast.Loc l' -> extend l  (TRef (fetch_type_of_loc l' s [])) acc
  ) s empty_env


(** [infer_expr e env] infers the type of the expression [e] under the type environment [env]. It returns the type [typ] that [e] is assigned to, or raises an error using [failwith] if the expression is not well-typed in the given environment.
  
  @raise Failure if the expression is not well-typed.
*)
let rec infer_expr (e: Ast.expr) (env: typenv): typ =
  match e with 
  | Val v -> 
    (match v with
    | Ast.Num _ -> TNum (* TNum *)
    | Ast.Bool _ -> TBool (* TSub *)
    | Ast.Loc l -> (* TLoc *)
      (match lookup l env with
      | Some t -> TRef t
      | None -> failwith ("Location " ^ l ^ " not found in type environment (sigma)")))
  | BinOp (Add, e1, e2) -> (* TAdd *)
    (match (infer_expr e1 env, infer_expr e2 env) with
    | (TNum, TNum) -> TNum
    | (_, _) -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | BinOp (Sub, e1, e2) -> (* TSub *)
    (match (infer_expr e1 env, infer_expr e2 env) with
    | (TNum, TNum) -> TNum
    | (_, _) -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | BinOp (Leq, e1, e2) -> (* TLeq *)
    (match (infer_expr e1 env, infer_expr e2 env) with
    | (TNum, TNum) -> TBool
    | (_, _) -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | BinOp (And, e1, e2) -> (* TAnd *)
    (match (infer_expr e1 env, infer_expr e2 env) with
    | (TBool, TBool) -> TBool
    | (_, _) -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | UnOp (Not, e) -> (* TNot *)
    (match (infer_expr e env) with
    | TBool -> TBool
    | _ -> failwith ("Cannot infer type for " ^ (Ast.string_of_expr e)))
  | UnOp (DeRef, e) -> (* TDer *)
    (match (infer_expr e env) with
    | TRef t -> t
    | _ -> failwith ("Cannot dereference non-location expression " ^ (Ast.string_of_expr e)))
  

(** [check_command_typing c env] Checks that the command is well-typed according to the typing rules using the typing environment [env]. This function ensures all commands and expressions are correct typed, but does not infer or check against a specific result type. *)
let rec typecheck_command (c: Ast.comm) (env: typenv): unit =
    match c with
    | Skip -> () (* TSkip *)
    | Assign (e1, e2) -> (* TAss *)
        (let t' = infer_expr e1 env in
        match t' with
        | TRef t -> 
          (if (infer_expr e2 env) <> t then 
            failwith ("Type mismatch in assignment, expected " ^ string_of_typ t ^ " but got " ^ string_of_typ (infer_expr e2 env)))
        | _ -> failwith ("You can only assign to locations, got " ^ (Ast.string_of_expr e1) ^ " of type " ^ string_of_typ t'))
    | Seq (c1, c2) -> (* TSeq *)
        (typecheck_command c1 env; typecheck_command c2 env)
    | If (e, c1, c2) -> (* TIf *)
        (if (infer_expr e env) <> TBool then 
          failwith ("If condition must be boolean, got " ^ (Ast.string_of_expr e) ^ " of type " ^ string_of_typ (infer_expr e env));
        typecheck_command c1 env; typecheck_command c2 env)
    | While (e, c) -> (* TWhl *)
        (if (infer_expr e env) <> TBool then 
          failwith ("While condition must be boolean, got " ^ (Ast.string_of_expr e) ^ " of type " ^ string_of_typ (infer_expr e env));
        typecheck_command c env)

(** [typecheck c] type checks program [c]. Types for expressions are inferred as needed. *)
let typecheck (c: Ast.top_level): unit =
  let Ast.Program (commands, states) = c in
  let sigma = types_of_loc_in_state states in
  typecheck_command commands sigma

(** [typecheck_verbose c] type checks program [c], printing verbose output. *)
let typecheck_verbose (c: Ast.top_level): unit =
  let Ast.Program (commands, states) = c in
  try 
    let sigma = types_of_loc_in_state states in
    let () = typecheck_command commands sigma in
    Printf.printf "Type checking successful for: %s\n" (Ast.string_of_top_level c)
  with Failure msg ->
    Printf.printf "Type checking failed for: %s\n%s\n" (Ast.string_of_top_level c) msg