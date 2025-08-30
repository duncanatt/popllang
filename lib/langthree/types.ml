
(* Data types. *)
type typ =
  | TNum
  | TBool
  | TRef of typ

(* Define map from variable names to types. *)
module Env = Map.Make(String)

(* Type synonym for environment with typ values. *)
type typenv = typ Env.t

(* Returns empty type environment. *)
let empty_env: typenv = Env.empty

(* Extends the type environment by the mapping value -> type. *)
let extend (x: String.t) (t: typ) (env: typenv): typenv =
  Env.add x t env 

(* Returns the corresponding type for mapped by value. *)
let lookup (x: string) (env: typenv): typ option =
  Env.find_opt x env

(* Returns the stringified value of a type. *)
let rec string_of_typ: typ -> string = function
  | TBool -> "bool"
  | TNum -> "num"
  | TRef t -> "ref(" ^ string_of_typ t ^ ")"

(* Pretty print typing environment. *)
let string_of_env (env: typenv) =
  env 
  |> Env.bindings 
  |> List.map (fun (x, t) -> x ^ " : " ^ string_of_typ t)
  |> String.concat ", "


(* fetch_type_of_loc is used to find the type of a single location l in state s. Cyclic reference locations are detected using known_locations. *)
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

(* Build the Sigma environment, mapping all locations from the state to their types. *)
(* Does not follow Slide 139 precisely, since the notes assume the Sigma upfront, but here we infer it. *)
let types_of_loc_in_state (s: Ast.state): typenv =
  Env.fold (fun l v acc -> 
    match v with
    | Ast.Num _ -> extend l TNum acc
    | Ast.Bool _ -> extend l TBool acc
    | Ast.Loc l' -> extend l  (TRef (fetch_type_of_loc l' s [])) acc
  ) s empty_env

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
  

(* Checks that the commands has well-formed types. Commands do not have explicit types so this functions doesn not infer or check against an explicit type. *)
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

(* Type check programs. Types for expressions are inferred as needed. *)
let typecheck (c: Ast.top_level): unit =
  let Ast.Program (commands, states) = c in
  let sigma = types_of_loc_in_state states in
  typecheck_command commands sigma
(*   
  try 
    let () = typecheck_command commands sigma in
    Printf.printf "Typecheck successful for: %s\n" (Ast.string_of_top_level c)
  with Failure msg ->
    Printf.printf "Typecheck failed for: %s\n%s\n" (Ast.string_of_top_level c) msg *)