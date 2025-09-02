open Ast

(** Free variables *)
let fv (e: expr) : string list =
  let rec fv_inner (e: expr) (found: string list) : string list =
    match e with
    | Val _ -> []
    | Var x -> [x]
    | Fun (f, (x, _), body) -> List.filter (fun y -> (y <> x && y <> f)) (fv_inner body found) (* found \ {f, x} *)
    | FunAnon ((x, _), body) -> List.filter (fun y -> (y <> x)) (fv_inner body found) (* found \ {f, x} *)
    | BinOp (_, e1, e2) -> (fv_inner e1 found) @ (fv_inner e2 found)
    | App (e1, e2) -> (fv_inner e1 found) @ (fv_inner e2 found)
    | If (cond, then_br, else_br) ->
        (fv_inner cond found) @ (fv_inner then_br found) @ (fv_inner else_br found)
  in
  List.sort_uniq String.compare (fv_inner e []) (* removes duplicate *)

(** [subst x e_new e] subsitutes variable [x] by value/expression [e_new] (inc. functions) in expression [e]. *)
let rec subst (x: string) (e_new: expr) (e: expr) : expr =
  match e with
  | Val _ -> e
  | Var y -> if x = y then e_new else e
  | Fun (f, (y, t_opt), body) ->
      if x = f || x = y || List.mem f (fv e_new) || List.mem y (fv e_new) then e (* f,y ∉ { x } ∪ fv(v) *)
      else Fun (f, (y, t_opt), subst x e_new body)
  | FunAnon ((y, t_opt), body) ->
      if x = y || List.mem y (fv e_new) then e
      else FunAnon ((y, t_opt), subst x e_new body)
  | BinOp (op, e1, e2) -> BinOp (op, subst x e_new e1, subst x e_new e2)
  | App (e1, e2) -> App (subst x e_new e1, subst x e_new e2)
  | If (cond, then_br, else_br) ->
      If (subst x e_new cond, subst x e_new then_br, subst x e_new else_br)


(** [is_value e] returns true if expression [e] is a value (number, boolean, or function). *)
let is_value (e: expr) : bool =
  match e with
  | Val _ -> true
  | Fun _ -> true
  | FunAnon _ -> true
  | _ -> false

(** [reduce e] performs a single reduction step on expression [e]. *)
let rec reduce (e: expr) : expr =
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
  | App (e1, e2) ->
        if is_value e1 then
          if is_value e2 then 
            (match e1 with
             | Fun (f, (x, _), body) -> (* RApp1*)
               body
               |> subst f e1
               |> subst x e2 
             | FunAnon ((x, _), body) -> (* RAppSmp*)
               subst x e2 body
             | _ -> failwith ("Function application to a non-function:" ^ string_of_expr e1 ^ " with argument " ^  string_of_expr e2)
            )
          else
            App (e1, reduce e2) (* RApp3 *)
        else
          App (reduce e1, e2) (* RApp2 *)
          
  | If (cond, e1, e2) ->
      (match cond with
      | Val (Bool true) -> e1 (* RIf1 *)
      | Val (Bool false) -> e2 (* RIf2 *)
      | _ -> If (reduce cond, e1, e2) (* RIf3 *))
  | Var x -> failwith (Printf.sprintf "You can only reduce closed terms; %s is free" x)
  | Fun _
  | FunAnon _
  | Val _ -> failwith ("Value '" ^ (Ast.string_of_expr e) ^ "' does not reduce")

(** [reduce_verbose e] reduces an expression [e] by one step, prints the reduction, and returns the resulting expression. *)
let reduce_verbose (e: expr): expr =
  let res = reduce e in
    let () = Printf.printf "%s reduces to %s\n" (Ast.string_of_expr e) (Ast.string_of_expr res) in 
    res

(** [reduce_all e] reduces expression e, step by step, into a final value and returns it as an [expression]. *)
let rec reduce_all (e: expr): expr =
  if is_value e then e else reduce_all (reduce e)

(** [reduce_all_verbose e] repeatedly reduces the given expression [e] step by step, printing the expression at each step, until it reaches a [value]. Returns the final value as an [expression].*)
let rec reduce_all_verbose (e: expr): expr =
  if is_value e then 
    ((Printf.printf "%s\n" (Ast.string_of_expr e)); 
    e)
  else 
    ((Printf.printf "%s\n" (Ast.string_of_expr e));
    reduce_all_verbose (reduce e))

let rec eval (_e: expr) : expr =
  failwith "todo"