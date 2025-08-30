open Ast

let rec eval_expr (e: expr) (s: state): value =
  match e with
  | BinOp (Add, e1, e2) -> (* EAdd *)
    (match (eval_expr e1 s, eval_expr e2 s) with
    | (Num n1, Num n2) -> Num (n1 + n2)
    | (_, _) -> failwith ("You can only add two numerals in " ^ (Ast.string_of_expr e)))
  | BinOp (Sub, e1, e2) -> (* ESub *)
    (match (eval_expr e1 s, eval_expr e2 s) with
    | (Num n1, Num n2) -> Num (n1 - n2)
    | (_, _) -> failwith "You can only subtract two numerals")
  | BinOp (Leq, e1, e2) -> (* ELeq*)
    (match (eval_expr e1 s, eval_expr e2 s) with
    | (Num n1, Num n2) -> Bool (n1 <= n2)
    | (_, _) -> failwith "You can only compare two numerals")
  | BinOp (And, e1, e2) -> (* EAnd *)
    (match (eval_expr e1 s, eval_expr e2 s) with
    | (Bool b1, Bool b2) -> Bool (b1 && b2)
    | (_, _) -> failwith "You can only compare two numerals")
  | UnOp (Not, e) -> (* ENot *)
    (match (eval_expr e s) with
    | Bool b -> Bool (not b)
    | _ -> failwith "You can only negate booleans")
  | UnOp (DeRef, l) -> (* EDer *)
      (match eval_expr l s with
      | Loc loc -> 
          (match Ast.lookup loc s with
          | Some v -> v
          | None -> failwith ("Location " ^ loc ^ " not found in state (out-of bounds dereference)"))
      | _ -> failwith "You can only dereference locations")
  | Val v -> v (* EVal *)

let rec eval_command (c: comm) (s: state): state =
  match c with
  | Skip -> s (* ESkip *)
  | Assign (e1, e2) -> (* EAssgn *)
      (match eval_expr e1 s with
      | Loc l ->
          let v = eval_expr e2 s in
          Ast.state_update l v s
      | _ -> failwith "You can only assign to locations")
  | Seq (c1, c2) -> (* ESeq *)
    let s' = eval_command c1 s in
    let s'' = eval_command c2 s' in
      s''
  | While (e, c) -> (* EWhl1, EWhl2 *)
    (match eval_expr e s with
    | Bool false -> s (* EWhl1 *)
    | Bool true -> (* EWhl2 *)
        (let s' = eval_command c s in
        let s'' = eval_command (While (e, c)) s' in
        s'')
    | _ -> failwith "You can only use booleans in while conditions")
  | If (e, c1, c2) -> (* EThen, EElse *)
    (match eval_expr e s with
    | Bool true -> eval_command c1 s (* EThen *)
    | Bool false -> eval_command c2 s (* EElse *)
    | _ -> failwith "You can only use booleans in if conditions")

let eval (p: top_level): Ast.state =
  let Program (c, s) = p in
  eval_command c s

let eval_verbose (p: top_level): Ast.state =
  let Program (c, s) = p in
  let s_res = eval_command c s in
  let () = Printf.printf "%s evaluates to %s\n" (Ast.string_of_top_level (Program (c, s))) (Ast.string_of_state s_res) in 
  s_res

let rec reduce_command (c: comm) (s: state): comm * state =
  match c with
  | Skip -> (Skip, s) (* RSkip *)
  | Assign (e1, e2) -> (* RAss *)  
    (match eval_expr e1 s with
    | Loc l1 -> 
      (let v = eval_expr e2 s in
      (Skip, (state_update l1 v s)))  
    | _ -> failwith ("You can only assign to locations in " ^ (Ast.string_of_comm c)))
  | Seq (Skip, c') -> (c', s) (* RSeq1 *)
  | Seq (c1, c2) -> (* RSeq2 *)
      let (c3, s') = reduce_command c1 s in
      (Seq (c3, c2), s')
  | If (e, c1, c2) -> (* RThen, RElse *)
    (match eval_expr e s with
    | Bool true -> (c1, s)  (* RThen *)
    | Bool false -> (c2, s) (* RElse *)
    | _ -> failwith ("You can only use booleans in if conditions in " ^ (Ast.string_of_comm c)))
  | While (e, c) -> (* RWhl *)
    (If (e, Seq (c, While (e, c)), Skip), s)

let reduce (p: top_level): (comm * state) =
  let Program (c, s) = p in
    reduce_command c s

let reduce_verbose (p: top_level): (comm * state) =
  let Program (c, s) = p in
  let c', s' = reduce_command c s in
    let () = Printf.printf "%s reduces to %s\n" (Ast.string_of_top_level p) (Ast.string_of_top_level (Program (c', s'))) in 
    (c', s')

let rec reduce_all (p: top_level): state =
  let Program (c, s) = p in
  match c with
  | Skip -> s
  | _ -> 
    let c', s' = (reduce p) in
      reduce_all (Program (c', s')) 

let rec reduce_all_verbose (p: top_level): state =
  let Program (c, s) = p in
  match c with
  | Skip ->
    (Printf.printf "%s\n" (Ast.string_of_top_level p)); 
    s
  | _ -> 
    (Printf.printf "%s\n" (Ast.string_of_top_level p));
    let c', s' = (reduce p) in  
      reduce_all_verbose (Program (c', s'))