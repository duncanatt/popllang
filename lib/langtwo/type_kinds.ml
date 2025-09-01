(* 
todo: change module name from Type_kinds to something better
The data types are defined in a separate mode from types.ml to avoid cyclic dependencies.
 *)

(** Data types. *)
type typ =
  | TNum
  | TBool
  | TFun of typ * typ

(** Pretty print a type. *)
let rec string_of_typ (t: typ): string =
  match t with
  | TNum -> "num"
  | TBool -> "bool"
  | TFun (t1, t2) -> Printf.sprintf "(%s -> %s)" (string_of_typ t1) (string_of_typ t2)
