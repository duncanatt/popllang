{
open Parser
}

(* Token patterns. *)
let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let num = digit+
(* let letter = ['a'-'z' 'A'-'Z']
let id = letter+ *)

(* Read function transforming strings into tokens. Invoked by the parser. *)
rule read = parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  (* | "*" { TIMES } *)
  | "+" { ADD }
  | "-" { SUB }
  | "<=" { LEQ }
  | "&&" { AND }
  | "~" { NOT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  (* | "let" { LET } *)
  (* | "=" { EQUALS } *)
  (* | "in" { IN } *)
  (* | "if" { IF } *)
  (* | "then" { THEN } *)
  (* | "else" { ELSE } *)
  (* | id { ID (Lexing.lexeme lexbuf) } *)
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }