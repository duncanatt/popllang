{
open Parser
}

(* Token patterns. *)
let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let num = digit+

(* Read function transforming strings into tokens. Invoked by the parser. *)
rule read = parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "+" { ADD }
  | "-" { SUB }
  | "<=" { LEQ }
  | "&&" { AND }
  | "~" { NOT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }