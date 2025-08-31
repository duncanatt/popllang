{
open Parser
}

(* Token patterns. *)
let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let num = digit+
let letter = ['a'-'z' 'A'-'Z']
let var = letter+ num*

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
  | "{" { LBRACK }
  | "}" { RBRACK }
  | "let" { LET }
  | "=" { EQUALS }
  | "==" { EQUALSEQUALS }
  | "," { COMMA }
  | "in" { IN }
  | "fun" { FUN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | var { VAR (Lexing.lexeme lexbuf) }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }