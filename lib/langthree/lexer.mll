{
open Parser
}

(* Token patterns. *)
let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let num = digit+
let letter = ['a'-'z' 'A'-'Z']
let loc = letter+ num*

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
  | "!" { BANG }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACK }
  | "}" { RBRACK }
  | ":=" { COLONEQUALS }
  | ";" { SEMICOLON }
  | "->" { ARROW }
  | "," { COMMA }
  | "skip" { SKIP }
  | "while" { WHILE }
  | "do" { DO }
  | "if" { THEN }
  | "then" { ELSE }
  | "else" { EOF }
  | loc { LOC (Lexing.lexeme lexbuf) }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }