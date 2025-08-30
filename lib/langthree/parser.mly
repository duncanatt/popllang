%{
open Ast
%}

// Token definitions.
%token <int> NUM
%token <string> LOC
%token TRUE
%token FALSE
%token ADD
%token SUB
%token LEQ
%token AND
%token NOT
%token BANG
%token LPAREN
%token RPAREN
%token COLONEQUALS
%token SEMICOLON
%token SKIP
%token WHILE
%token DO
%token IF
%token THEN
%token ELSE
%token COMMA
%token ARROW
%token LBRACK
%token RBRACK
%token EOF

// Operator associations and precedence. Lower in the list means higher 
// precedence.
%nonassoc DO
%nonassoc ELSE

%left SEMICOLON
%left AND
%right BANG
%right NOT
%left LEQ
%left ADD
%left SUB

// Main program production.
%start <top_level> prog

%%

// Program root.
prog:
	| c = command; EOF { Program (c, empty_state) }
	| LBRACK; s = state_pairs; RBRACK; c = command; EOF { Program (c, (state_from_list (s))) }
	;

value:
  | n = NUM { Num n }  
  | x = LOC { Loc x }
  | TRUE { Bool true }
  | FALSE { Bool false }
  ;

// Expressions.
expr:
  | v = value {Val v}
  | e1 = expr; ADD; e2 = expr { BinOp (Add, e1, e2) }
  | e1 = expr; SUB; e2 = expr { BinOp (Sub, e1, e2) }
  | e1 = expr; LEQ; e2 = expr { BinOp (Leq, e1, e2) }
  | e1 = expr; AND; e2 = expr { BinOp (And, e1, e2) }
  | NOT; e = expr { UnOp (Not, e) }
  | BANG; e = expr { UnOp (DeRef, e) }
  | LPAREN; e = expr; RPAREN { e }
  ;

// Command.
command:
  | SKIP { Skip }
  | c1 = command; SEMICOLON; c2 = command { Seq (c1, c2) }
  | e1 = expr; COLONEQUALS; e2 = expr { Assign (e1, e2) }
  | WHILE; e = expr; DO; c = command { While (e, c) }
  | IF; e = expr; THEN; c1 = command; ELSE; c2 = command { If (e, c1, c2) }
  ;

// State as pairs.
state_pairs:
  | /* empty */ { [] }
  | l = LOC; ARROW; v = value; COMMA s = state_pairs; { (l, v) :: s }
  | l = LOC; ARROW; v = value; { [(l, v)] }
  ;
