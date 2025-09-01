%{
open Ast
%}

// Token definitions.
%token <int> NUM
%token <string> VAR
%token TRUE
%token FALSE
%token ADD
%token SUB
%token LEQ
%token AND
%token NOT
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token EOF

// Operator associations and precedence. Lower in the list means higher 
// precedence.
%nonassoc IN
%left AND
%right NOT
%left LEQ
%left ADD SUB
// %left SUB

// Main program production.
%start <expr> prog

%%

// Program root.
prog:
	| e = expr; EOF { e }
	;
	
// Expressions.
expr:
  | n = NUM { Val (Num n) }  
  | x = VAR { Var x }
  | TRUE { Val (Bool true) }
  | FALSE { Val (Bool false) }
  | e1 = expr; ADD; e2 = expr { BinOp (Add, e1, e2) }
  | e1 = expr; SUB; e2 = expr { BinOp (Sub, e1, e2) }
  | e1 = expr; LEQ; e2 = expr { BinOp (Leq, e1, e2) }
  | e1 = expr; AND; e2 = expr { BinOp (And, e1, e2) }
  | NOT; e = expr { UnOp (Not, e) }
  | LET; x = VAR; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2)}
  | LPAREN; e = expr; RPAREN { e }
  ;
