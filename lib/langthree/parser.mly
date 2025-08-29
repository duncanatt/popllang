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
%start <comm> prog

%%

// Program root.
prog:
	| c = command; EOF { c }
	;
	
// Command.
command:
  | SKIP { Skip }
  | c1 = command; SEMICOLON; c2 = command { Seq (c1, c2) }
  | e1 = expr; COLONEQUALS; e2 = expr { Assign (e1, e2) }
  | WHILE; e = expr; DO; c = command { While (e, c) }
  | IF; e = expr; THEN; c1 = command; ELSE; c2 = command { If (e, c1, c2) }
  ;

// Expressions.
expr:
  | v = value {v}
  | e1 = expr; ADD; e2 = expr { BinOp (Add, e1, e2) }
  | e1 = expr; SUB; e2 = expr { BinOp (Sub, e1, e2) }
  | e1 = expr; LEQ; e2 = expr { BinOp (Leq, e1, e2) }
  | e1 = expr; AND; e2 = expr { BinOp (And, e1, e2) }
  | NOT; e = expr { UnOp (Not, e) }
  | BANG; e = expr { UnOp (DeRef, e) }
  | LPAREN; e = expr; RPAREN { e }
  ;

value:
  | n = NUM { Val (Num n) }  
  | x = LOC { Val (Loc x) }
  | TRUE { Val (Bool true) }
  | FALSE { Val (Bool false) }
  ;