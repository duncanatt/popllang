%{
open Langone_ast
%}

%token <int> NUM
// %token <string> ID
%token TRUE
%token FALSE
%token ADD
%token SUB
%token LEQ
%token AND
%token NOT
%token LPAREN
%token RPAREN
// %token LET
// %token EQUALS
// %token IN
// %token IF
// %token THEN
// %token ELSE
%token EOF

// %nonassoc IN
// %nonassoc ELSE
%left AND
%right NOT
%left LEQ
%left ADD
%left SUB

%start <expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
  | n = NUM { Val (Num n) }  
  | TRUE { Val (Bool true) }
  | FALSE { Val (Bool false) }
  | e1 = expr; ADD; e2 = expr { BinOp(Add, e1, e2) }
  | e1 = expr; SUB; e2 = expr { BinOp(Sub, e1, e2) }
  | e1 = expr; LEQ; e2 = expr { BinOp(Leq, e1, e2) }
  | e1 = expr; AND; e2 = expr { BinOp(And, e1, e2) }
  | NOT; e = expr { UnOp(Not, e) }
  | LPAREN; e = expr; RPAREN { e }
  ;
  
// expr:
// 	| i = INT { Int i }
// 	| x = ID { Var x }
// 	| TRUE { Bool true }
// 	| FALSE { Bool false }
// 	| e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
// 	| e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) } 
// 	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
// 	| LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
// 	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
// 	| LPAREN; e=expr; RPAREN {e} 
// 	;
	
