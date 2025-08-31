%{
open Ast
%}

%token <int> NUM
%token <string> VAR
%token TRUE FALSE
%token ADD SUB LEQ AND NOT
%token LPAREN RPAREN
%token LET EQUALS IN
%token FUN LBRACK RBRACK
%token COMMA
%token IF THEN ELSE
%token EOF

%left AND
%right NOT
%left LEQ
%left ADD SUB

%start <expr> prog

%%

// Program root.
prog:
  | e = expr; EOF { desugar e }

// Expressions.
expr:
  | LET; ps = p_many; IN; e = expr
      { match ps with [ (x,e1) ] -> S_Let(x,e1,e) | _ -> S_LetMany(ps,e) }
  | FUN; LPAREN; y = VAR; RPAREN; LBRACK; e = expr; RBRACK { S_FunAnon(y,e) }
  | FUN; f = VAR; LPAREN; ys = var_many; RPAREN; LBRACK; e = expr; RBRACK
      { match ys with [y] -> S_Fun(f,y,e) | _ -> S_FunMany(f,ys,e) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { S_If(e1,e2,e3) }
  | e = expr_op { e }

// Unary/Binary operators + function application.
expr_op:
  | e1 = expr_op; ADD; e2 = expr_op { S_BinOp(S_Add,e1,e2) }
  | e1 = expr_op; SUB; e2 = expr_op { S_BinOp(S_Sub,e1,e2) }
  | e1 = expr_op; LEQ; e2 = expr_op { S_BinOp(S_Leq,e1,e2) }
  | e1 = expr_op; AND; e2 = expr_op { S_BinOp(S_And,e1,e2) }
  | NOT e = expr_op { S_UnOp(S_Not,e) }
  | f = expr_app { f }

expr_app:
  | f = expr_app; arg = expr_atom { S_App(f,arg) }
  | a = expr_atom { a }

expr_atom:
  | n = NUM { S_Val(Num n) }
  | x = VAR { S_Var x }
  | TRUE { S_Val(Bool true) }
  | FALSE { S_Val(Bool false) }
  // | e = expr { e }
  | LPAREN; e = expr; RPAREN { e }

p_many:
  | x = VAR; EQUALS; e = expr { [(x,e)] }
  | x = VAR; EQUALS; e = expr; COMMA; ps = p_many { (x,e)::ps }

var_many:
  | x = VAR { [x] }
  | x = VAR; COMMA; xs = var_many { x::xs }
  | { [] }