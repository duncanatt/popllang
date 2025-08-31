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
%token FUN
%token LBRACK
%token RBRACK
%token COMMA
%token IF
%token THEN
%token ELSE
%token EOF

// Operator associations and precedence. Lower in the list means higher 
// precedence.
%left AND
%right NOT
%left LEQ
%left ADD
%left SUB

// Main program production.
%start <expr> prog

%%

// Program root.
prog:
  | e = expr; EOF { desugar e }
  ;

// Expressions.
expr:
  | LET; ps = p_many; IN; e = expr  // sugar
  {
    match ps with
    | [ (x,e1) ] -> S_Let (x, e1, e)
    | _ -> S_LetMany (ps, e)
  }
  // | LET; x = VAR; EQUALS; e1 = expr; IN; e2 = expr { S_Let (x, e1, e2) } // sugar
  // | LET; ps = p_many; IN; e = expr { S_LetMany (ps, e) } // sugar
  | FUN; LPAREN; y = VAR; RPAREN; LBRACK; e = expr; RBRACK { S_FunAnon (y, e) }
  | FUN; f = VAR; LPAREN; ys = var_many; RPAREN; LBRACK; e = expr; RBRACK 
  {
    match ys with
    | [ y ] -> S_Fun (f, y, e) 
    | _ -> S_FunMany (f, ys, e) (* sugar *)
  }
  // | FUN; f = VAR; LPAREN; y = VAR; RPAREN; LBRACK; e = expr; RBRACK { S_Fun (f, y, e) }
  // | FUN; f = VAR; LPAREN; ys = var_many; RPAREN; LBRACK; e = expr; RBRACK { S_FunMany (f, ys, e) } // sugar
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { S_If (e1, e2, e3) } // sugar
  | e = expr_others { e }
  ;

// We split expressions in different hierarchy (expr_others, app_expr, atom) to avoid ambiguity in parsing function application and binary operators. For example, the expression f(x) + y could be parsed as (f(x)) + y or f(x + y); this ambuiguity is resolved by having hierarchic expression levels. 

expr_others:
  | e1 = expr_others; ADD; e2 = expr_others { S_BinOp (S_Add, e1, e2) }
  | e1 = expr_others; SUB; e2 = expr_others { S_BinOp (S_Sub, e1, e2) }
  | e1 = expr_others; LEQ; e2 = expr_others { S_BinOp (S_Leq, e1, e2) }
  | e1 = expr_others; AND; e2 = expr_others { S_BinOp (S_And, e1, e2) } // sugar
  | NOT e = expr_others { S_UnOp (S_Not, e) } // sugar
  | a = app_expr { a }
  ;

app_expr:
  | f = app_expr; LPAREN; es = expr_many; RPAREN
      {
        match es with
        | [e] -> S_App (f, e)
        | _   -> S_AppMany (f, es) (* sugar *)
      }
  // | f = app_expr; LPAREN; es = expr_many; RPAREN { S_AppMany (f, es) }
  // | f = app_expr; arg = atom                    { S_App (f, arg) }
  | a = atom                                    { a }
  ;

atom:
  | n = NUM    { S_Val (Num n) }
  | x = VAR    { S_Var x }
  | TRUE       { S_Val (Bool true) }
  | FALSE      { S_Val (Bool false) }
  | LPAREN; e = expr; RPAREN { e }
  ;

expr_many:
  | e = expr { [e] }
  | e = expr; COMMA; es = expr_many { e :: es }
  ;

p_many:
  | x = VAR; EQUALS; e = expr { [(x, e)] }
  | x = VAR; EQUALS; e = expr; COMMA; es = p_many { (x, e) :: es }
  ;
  
var_many:
  | x = VAR; COMMA; xs = var_many { x :: xs }
  | { [] }
  ;