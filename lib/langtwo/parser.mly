%{
open Ast
%}

%token <int> NUM
%token <string> VAR
%token TRUE FALSE
%token ADD SUB LEQ AND NOT EQUALSEQUALS
%token LPAREN RPAREN
%token LET EQUALS IN
%token FUN LBRACK RBRACK
%token COMMA
%token COLON
%token ARROW
%token IF THEN ELSE
%token EOF

// %right NOT
%right ARROW
%left EQUALSEQUALS
%left AND
%left LEQ
%left ADD SUB

%start <expr> prog

%%

////////////////////////////////
// This VERSION accepts the more general syntax
// without conflicts due to the hierarchical parsing pattern
////////////////////////////////

// Program root.
prog:
  | e = expr; EOF { desugar e }
  ;

// We split expressions in different hierarchy (let_expr, bin_op_expr, ...) to avoid ambiguity in parsing function application and other expressions (Let/If/Fun). For example, the expression f(x) + y could be parsed as (f(x)) + y or f(x + y); this ambuiguity is resolved by having hierarchic expression levels. 

// Expressions.
expr:
  | IF c = expr; THEN; t = expr; ELSE e = expr; { S_If(c,t,e); }
  | l = let_expr; { l; }
  ;

let_expr:
// Pattern for multiple let bindings with optional type annotations : 
//   x : t = e, y = e
  | LET; ps = separated_list(COMMA, var_type_exp); IN; e = expr; 
      { match ps with [ (x,e1) ] -> S_Let(x,e1,e) | _ -> S_LetMany(ps,e); }
  | b = bin_op_expr; { b; }
  ;

bin_op_expr:
  | l = bin_op_expr; AND; r = bin_op_expr; { S_BinOp(S_And,l,r); }
  | l = bin_op_expr; LEQ; r = bin_op_expr; { S_BinOp(S_Leq,l,r); }
  | l = bin_op_expr; ADD; r = bin_op_expr; { S_BinOp(S_Add,l,r); }
  | l = bin_op_expr; SUB; r = bin_op_expr; { S_BinOp(S_Sub,l,r); }
  | l = bin_op_expr; EQUALSEQUALS; r = bin_op_expr; { S_BinOp(S_Equal,l,r); }
  | a = app_expr; { a; }
  ;

app_expr:
  | f = app_expr; a = unary_expr; { S_App(f,a); }  (* left-associative application *)
  | a = unary_expr; { a; }
  ;

unary_expr:
  | NOT; a = unary_expr; { S_UnOp(S_Not,a); }
  | a = values_par; { a; }
  ;

// Values and brackets
values_par:
  | n = NUM; { S_Val(Num n); }
  | x = VAR; { S_Var x; }
  | TRUE; { S_Val(Bool true); }
  | FALSE; { S_Val(Bool false); }
  | LPAREN; e = expr; RPAREN; { e; }
  | FUN; LPAREN; y = var_type_pair; RPAREN; LBRACK; e = expr; RBRACK; { S_FunAnon(y,e); }
  | FUN; f = VAR; LPAREN; ys = separated_list(COMMA,var_type_pair); RPAREN; LBRACK; e = expr RBRACK;
      { match ys with [y] -> S_Fun(f,y,e) | _ -> S_FunMany(f,ys,e); }
  ;

// Pattern for multiple let bindings with optional type annotations : 
//   x : t = e, y = e
var_type_exp:
  | v = var_type_pair; EQUALS; e = expr { (v,e) }
  ;

// Variable name and optional type
var_type_pair:
  | x = VAR { (x, None ) }
  | x = VAR; COLON; t = full_type { (x, Some t) }
  ;

full_type:
  | t = VAR {
      match t with
      | "int" -> TNum
      | "bool" -> TBool
      | _ -> failwith ("Unknown type: " ^ t)
    }
  | t1 = full_type; ARROW; t2 = full_type { TFun (t1, t2) }
  | LPAREN; t = full_type; RPAREN { t } 
  ;




////////////////////////////////
// VERSION with conflicts, but accepts the more general syntax
////////////////////////////////

// // Program root.
// prog:
//   | e = expr; EOF { desugar e }

// // Expressions.
// expr:
//   | LET; ps = separated_list(COMMA, var_pair); IN; e = expr
//       { match ps with [ (x,e1) ] -> S_Let(x,e1,e) | _ -> S_LetMany(ps,e) }
//   | FUN; LPAREN; y = VAR; RPAREN; LBRACK; e = expr; RBRACK { S_FunAnon(y,e) }
//   | FUN; f = VAR; LPAREN; ys = separated_list(COMMA, VAR); RPAREN; LBRACK; e = expr; RBRACK
//       { match ys with [y] -> S_Fun(f,y,e) | _ -> S_FunMany(f,ys,e) }
//   | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { S_If(e1,e2,e3) }
//   | e1 = expr; ADD; e2 = expr { S_BinOp(S_Add,e1,e2) }
//   | e1 = expr; SUB; e2 = expr { S_BinOp(S_Sub,e1,e2) }
//   | e1 = expr; LEQ; e2 = expr { S_BinOp(S_Leq,e1,e2) }
//   | e1 = expr; AND; e2 = expr { S_BinOp(S_And,e1,e2) }
//   | NOT e = expr { S_UnOp(S_Not,e) }
//   | f = expr; arg = expr { S_App(f,arg) }
//   | n = NUM { S_Val(Num n) }
//   | x = VAR { S_Var x }
//   | TRUE { S_Val(Bool true) }
//   | FALSE { S_Val(Bool false) }
//   | LPAREN; e = expr; RPAREN { e }
//   ;

// var_pair:
//   | x = VAR; EQUALS; e = expr { (x,e) }
//   ;








////////////////////////////////
// OTHER VERSION [no conflicts]
// with hierarchical parsing of application (so that it does 
// not conflict with other binary operators/let/if/fun).
////////////////////////////////

// // Program root.
// prog:
//   | e = expr; EOF { desugar e }

// // Expressions.
// expr:
//   | LET; ps = separated_list(COMMA, var_pair); IN; e = expr
//       { match ps with [ (x,e1) ] -> S_Let(x,e1,e) | _ -> S_LetMany(ps,e) }
//   | FUN; LPAREN; y = VAR; RPAREN; LBRACK; e = expr; RBRACK { S_FunAnon(y,e) }
//   | FUN; f = VAR; LPAREN; ys = separated_list(COMMA, VAR); RPAREN; LBRACK; e = expr; RBRACK
//       { match ys with [y] -> S_Fun(f,y,e) | _ -> S_FunMany(f,ys,e) }
//   | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { S_If(e1,e2,e3) }
//   | e = expr_op { e }

// // Unary/Binary operators + function application.
// expr_op:
//   | e1 = expr_op; ADD; e2 = expr_op { S_BinOp(S_Add,e1,e2) }
//   | e1 = expr_op; SUB; e2 = expr_op { S_BinOp(S_Sub,e1,e2) }
//   | e1 = expr_op; LEQ; e2 = expr_op { S_BinOp(S_Leq,e1,e2) }
//   | e1 = expr_op; AND; e2 = expr_op { S_BinOp(S_And,e1,e2) }
//   | NOT e = expr_op { S_UnOp(S_Not,e) }
//   | f = expr_app { f }

// expr_app:
//   | f = expr_app; arg = expr_atom { S_App(f,arg) }
//   | a = expr_atom { a }

// expr_atom:
//   | n = NUM { S_Val(Num n) }
//   | x = VAR { S_Var x }
//   | TRUE { S_Val(Bool true) }
//   | FALSE { S_Val(Bool false) }
//   // | e = expr { e }
//   | LPAREN; e = expr; RPAREN { e }

// var_pair:
//   | x = VAR; EQUALS; e = expr { (x,e) }
//   ;
