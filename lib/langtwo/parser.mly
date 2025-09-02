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
      | "num" -> TNum
      | "bool" -> TBool
      | _ -> failwith ("Unknown type: " ^ t)
    }
  | t1 = full_type; ARROW; t2 = full_type { TFun (t1, t2) }
  | LPAREN; t = full_type; RPAREN { t } 
  ;
