
(* Get AST from string *)
let get_ast (s: string): Ast.expr = 
  let lexbuf = Lexing.from_string s in 
  Parser.prog Lexer.read lexbuf

(* Get AST from string and print AST *)
let get_ast_verbose (s: string): Ast.expr = 
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.prog Lexer.read lexbuf in
  let () = Printf.printf "Parsed AST:\n%s\n" (Ast.string_of_expr ast) in
    ast