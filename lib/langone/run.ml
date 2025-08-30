
(** [get_ast s] parses the input string [s] and returns its abstract syntax tree (AST) as an [expression]. *)
let get_ast (s: string): Ast.expr = 
  let lexbuf = Lexing.from_string s in 
  Parser.prog Lexer.read lexbuf

(** [get_ast_verbose s] parses the input string [s] into an AST of type [expression], prints the resulting AST, and returns it. *)
let get_ast_verbose (s: string): Ast.expr = 
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.prog Lexer.read lexbuf in
  let () = Printf.printf "Parsed AST:\n%s\n" (Ast.string_of_expr ast) in
    ast
