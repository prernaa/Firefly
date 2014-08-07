type token =
  | PRINT
  | INTEGER of (int)
  | FLOAT of (float)
  | PLUS
  | EOF
  | VEC2 of ((int * int))

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
