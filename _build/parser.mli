type token =
  | INTEGER of (int)
  | FLOAT of (float)
  | PLUS
  | ON
  | EOF
  | VEC2 of ((float * float))

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
