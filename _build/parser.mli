type token =
  | INTEGER of (int)
  | FLOAT of (float)
  | ASSIGN
  | PLUS
  | MINUS
  | ON
  | OFF
  | EOF
  | VEC2 of ((float * float))
  | IDENTIFIER of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
