type op = Add 

type expr =
    Literal of int
  | Id of string
  | Vec2 of int * int
  | Binop of expr * op * expr

type program =
	Print
	| Exp of expr
