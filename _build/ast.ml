type op = Add 

type expr =
    Integer of int
  | Float of float	
  | Id of string
  | Vec2 of int * int
  | Binop of expr * op * expr

type program =
	Print
	| Exp of expr
