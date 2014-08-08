type op = Add | On

type expr =
    Integer of int
  | Float of float	
  | Id of string
  | Vec2 of float * float
  | Binop of expr * op * expr

type exprs = 
	expr list

type program = 
	exprs
