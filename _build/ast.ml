type op = Add | On | Off

type constant =
	Integer of int
  | Float of float
  
type expr =
    Constant of constant
  | NegConstant of constant
  | Identifier of string (* can contain alphabets, numbers or underscores, but must begin with an alphabet*)
  | Vec2 of float * float
  | Binop of expr * op * expr
  | Assign of string * expr

type stmt =
	Expr of expr
  
type stmts = 
	stmt list
	
type program = 
	stmts