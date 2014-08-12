type op = Add | Minus | LessThan | LessThanEq | GreaterThan | GreaterThanEq | 
			EqualsTo| On | Off

type constant =
	Integer of int
  | Float of float
 
type expr =
    Constant of constant
  | NegConstant of constant
  | Identifier of string (* can contain alphabets, numbers or underscores, but must begin with an alphabet*)
  | Vec2 of expr * expr
  | Binop of expr * op * expr
  | Assign of string * expr  
  
type stmt =
	Expr of expr
  |	Block of stmt list
  | If of expr * stmt * stmt
  | While of expr * stmt
  
type stmts = 
	stmt list
	
type program = 
	stmts