type op = Add | Minus | Multiply | Divide | LessThan | LessThanEq | 
			GreaterThan | GreaterThanEq | EqualsTo | NotEqualsTo | On | Off |
			Or | And

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
  | Not of expr
  | Sqrt of expr
  
type stmt =
	Expr of expr
  |	Block of stmt list
  | If of expr * stmt * stmt
  | While of expr * stmt  
  
type stmts = 
	stmt list

type fdef = 
    Fdef of string * stmt

type fdefs =
	fdef list
	
type program = 
	stmts * fdefs
