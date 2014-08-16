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
  | Sin of expr
  | Cos of expr
  | Getx of expr
  | Gety of expr
  | Local of int
  
type stmt =
	Expr of expr
  |	Block of stmt list
  | If of expr * stmt * stmt
  | While of expr * stmt  
  | Call of string * (expr list)
  | Fdef of string * int * stmt
  
type stmts = 
	stmt list

(*type fdef = *)
    

(*type fdefs =
	fdef list*)
	
type program = 
	stmts