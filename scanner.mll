{ open Parser }

let flt = ['-']?['0'-'9']+ ['.'] ['0'-'9']+

rule token = parse
  [' ' '\t' '\r' '\n'] 		{ token lexbuf } (* Whitespace *)
| '+'      { PLUS }
| '-'		{ MINUS }
| ['0'-'9']+ as lxm 		{ INTEGER(int_of_string lxm) }
| "on"						{ ON }
| "="						{ ASSIGN }
| flt as lxm 	{ FLOAT(float_of_string lxm) }
| ('[') ((['-''+']? ['0'-'9']+ ['.']? ['0'-'9']*) as num1) (',') ((['-''+']? ['0'-'9']+ ['.']? ['0'-'9']*) as num2) (']') 
							{ VEC2( (float_of_string num1) , (float_of_string num2) ) }
| eof { EOF }
| ['a'-'z' 'A'-'Z']+ ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm {IDENTIFIER(lxm)}
| _ as char 				{ raise (Failure("illegal character " ^ Char.escaped char)) }
