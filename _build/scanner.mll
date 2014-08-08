{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] 		{ token lexbuf } (* Whitespace *)
| '+'      { PLUS }
| ['0'-'9']+ as lxm 		{ INTEGER(int_of_string lxm) }
| "on"						{ ON }
| (['0'-'9']+ ['.'] ['0'-'9']+) as lxm 	{ FLOAT(float_of_string lxm) }
| ('[') ((['0'-'9']+) as num1) (',') ((['0'-'9']+) as num2) (']') 
							{ VEC2( (float_of_string num1) , (float_of_string num2) ) }
| eof { EOF }
| _ as char 				{ raise (Failure("illegal character " ^ Char.escaped char)) }