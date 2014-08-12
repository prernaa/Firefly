{ open Parser }

let flt = ['0'-'9']+ ['.'] ['0'-'9']+

rule token = parse
  [' ' '\t' '\r' '\n'] 		{ token lexbuf } (* Whitespace *)
| "/*"		{comment lexbuf}
| '+'      	{ PLUS }
| '-'		{ MINUS }
| '('		{ LPAREN }
| ')'		{ RPAREN }
| '{'      	{ LBRACE }
| '}'      	{ RBRACE }
| '['		{ OPENVEC }
| ','		{ COMMA }
| ']'		{ CLOSEVEC }
| '<'		{ LESS }
| "<="		{ LESSEQ }
| '>'		{ GREATER }
| ">="		{ GREATEREQ }
| "=="		{ EQUALSTO }
| ['0'-'9']+ as lxm 		{ INTEGER(int_of_string lxm) }
| "on"						{ ON }
| "off"						{ OFF }
| "="						{ ASSIGN }
| flt as lxm 	{ FLOAT(float_of_string lxm) }
| "if"     	{ IF }
| "else"   	{ ELSE }
| eof { EOF }
| ['a'-'z' 'A'-'Z']+ ['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm {IDENTIFIER(lxm)}
| _ as char 				{ raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/"		{token lexbuf}
| _			{comment lexbuf}
