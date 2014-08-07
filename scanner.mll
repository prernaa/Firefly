{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| '+'      { PLUS }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| "print" { PRINT }
| ('[') ((['0'-'9']+) as num1) (',') ((['0'-'9']+) as num2) (']') { VEC2( (int_of_string num1) , (int_of_string num2) ) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
