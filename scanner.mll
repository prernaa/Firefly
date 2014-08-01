{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| '+'      { PLUS }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| "print" { PRINT }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
