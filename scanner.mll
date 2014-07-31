{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "print" { PRINT }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
