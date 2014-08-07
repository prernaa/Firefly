type token =
  | PRINT
  | INTEGER of (int)
  | FLOAT of (float)
  | PLUS
  | EOF
  | VEC2 of ((int * int))

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 14 "parser.ml"
let yytransl_const = [|
  257 (* PRINT *);
  260 (* PLUS *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  258 (* INTEGER *);
  259 (* FLOAT *);
  261 (* VEC2 *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\003\000\004\000\005\000\007\000\000\000\
\000\000\006\000"

let yydgoto = "\002\000\
\007\000\008\000"

let yysindex = "\002\000\
\255\254\000\000\000\000\000\000\000\000\000\000\000\000\005\255\
\003\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\001\000"

let yytablesize = 10
let yytable = "\003\000\
\004\000\005\000\001\000\006\000\004\000\005\000\002\000\006\000\
\009\000\010\000"

let yycheck = "\001\001\
\002\001\003\001\001\000\005\001\002\001\003\001\000\000\005\001\
\004\001\009\000"

let yynames_const = "\
  PRINT\000\
  PLUS\000\
  EOF\000\
  "

let yynames_block = "\
  INTEGER\000\
  FLOAT\000\
  VEC2\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 18 "parser.mly"
         ( Print )
# 78 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 19 "parser.mly"
          ( Exp(_1) )
# 85 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 22 "parser.mly"
                  ( Integer(_1) )
# 92 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 23 "parser.mly"
                      ( Float(_1) )
# 99 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : (int * int)) in
    Obj.repr(
# 24 "parser.mly"
         ( Vec2(fst(_1),snd(_1)) )
# 106 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 25 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 114 "parser.ml"
               : 'expr))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
