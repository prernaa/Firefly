%{ open Ast %}

%token PRINT
%token PLUS
%token EOF

%left PLUS

%start program
%type <Ast.program> program

%%

program:
	PRINT			{ Print }
	
expr:
    LITERAL          { Literal($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
