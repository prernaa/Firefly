%{ open Ast %}

%token PRINT
%token EOF

%start program
%type <Ast.program> program

%%

program:
	PRINT			{ Print }
