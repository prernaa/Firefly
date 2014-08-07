%{ open Ast %}

%token PRINT
%token <int> INTEGER
%token <float> FLOAT
%token PLUS
%token EOF
%token <(int * int)> VEC2

%left PLUS

%start program
%type <Ast.program> program

%%

program:
	PRINT			{ Print }
	| expr			{ Exp($1) }
	
expr:
    INTEGER    			{ Integer($1) }
  |	FLOAT      	 	   	{ Float($1) }
  | VEC2	{ Vec2(fst($1),snd($1)) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
