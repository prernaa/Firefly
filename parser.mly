%{ open Ast %}

%token <int> INTEGER
%token <float> FLOAT
%token ASSIGN PLUS MINUS ON
%token EOF
%token <(float * float)> VEC2
%token <string> IDENTIFIER
 
%left ASSIGN
%right ON
%left PLUS MINUS

%start program
%type <Ast.program> program

%%

program:		
	 exprs				{ List.rev $1 }
	  
exprs:	  
	 /* nothing */ { [] }
	| exprs expr				{ ($2 :: $1) }	
	
expr:
    INTEGER    							{ Integer($1) }
  |	FLOAT      	 	   					{ Float($1) }
  | VEC2								{ Vec2(fst($1),snd($1)) }
  | IDENTIFIER							{ Identifier($1) }
  | IDENTIFIER ASSIGN expr				{ Assign ($1, $3)}
  | expr PLUS expr 						{ Binop($1, Add,   $3) }
  | expr ON expr 						{ Binop($1, On,   $3) }