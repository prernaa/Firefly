%{ open Ast %}

%token <int> INTEGER
%token <float> FLOAT
%token ASSIGN PLUS MINUS ON OFF
%token EOF
%token <(float * float)> VEC2
%token <string> IDENTIFIER
 
%left ASSIGN
%right ON OFF
%left PLUS MINUS
%nonassoc UMINUS UPLUS

%start program
%type <Ast.program> program

%%

program:		
	 stmts				{ List.rev $1 }
	  
stmts:	  
	 /* nothing */ 						{ [] }
	| stmts stmt						{ ($2 :: $1) }	

stmt:
	expr								{ Expr($1) }	
	
expr:
	constant							{ Constant($1) }
  | VEC2								{ Vec2(fst($1),snd($1)) }
  | IDENTIFIER							{ Identifier($1) }
  | IDENTIFIER ASSIGN expr				{ Assign ($1, $3)}
  | expr PLUS expr 						{ Binop($1, Add,   $3) }
  | expr ON expr 						{ Binop($1, On,   $3) }
  | expr OFF expr						{ Binop($1, Off,   $3) }
  | PLUS constant	%prec UPLUS			{ Constant($2) }
  | MINUS constant	%prec UMINUS		{ NegConstant($2) }
  
constant:
	INTEGER								{ Integer($1) }
  |	FLOAT								{ Float($1)	}

