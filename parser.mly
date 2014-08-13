%{ open Ast %}

%token <int> INTEGER
%token <float> FLOAT
%token LESS LESSEQ GREATER GREATEREQ EQUALSTO NOTEQUALS
%token AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE
%token OPENVEC COMMA CLOSEVEC LET
%token ASSIGN PLUS MINUS TIMES DIVIDE ON OFF IF ELSE WHILE
%token EOF
%token <(float * float)> VEC2
%token <string> IDENTIFIER
 
%left LPAREN RPAREN
%right ASSIGN
%left OR
%left AND
%left NOT
%nonassoc LESS LESSEQ GREATER GREATEREQ EQUALSTO NOTEQUALS
%right ON OFF
%left OPENVEC COMMA CLOSEVEC
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS UPLUS

%start program
%type <Ast.program> program

%%

program:		
	stmts fdefs							{ (List.rev $1, List.rev $2) }	
	
fdefs:
	/* nothing */    					{ [] }
  |	fdefs fdef 							{ ($2 :: $1) }
	
fdef:	
    LET IDENTIFIER LPAREN RPAREN ASSIGN LBRACE stmt RBRACE
										{ Fdef($2, $7) }
	  
stmts:	  
	 /* nothing */ 						{ [] }
	| stmts stmt						{ ($2 :: $1) }	

stmt:
	expr_stmt							{ Expr($1) }	
  |	LBRACE stmts RBRACE					{ Block(List.rev $2) }
  | IF expr stmt ELSE stmt    			{ If($2, $3, $5) }
  | WHILE expr stmt						{ While($2, $3) }  

vec2:
	OPENVEC expr COMMA expr CLOSEVEC	{ Vec2($2,$4) }	

expr_stmt:
	IDENTIFIER ASSIGN expr				{ Assign ($1, $3)}
  | expr ASSIGN expr					{ raise (Failure ("Cannot have an expression on LHS of assignment!" ) ) }
  | expr ON expr 						{ Binop($1, On,   $3) }
  | expr OFF expr						{ Binop($1, Off,   $3) }
	
expr:
	constant							{ Constant($1) }
  | expr_stmt							{ $1 }
  | vec2								{ $1 }
  | IDENTIFIER							{ Identifier($1) }
  | expr PLUS expr 						{ Binop($1, Add,   $3) }
  | expr MINUS expr						{ Binop($1, Minus, $3) }
  | expr TIMES expr						{ Binop($1, Multiply, $3) }
  | expr DIVIDE expr					{ Binop($1, Divide, $3) }
  | expr LESS expr						{ Binop($1, LessThan, $3) }
  | expr LESSEQ expr					{ Binop($1, LessThanEq, $3) }
  | expr GREATER expr					{ Binop($1, GreaterThan, $3) }
  | expr GREATEREQ expr					{ Binop($1, GreaterThanEq, $3) }
  | expr EQUALSTO expr					{ Binop($1, EqualsTo, $3) }
  | expr NOTEQUALS expr					{ Binop($1, NotEqualsTo, $3) }
  | expr OR expr						{ Binop($1, Or, $3) }
  | expr AND expr						{ Binop($1, And, $3) }
  | NOT expr							{ Not($2) }
  | PLUS constant	%prec UPLUS			{ Constant($2) }
  | MINUS constant	%prec UMINUS		{ NegConstant($2) }
  | LPAREN expr RPAREN 					{ $2 }
  
constant:
	INTEGER								{ Integer($1) }
  |	FLOAT								{ Float($1)	}
										
