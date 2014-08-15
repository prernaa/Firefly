%{ open Ast %}

%token <int> INTEGER
%token <float> FLOAT
%token LESS LESSEQ GREATER GREATEREQ EQUALSTO NOTEQUALS
%token AND OR NOT
%token LPAREN RPAREN LBRACE RBRACE
%token OPENVEC COMMA CLOSEVEC LET
%token SQRT SIN COS ASSIGN PLUS MINUS TIMES DIVIDE ON OFF IF ELSE WHILE ENDIF
%token EOF
%token <(float * float)> VEC2
%token GETX GETY
%token <string> IDENTIFIER
 
%left LPAREN RPAREN
%right SIN COS
%right SQRT
%right ASSIGN
%left OR
%left AND
%right NOT
%nonassoc LESS LESSEQ GREATER GREATEREQ EQUALSTO NOTEQUALS
%right ON OFF
%left OPENVEC COMMA CLOSEVEC
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc GETX GETY
%nonassoc UMINUS UPLUS

%start program
%type <Ast.program> program

%%

program:			
   /* nothing */ { [] }
 | program stmt { ($2 :: $1) }
	  
stmts:	  
	 /* nothing */ 						{ [] }
	| stmts stmt						{ ($2 :: $1) }		

stmt:
	expr_stmt							{ Expr($1) }	
  |	LBRACE stmts RBRACE					{ Block(List.rev $2) }
  | IF expr stmt ELSE stmt ENDIF		{ If($2, $3, $5) }
  | WHILE expr stmt						{ While($2, $3) }  
  | IDENTIFIER LPAREN RPAREN			{ Call ($1) }
  | LET IDENTIFIER LPAREN formals_opt RPAREN ASSIGN stmt
										{ Fdef($2, $4, $7) }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 } 

formal_list:
    IDENTIFIER                   { [$1] }
  /*| formal_list COMMA ID { $3 :: $1 } */
										
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
  | SQRT expr							{ Sqrt($2) }
  | SIN expr							{ Sin($2) }
  | COS expr							{ Cos($2) }
  | expr GETX							{ Getx($1) }
  | expr GETY							{ Gety($1) }
  
constant:
	INTEGER								{ Integer($1) }
  |	FLOAT								{ Float($1)	}
										
