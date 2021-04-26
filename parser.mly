%token <float> FLOAT
%token EXP
%token MULT
%token DIVIDE
%token PLUS
%token SUB
%token LPAREN
%token RPAREN
%token <string> VARIABLE
%token EOF

%left PLUS
%left SUB
%left MULT
%left DIVIDE


%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	

	
expr:
	| i = FLOAT { Float i }
	| coeff = FLOAT; var = VARIABLE; { Poly (coeff, var, 1.) }
	| var = VARIABLE;{ Var (var) }
	| var = VARIABLE; EXP; exponent = FLOAT { Poly (1., var, exponent) }
	| coeff = FLOAT; var = VARIABLE; EXP; exponent = FLOAT { Poly (coeff, var, exponent) }
	| e1 = expr; MULT; e2 = expr { Binop (Mult, e1, e2) } 
	| e1 = expr; DIVIDE; e2 = expr { Binop (Divide, e1, e2) } 
	| e1 = expr; SUB; e2 = expr { Binop (Sub, e1, e2) } 
	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
	| e1 = FLOAT; EXP; exp = FLOAT { Binop (Exp, Float e1 , Float exp)}
	| LPAREN; e=expr; RPAREN {e} 
	;
	
