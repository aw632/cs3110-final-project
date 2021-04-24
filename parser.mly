%token <float> FLOAT
%token TIMES
%token PLUS
%token SUB
%token <float> EXP
%token LPAREN
%token RPAREN
%token <string> VARIABLE
%token EOF

%left PLUS
%left SUB
%left TIMES
%left EXP

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
	| i = FLOAT { Float i }
	| coeff = FLOAT; var = VARIABLE; exp = EXP { Poly (coeff, var, exp) }
	| var = VARIABLE; exp = EXP { Poly (1., var, exp) }
	| var = VARIABLE;{ Var (var) }
	| e1 = expr; TIMES; e2 = expr { Binop (Mult, e1, e2) } 
	| e1 = expr; SUB; e2 = expr { Binop (Sub, e1, e2) } 
	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
	| LPAREN; e=expr; RPAREN {e} 
	;
	
