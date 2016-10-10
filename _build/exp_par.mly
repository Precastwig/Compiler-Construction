%token <int> INT
%token PLUS
%token TIMES
%token MINUS
%token DIVIDE
%token LESS
%token GREATER
%token EQUAL
%token AND
%token OR
%token NOT
%token EOF
%left OR	/* low precendence */
%left AND
%left EQUAL /* end of equals */
%left PLUS
%left MINUS
%left DIVIDE
%left TIMES 
%left NOT 		/* high precedence */
%start <int> top
%%
top:
	| e = exp; EOF		{ e }

exp:
	| i = INT			{ i }
	| e = exp; OR; f = exp	    { e }
	| e = exp; AND; f = exp    { e }
	| e = exp; EQUAL; f = exp  { e }
	| e = exp; NOT; EQUAL; f = exp  { e }
	| e = exp; LESS; EQUAL; f = exp    { e }
	| e = exp; GREATER; EQUAL; f = exp    { e }
	| e = exp; PLUS; f = exp { e }
	| e = exp; MINUS; f = exp { e }
	| e = exp; DIVIDE; f = exp { e }
	| e = exp; TIMES; f = exp { e }
	| NOT; e = exp; 		{ e }
