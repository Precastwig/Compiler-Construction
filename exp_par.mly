%token <int> INT
%token PLUS
%token TIMES
%token EOF
%left PLUS		/* low precendence */
%left TIMES 	/* high precedence */
%start <int> top
%%
top:
	| e = exp; EOF		{ e }

exp:
	| i = INT			{ i }
	| e = exp; PLUS; f = exp { e + f }
	| e = exp; TIMES; f = exp { e * f }
