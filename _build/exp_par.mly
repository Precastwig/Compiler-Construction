%{
	open Types
%}

%token <int> INT
%token <string> STR
%token LEFTBRACE
%token RIGHTBRACE
%token LEFTROUNDBRACKET
%token RIGHTROUNDBRACKET
%token COMMA
%token SEMICOLON
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
%token WHILE
%token DO
%token IF
%token ELSE
%token ASSIGN
%token READINT
%token PRINTINT
%token LET
%token IN
%token NEW
%left ASSIGN
%left OR
%left AND
%left EQUAL
%left LESS
%left GREATER
%left PLUS
%left MINUS
%left DIVIDE
%left TIMES
%left WHILE
%left DO
%left IF
%left ELSE
%left NOT
%left INT
%left COMMA
%left SEMICOLON
%left LEFTROUNDBRACKET
%left RIGHTROUNDBRACKET	
%left EOF 	/* high precedence */
%start <Types.program> top
%%
top:
	| el = separated_list( SEMICOLON , fundef ); EOF		{ el }

fundef:
	| s = STR; LEFTROUNDBRACKET; a = args; RIGHTROUNDBRACKET; LEFTBRACE; f = exp; RIGHTBRACE 	{ (s, a, f) }

args:
	| l = separated_list( COMMA , STR );		{ l }

exp:
	| e = exp; SEMICOLON; p = exp				{ Types.Seq (e, p) }
	| WHILE; e = exp; DO; p = exp				{ Types.While (e, p) }
	| IF; e = exp; DO; p = exp; ELSE; f = exp 	{ Types.If (e, p, f) }
	| e = exp; ASSIGN; p = exp					{ Types.Asg (e, p) }
	| NOT; e = exp								{ Types.Deref e }
	| e = exp; o = opcode; p = exp 				{ Types	.Operator (o, e, p) }
	| e = exp; LEFTROUNDBRACKET; p = exp; RIGHTROUNDBRACKET { Types.Application (e, p) }
	| i = INT									{ Types.Const i }
	| READINT; LEFTROUNDBRACKET; RIGHTROUNDBRACKET { Types.Readint }
	| PRINTINT; LEFTROUNDBRACKET; e = exp; RIGHTROUNDBRACKET { Types.Printint (e) }
	| s = STR										{ Types.Identifier s }
	| LET; s = STR; EQUAL; e = exp; IN; f = exp	{ Types.Let (s, e, f) }
	| NEW; s = STR; EQUAL; e = exp; IN; f = exp { Types.New (s, e, f) }

opcode:
	| PLUS	{ Plus }
	| MINUS { Minus }
	| TIMES	{ Times }
	| DIVIDE { Divide}
	| EQUAL { Equal }
	| LESS; EQUAL { Leq }
	| GREATER; EQUAL { Geq }
	| NOT; EQUAL	{ Noteq }
	| AND	{ And }
	| OR	{ Or }
	| NOT	{ Not }
