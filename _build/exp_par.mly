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

%left COMMA
%left WHILE
%left DO
%left IF
%left ELSE
%left LET
%left IN
%left NEW
%left OR
%left AND
%left ASSIGN
%left EQUAL
%left LESS
%left GREATER
%left PLUS
%left MINUS
%left DIVIDE
%left TIMES
%left NOT
%left INT
%left READINT
%left PRINTINT
%left LEFTROUNDBRACKET
%left RIGHTROUNDBRACKET	
%left SEMICOLON
%left LEFTBRACE
%left RIGHTBRACE
%left STR
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
	| LEFTROUNDBRACKET; e = exp; RIGHTROUNDBRACKET { e }
	| e = exp; SEMICOLON; p = exp				{ Types.Seq (e, p) }
	| WHILE; e = exp; DO; p = exp				{ Types.While (e, p) }
	| IF; e = lexp; DO; p = exp					{ Types.If (e, p) }
	| IF; e = lexp; DO; p = exp; ELSE; f = exp 	{ Types.Ifelse (e, p, f) }
	| s = STR; ASSIGN; p = lexp					{ Types.Asg (s, p) }
	| NOT; e = exp								{ Types.Deref e }
	| e = exp; LEFTROUNDBRACKET; p = exp; RIGHTROUNDBRACKET { Types.Application (e, p) }
	| READINT; LEFTROUNDBRACKET; RIGHTROUNDBRACKET { Types.Readint }
	| PRINTINT; LEFTROUNDBRACKET; e = exp; RIGHTROUNDBRACKET { Types.Printint (e) }
	| LET; s = STR; EQUAL; e = exp; IN; f = exp	{ Types.Let (s, e, f) }
	| NEW; s = STR; EQUAL; e = exp; IN; f = exp { Types.New (s, e, f) }

lexp:
	| LEFTROUNDBRACKET; e = lexp; RIGHTROUNDBRACKET { e }
	| e = lexp; o = opcode; p = lexp 				{ Types.Operator (o, e, p) }
	| i = INT									{ Types.Const i }
	| s = STR									{ Types.Identifier s }

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
