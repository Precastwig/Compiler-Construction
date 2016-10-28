%{
	open Types
%}

%token <int> INT
%token <string> STR
%token <string> STRING
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
%token TRUE
%token FALSE
%token AT
%token MAIN

%left COMMA
%left SEMICOLON
%left STR

%left WHILE DO 
%left IF
%left ELSE
%left LET NEW IN

%left OR
%left AND

%left EQUAL
%left LESS
%left GREATER

%left ASSIGN

%left PLUS
%left MINUS
%left DIVIDE
%left TIMES

%left AT
%left NOT
%left LEFTROUNDBRACKET  /* high precedence */
%start <Types.program> top
%%
top:
	| fl = separated_list( SEMICOLON , fundef ); SEMICOLON; m = maindef; SEMICOLON; EOF { ( m , fl)  }
	| f = fundef; SEMICOLON; m = maindef; SEMICOLON; EOF 	{  (m, [f]) }
	| m = maindef; SEMICOLON; EOF					{ (m , []) }

maindef:
	| MAIN;  LEFTROUNDBRACKET; a = args; RIGHTROUNDBRACKET; LEFTBRACE; f = exp; RIGHTBRACE 	{ Types.Main ( a, f) }

fundef:
	| s = STR; LEFTROUNDBRACKET; a = args; RIGHTROUNDBRACKET; LEFTBRACE; f = exp; RIGHTBRACE 	{ Types.Fun (s, a, f) }

args:
	| l = separated_list( COMMA , STR );		{ l }

exp:
	| TRUE										{ Types.True }
	| FALSE										{ Types.False } 
	| e = exp; ASSIGN; p = exp					{ Types.Asg (e, p) }
	| LEFTROUNDBRACKET; e = exp; RIGHTROUNDBRACKET { e }
	| LEFTBRACE; e = exp; RIGHTBRACE			{ e }
	| e = exp; SEMICOLON; p = exp				{ Types.Seq (e, p) }
	| e = exp; COMMA; p = exp				{ Types.Seq (e, p) }
	| e = exp; SEMICOLON;						{ e }
	| WHILE; e = exp; DO; LEFTBRACE; p = exp; RIGHTBRACE { Types.While (e, p) }
	| IF; e = exp; DO; p = exp;					{ Types.If (e, p) }
	| IF; e = exp; DO; p = exp; ELSE; f = exp 	{ Types.Ifelse (e, p, f) }

	| e = exp; PLUS; p = exp 				{ Types.Operator (Plus, e, p) }
	| e = exp; MINUS; p = exp 				{ Types.Operator (Minus, e, p) }
	| e = exp; TIMES; p = exp 				{ Types.Operator (Times, e, p) }
	| e = exp; DIVIDE; p = exp 				{ Types.Operator (Divide, e, p) }
	| e = exp; EQUAL; p = exp 				{ Types.Operator (Equal, e, p) }
	| e = exp; LESS; EQUAL; p = exp 				{ Types.Operator (Leq, e, p) }
	| e = exp; GREATER; EQUAL; p = exp 				{ Types.Operator (Geq, e, p) }
	| e = exp; GREATER; p = exp				{ Types.Operator (Greater, e, p) }
	| e = exp; LESS; p = exp				{ Types.Operator (Less, e, p) }
	| e = exp; NOT; EQUAL; p = exp 				{ Types.Operator (Noteq, e, p) }
	| e = exp; AND; p = exp 				{ Types.Operator (And, e, p) }
	| e = exp; OR; p = exp 				{ Types.Operator (Or, e, p) }

	| NOT; p = exp 				{ Types.Not p }

	| i = INT									{ Types.Const i }
	| s = STRING								{ Types.String s }
	| s = STR									{ Types.Identifier s }
	| AT; e = exp								{ Types.Deref e }

	| e = exp; LEFTROUNDBRACKET; p = exp; RIGHTROUNDBRACKET { Types.Application (e, p) }

	| READINT; LEFTROUNDBRACKET; RIGHTROUNDBRACKET { Types.Readint }
	| PRINTINT; LEFTROUNDBRACKET; e = exp; RIGHTROUNDBRACKET { Types.Printint (e) }
	| LET; s = STR; EQUAL; e = exp; IN; f = exp	{ Types.Let (s, e, f) }
	| NEW; s = STR; EQUAL; e = exp; IN; f = exp { Types.New (s, e, f) }

