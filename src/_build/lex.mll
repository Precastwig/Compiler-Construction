{
open Par
exception SyntaxError of string
}

let int = ['0'-'9'] ['0'-'9']*
let wloop = "while"
let d = "do"
let white = [' ' '\t']+
let str = ['a'-'z' 'A'-'Z' '_' '0'-'9'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let comment = "/*" (str | '\n'| '\r' | "\r\n" | ' ' | '(' | ')' | '{' | '}' | '=' | ',' | ';' | '|' | '<' | '>' | '/' | '+' | '*' | '-' | '!' | ':' | '/')* "*/"
let quote = "\"" ['a'-'z' 'A'-'Z' '_' '0'-'9' ' ' '.' ',' ';' '@' '#']* "\""
let newline = '\r' | '\n' | "\r\n"

rule read = 
	parse
	| white { read lexbuf }
	| newline { read lexbuf }
	| comment { read lexbuf }
	| int 	{ INT (int_of_string (Lexing.lexeme lexbuf)) }
	| wloop { WHILE }
	| d		{ DO }	
	| quote { STRING (Lexing.lexeme lexbuf) }
	| "true" { TRUE }
	| "false" { FALSE }
	| "if"	{ IF }
	| "else"	{ ELSE }
	| ":="	{ ASSIGN }
	| "read_int" { READINT }
	| "print_int" { PRINTINT }
	| "let"	{ LET }
	| "in"	{ IN }
	| "new"	{ NEW }
	| str	{ STR (Lexing.lexeme lexbuf) }
	| '{'	{ LEFTBRACE }
	| '}'	{ RIGHTBRACE }
	| '('	{ LEFTROUNDBRACKET }
	| ')'	{ RIGHTROUNDBRACKET }
	| ','	{ COMMA }
	| ';'	{ SEMICOLON }
	| '|'  { OR }
	| '&'  { AND }
	| '='	{ EQUAL }
	| '<'	{ LESS }
	| '>'	{ GREATER }
	| '+'	{ PLUS }
	| '-'	{ MINUS }
	| '/'	{ DIVIDE }
	| '*' 	{ TIMES }
	| '!'	{ NOT }
	| _		{ raise (SyntaxError ("Unexpected char: ")) }
	| eof 	{ EOF }
