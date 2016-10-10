{
open Exp_par
exception SyntaxError of string
}

let int = ['0'-'9'] ['0'-'9']*
let var = ['a'-'Z'] ['a'-'Z']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = 
	parse
	| white { read lexbuf }
	| newline { read lexbuf }
	| int 	{ INT (int_of_string (Lexing.lexeme lexbuf)) }
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
