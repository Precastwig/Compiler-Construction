open Types
open Exp_lex
open Lexing
open Printf

let rec read_to_empty buf = 
	let s = read_line() in
	if s = "" then buf
	else (Buffer.add_string buf s;
			Buffer.add_string buf "\n";
			read_to_empty buf)

let rec printi i = if i > 0 then " " ^ printi (i-1) else ""

let rec stringlist sl =
	match sl with
	| [] -> ""
	| x :: [] -> x
	| x :: xs -> x ^ "," ^ stringlist xs

let printopcode o i =
	match o with
	| Plus	-> printi i ^ "PLUS"
	| Minus	-> printi i ^ "MINUS"
	| Times	-> printi i ^ "TIMES"
	| Divide-> printi i ^ "DIVIDE"
	| Leq	-> printi i ^ "LESS OR EQUAL"
	| Geq	-> printi i ^ "GREATER OR EQUAL"
	| Equal	-> printi i ^ "EQUAL"
	| Noteq	-> printi i ^ "NOT EQUAL"
	| And	-> printi i ^ "AND"
	| Or	-> printi i ^ "OR"
	| Not 	-> printi i ^ "NOT"
	| Greater -> printi i ^ "GREATER"
	| Less	-> printi i ^ "LESS"


let rec printexp e i= 
	let i = i + 4 in
	match e with
	| Seq (f, p) 		-> 	printi i ^ "Seq of (\n" 		^ printexp f (i-4) ^ ",\n" ^ printexp p (i-4) ^ ")"
	| While (f, p) 		-> 	printi i ^ "While of (\n" 		^ printexp f i ^ ",\n" ^ printexp p i ^ ")"
	| Ifelse (f, p, q)	-> 	printi i ^ "Ifelse of (\n" 		^ printexp f i ^ ",\n" ^ printexp p i ^ ",\n" ^ printexp q i ^ ")"
	| If (f, p)			->	printi i ^ "If of (\n" 			^ printexp f i ^ ",\n" ^ printexp p i ^ ")"
	| Asg (s, p) 		-> 	printi i ^ "Assignment of (\n" 	^ printi (i+4) ^ s ^ ",\n" ^ printexp p i ^ ")"
	| Deref f			->	printi i ^ "Dereference of \n" 	^ printexp f i
	| Operator (o, f, p)-> 	printi i ^ "Operator of (\n" 	^ printexp f i ^ ",\n" ^ printopcode o (i+4) ^ ",\n" ^ printexp p i ^ ")"
	| Application (f, p)-> 	printi i ^ "Application of (\n" 	^ printexp f i ^ ",\n" ^ printexp p i ^ ")"
	| Const x			-> 	printi i ^ "Const of " 			^ string_of_int x
	| Readint 			-> 	printi i ^ "Readint"
	| Printint f 		->	printi i ^ "print_int of " 		^ printexp f i
	| Identifier s		-> 	printi i ^ "Identifier of " 	^ s
	| String s 			->  printi i ^ "String of " 		^ s
	| Let (s, f, p)		-> 	printi i ^ "Let of (\n" 			^ s ^ ",\n" ^ printexp f i ^ ",\n" ^ printexp p i ^ ")"
	| New (s, f, p)		-> 	printi i ^ "New of (\n" 			^ s ^ ",\n" ^ printexp f i ^ ",\n" ^ printexp p i ^ ")"

let printfunc f =
	let (s, sl, e) = f in
		s ^ "\n arguments (" ^ stringlist sl ^ ")\n" ^ printexp e 0

let rec printlistfunctions l =
	match l with
	| [] -> ""
	| x :: [] -> "function (" ^ printfunc x ^ ")\n"
	| x :: xs -> "function (" ^ printfunc x ^ "),\n" ^ printlistfunctions xs
	

let outputprog prog = "[" ^ printlistfunctions prog ^ "]" ^ "\n"

let load_file f =
	let ic = open_in f in
	let n = in_channel_length ic in
	let s = String.create n in
	really_input ic s 0 n;
	close_in ic;
	(s)

let print_position lexbuf =
	let pos = lexbuf.lex_curr_p in
	eprintf "Pos %d:%d:%d\n" pos.pos_lnum pos.pos_bol pos.pos_cnum

let parsewitherror lexbuf =
	try Exp_par.top Exp_lex.read lexbuf with
	| SyntaxError msg -> prerr_string (msg ^ ": ");
						print_position lexbuf;
						exit (-1)
	| Exp_par.Error -> prerr_string "Parse error: ";
						print_position lexbuf;
						exit (-1)

let _ = 
	load_file "bisection.ml"
	|> Lexing.from_string 
	|> parsewitherror
	|> outputprog
	|> print_string;
