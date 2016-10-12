open Types

let rec read_to_empty buf = 
	let s = read_line() in
	if s = "" then buf
	else (Buffer.add_string buf s;
			Buffer.add_string buf "\n";
			read_to_empty buf)

let rec printi i = if i > 0 then " " ^ printi (i-1) else ""

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
	| Not ->   printi i ^ "NOT"


let rec printexp e i= 
	let i = i + 4 in
	match e with
	| Seq (f, p) -> 	printi i ^ "Seq of (\n" 	^ printexp f i ^ ",\n " ^ printexp p i ^ ")"
	| While (f, p) -> 	printi i ^ "While of (\n" 	^ printexp f i ^ ",\n " ^ printexp p i ^ ")"
	| If (f, p, q)	-> 	printi i ^ "If of (\n" 		^ printexp f i ^ ",\n " ^ printexp p i ^ ",\n " ^ printexp q i ^ ")"
	| Asg (f, p) -> 	printi i ^ "Assignment of (\n" 	^ printexp f i ^ ",\n " ^ printexp p i ^ ")"
	| Deref f	->		printi i ^ "Dereference of \n" 	^ printexp f i
	| Operator (o, f, p) -> printi i ^ "Operator of (\n" 	^ printexp f i ^ ",\n " ^ printopcode o (i+4) ^ ",\n " ^ printexp p i ^ ")"
	| Application (f, p) -> printi i ^ "Application of (" 	^ printexp f i ^ ",\n " ^ printexp p i ^ ")"
	| Const x	-> 		printi i ^ "Const of " 		^ string_of_int x
	| Readint 	-> 		printi i ^ "Readint"
	| Printint f ->		printi i ^ "print_int of " 	^ printexp f i
	| Identifier s	-> 	printi i ^ "Identifier of " ^ s
	| Let (s, f, p)	-> 	printi i ^ "Let of (" ^ s ^ ",\n " ^ printexp f i ^ ",\n " ^ printexp p i ^ ")"
	| New (s, f, p)	-> 	printi i ^ "New of (" ^ s ^ ",\n " ^ printexp f i ^ ",\n " ^ printexp p i ^ ")"

let rec stringlist sl =
	match sl with
	| [] -> ""
	| x :: [] -> x
	| x :: xs -> x ^ "," ^ stringlist xs

let printfunc f =
	let (s, sl, e) = f in
		s ^ "\n arguments (" ^ stringlist sl ^ ")\n" ^ printexp e 0

let rec printlistfunctions l =
	match l with
	| [] -> ""
	| x :: [] -> "function " ^ printfunc x ^ "\n"
	| x :: xs -> "function " ^ printfunc x ^ ",\n" ^ printlistfunctions xs
	

let outputprog prog = "[" ^ printlistfunctions prog ^ "]"

let _ = 
	read_to_empty (Buffer.create 1)
	|> Buffer.contents
	|> Lexing.from_string 
	|> Exp_par.top Exp_lex.read 
	|> outputprog
	|> print_string;
