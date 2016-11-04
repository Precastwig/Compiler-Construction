open Types
open Printf

(*let functionsize p =
	match p with
	| Operator (o, f, p) -> 1 + (functionsize f) + (functionsize p)
	| Not f	-> 1 + (functionsize f)
	| Const x -> 1
	| True -> 1
	| False -> 1
	| String s -> 1
	| Asg (a, e) -> 1 + (functionsize a) + (functionsize e)
	| Let (x, e, f) -> 1 + (functionsize x) + (functionsize e) + (functionsize f)
	| New (x, e, f) -> 1 + (functionsize x) + (functionsize e) + (functionsize f)
	| Seq (f, p) -> (functionsize f) + (functionsize p)
	| Ifelse (e, p, f) -> 1 + (functionsize e) + (functionsize p) + (functionsize f)
	| If (f, p) -> 1 + (functionsize f) + (functionsize p)
	| While (f, p) 1 + (functionsize f) + (functionsize p)
	| Application(Identifier f, p) ->  *)

let rec read_to_empty buf = 
	let s = read_line() in
	if s = "" then buf
	else (Buffer.add_string buf s;
			Buffer.add_string buf "\n";
			read_to_empty buf)

let rec printi i = if i > 0 then " " ^ printi (i-1) else ""

let functionlist = []

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
	| Asg (s, p) 		-> 	printi i ^ "Assignment of (\n" 	^ printexp s i ^ ",\n" ^ printexp p i ^ ")"
	| Deref f			->	printi i ^ "Dereference of \n" 	^ printexp f i
	| Operator (o, f, p)-> 	printi i ^ "Operator of (\n" 	^ printexp f i ^ ",\n" ^ printopcode o (i+4) ^ ",\n" ^ printexp p i ^ ")"
	| Application (f, p)-> 	printi i ^ "Application of (\n" 	^ printexp f i ^ ",\n" ^ printexp p i ^ ")"
	| Const x			-> 	printi i ^ "Const of " 			^ string_of_int x
	| Readint 			-> 	printi i ^ "Readint"
	| Printint f 		->	printi i ^ "print_int of " 		^ printexp f i
	| Identifier s		-> 	printi i ^ "Identifier of " 	^ s
	| Let (s, f, p)		-> 	printi i ^ "Let of (\n" 			^ s ^ ",\n" ^ printexp f i ^ ",\n" ^ printexp p i ^ ")"
	| New (s, f, p)		-> 	printi i ^ "New of (\n" 			^ s ^ ",\n" ^ printexp f i ^ ",\n" ^ printexp p i ^ ")"
	| True -> printi i ^ "True"
	| False -> printi i ^ "False"
	| String s -> printi i ^ "String '" ^ s ^ "'"
	| _ -> "unkown, print error"
	

let printfunc f =
	let (s, sl, e) = f in
		s ^ "\n arguments (" ^ stringlist sl ^ ")\n" ^ printexp e 0

let rec printlistfunctions l =
	match l with
	| [] -> ""
	| x :: [] -> "function (" ^ printfunc x ^ ")\n"
	| x :: xs -> "function (" ^ printfunc x ^ "),\n" ^ printlistfunctions xs
	
(*This is the main function that prints a type of program*)
let outputprog prog = "[" ^ printlistfunctions prog ^ "]" ^ "\n"

let rec maybetostr f =
	match f with
		| Mstring s -> s
		| Cons x 	-> string_of_int x 
		| Mtrue 	-> "true"
		| Mfalse	-> "false"
		| Unit		-> "unit"
	
let tobool e = 
	match e with
	| Cons x -> if x = 0 then false else true
	| Mstring s -> if s = "" then false else true
	| Mtrue -> true
	| Mfalse -> false
	| Unit -> false
	
let maybetoint e =
	match e with
	| Cons x -> x
	| Mstring s -> String.length s
	| Mtrue -> 1
	| Mfalse -> 0
	| Unit -> 0
