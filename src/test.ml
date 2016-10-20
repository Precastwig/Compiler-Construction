open Types
open Lex
open Lexing
open Hashtbl
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
	let s = Bytes.create n in
	really_input ic s 0 n;
	close_in ic;
	(s)

let print_position lexbuf =
	let pos = lexbuf.lex_curr_p in
	eprintf "Pos %d:%d:%d\n" pos.pos_lnum pos.pos_bol pos.pos_cnum

let store = Hashtbl.create 100

type maybe = 
	| Cons of int
	| True
	| False
	| Unit

let deref e =
	match e with
	| Cons x -> x
	| True -> 1
	| False -> 0	

let rec evaltonum e =
	match e with
	| Seq (f, p)			-> let _ = evaltonum f in evaltonum p
	| Const x	 			-> Cons x
	| Operator (Plus, f, p)	-> Cons (deref (evaltonum f) + deref (evaltonum p))
	| Operator (Minus, f, p)-> Cons (deref (evaltonum f) - deref (evaltonum p))
	| Operator (Times, f, p)-> Cons (deref (evaltonum f) * deref (evaltonum p))
	| Operator (Divide, f,p)-> Cons (deref (evaltonum f) / deref (evaltonum p))
	| Operator (Leq,f, p)	-> if deref (evaltonum f) <= deref (evaltonum p) then True else False
	| Operator (Geq,f, p)	-> if deref (evaltonum f) >= deref (evaltonum p) then True else False
	| Operator (Equal,f,p)	-> if deref (evaltonum f) == deref (evaltonum p) then True else False
	| Operator (Noteq,f,p)	-> if deref (evaltonum f) != deref (evaltonum p) then True else False
	| Operator (And,f,p)	-> if deref (evaltonum f) == 1 && deref (evaltonum p) == 1 then True else False
	| Operator (Or, f, p)	-> if deref (evaltonum f) == 1 || deref (evaltonum p) == 1 then True else False
	| Operator (Not, f, p) 	-> if deref (evaltonum f) == 1 then False else True
	| Operator (Greater,f,p)-> if deref (evaltonum f) > deref (evaltonum p) then True else False
	| Operator (Less,f,p)	-> if deref (evaltonum f) < deref (evaltonum p) then True else False
	| Asg (Identifier x, e) -> let v = deref (evaltonum e) in Hashtbl.replace store x v; Cons v
	| Seq (f, p)			-> let _ = evaltonum f in
								let v = evaltonum p in v
	| Ifelse (e, p, f)		-> if deref (evaltonum e) == 1 then evaltonum p else evaltonum f
	| If (f, p)				-> if deref (evaltonum f) == 1 then evaltonum p else Unit
	| Readint				-> Unit
	| Printint(e)			-> evaltonum e
	| Identifier x 			-> Cons (Hashtbl.find store x)
	| _						-> eprintf "Not implemented"; Unit
	
let fundeftonum p =
	let (s, sl, e) = p in
	evaltonum e

let rec evaltonumprogram p = 
	match p with
	| x :: [] -> string_of_int ( deref (fundeftonum x) )
	| x :: xs -> string_of_int ( deref (fundeftonum x) ) ^ "\n" ^ evaltonumprogram xs

let parsewitherror lexbuf =
	try Par.top Lex.read lexbuf with
	| SyntaxError msg -> prerr_string (msg ^ ": ");
						print_position lexbuf;
						exit (-1)
	| Par.Error -> prerr_string "Parse error: ";
						print_position lexbuf;
						exit (-1)

let basicone =
	(load_file "tests/basic1.ml"
	|> Lexing.from_string 
	|> parsewitherror
	|> evaltonumprogram)

let basictwo = 
	(load_file "tests/basic2.ml"
	|> Lexing.from_string 
	|> parsewitherror
	|> evaltonumprogram)

let basicthree = 
	(load_file "tests/basic3.ml"
	|> Lexing.from_string 
	|> parsewitherror
	|> evaltonumprogram)

let basicfour = 
	(load_file "tests/basic4.ml"
	|> Lexing.from_string 
	|> parsewitherror
	|> evaltonumprogram)

let basicfive = 
	(load_file "tests/basic5.ml"
	|> Lexing.from_string 
	|> parsewitherror
	|> evaltonumprogram)

let doalltests =
	"basic1: " ^ basicone ^ " should be 10\n" ^ 
	"basic2: " ^ basictwo ^ " should be 3\n" ^
	"basic3: " ^ basicthree ^ " should be 7\n" ^
	"basic4: " ^ basicfour ^ " should be 7\n" ^
	"basic5: " ^ basicfive ^ " should be 5\n"
	"basic6: is not applicible for this exercise (let and new)\n" ^
	"bisection: is not applicable for this exercise (not imperitive)\n"
	
 

(* After parsewitherror we can change between evaltonumprogram or outputprog *)
let _ = 
	doalltests
	|> print_string;
