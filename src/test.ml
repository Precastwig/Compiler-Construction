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

(*These functions print a type of program*)

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

(*Code that loads a file *)
let load_file f =
	let ic = open_in f in
	let n = in_channel_length ic in
	let s = Bytes.create n in
	really_input ic s 0 n;
	close_in ic;
	(s)

(*Error handling position code*)
let print_position lexbuf =
	let pos = lexbuf.lex_curr_p in
	eprintf "Pos %d:%d:%d\n" pos.pos_lnum pos.pos_bol pos.pos_cnum

(*This code evaluates a type of program*)

let rec argsexpcreate store values names =
	match names, values with
	| [], [] -> ()
	| [], x::xs -> eprintf "Error wrong number of arguments to function\n"; ()
	| x :: xs, [] -> eprintf "Error wrong number of arguments to function\n"; ()
	| n :: ns, v::vs -> Hashtbl.add store n v; argsexpcreate store vs ns

let string_repeat str x =
	let len = Bytes.length str in
	let res = Bytes.create(x * len) in
	for i = 0 to pred x do
		Bytes.blit str 0 res (i * len) len
	done;
	Bytes.to_string res
	
let tobool e = 
	match e with
	| Cons x -> if x = 0 then false else true
	| Mstring s -> if s = "" then false else true
	| Mtrue -> true
	| Mfalse -> false
	
let maybetoint e =
	match e with
	| Cons x -> x
	| Mstring s -> String.length s
	| Mtrue -> 1
	| Mfalse -> 0

let lookup i store = 
	match i with
	| Identifier s -> (try Hashtbl.find store s with
											| Not_found -> eprintf "Variable '%s' is not defined\n" s; Unit
											| e -> raise e )

let rec evalexplist env store e = 
	match e with
	| Const x -> [Cons x]
	| True 	 -> [Mtrue]
	| False  -> [Mfalse]
	| String s -> [Mstring s]
	| Identifier x 			-> [lookup (Identifier x) env]
	| Deref (Identifier e) 	-> [lookup (Identifier e) store]
	| Seq(f, s) -> List.rev_append (evalexplist env store f) (evalexplist env store s)

let rec printfl fl =
	match fl with
	| [] -> "fin\n"
	| Fun (s, sl, exp) :: xs -> s ^ "\n" ^ printfl xs

let rec findfunc fl n =
	match fl with
	| [] -> eprintf "Error, function does not exist\n"; ([],Ref Unit)
	| Fun (s, sl, exp) :: xs -> if s = n then (sl, exp) else findfunc xs n

let rec evalexp env store e fl =
	(match e with
	| Const x	 			-> Cons x
	| True					-> Mtrue
	| False					-> Mfalse
	| String s				-> Mstring s
	| Operator (Plus, f, p)	-> let k = evalexp env store f fl in
								let j = evalexp env store p fl in
								( match k with
								| Cons x -> ( match j with 
											| Cons y 	-> Cons ( x + y )
											| Mstring y -> Mstring ( string_of_int x ^ y )
											| _ 		-> eprintf "Plus Not implemented"; Unit )
								| Mstring x -> ( match j with
												| Cons y 	-> Mstring ( x ^ string_of_int y )
												| Mstring y	-> Mstring ( x ^ y )
												| _			-> eprintf "Plus Not implemented"; Unit )
								| _ 		-> eprintf "Plus Not implemented"; Unit )
	| Operator (Minus, f, p)-> let k = evalexp env store f fl in
								let j = evalexp env store p fl in
								( match k with
								| Cons x -> ( match j with 
											| Cons y 	-> Cons ( x - y )
											| Mstring y -> eprintf "Minus Not implemented"; Unit
											| _ 		-> eprintf "Minus Not implemented"; Unit )
								| Mstring x -> ( match j with
												| Cons y 	-> Mstring ( String.sub x 0 ((String.length x) - y) )
												| Mstring y	-> eprintf "Minus Not implemented"; Unit
												| _			-> eprintf "Minus Not implemented"; Unit )
								| _ 		-> eprintf "Minus Not implemented"; Unit )
	| Operator (Times, f, p)-> let k = evalexp env store f fl in
								let j = evalexp env store p fl in
								( match k with
								| Cons x -> ( match j with 
											| Cons y 	-> Cons ( x * y )
											| Mstring y -> Mstring ( string_repeat y x )
											| _ 		-> eprintf "Times Not implemented"; Unit )
								| Mstring x -> ( match j with
												| Cons y 	-> Mstring ( string_repeat x y )
												| Mstring y	-> Mstring ( x ^ y )
												| _			-> eprintf "Times Not implemented"; Unit )
								| _ 		-> eprintf "Times Not implemented"; Unit )
	| Operator (Divide, f,p)-> let k = evalexp env store f fl in
								let j = evalexp env store p fl in
								( match k with
								| Cons x -> ( match j with 
											| Cons y 	-> Cons ( x / y )
											| Mstring y -> eprintf "Divide Not implemented"; Unit
											| _ 		-> eprintf "Divide Not implemented"; Unit )
								| Mstring x -> ( match j with
												| Cons y 	-> eprintf "Divide Not implemented"; Unit
												| Mstring y	-> eprintf "Divide Not implemented"; Unit
												| _			-> eprintf "Divide Not implemented"; Unit )
								| _ 		-> eprintf "Divide Not implemented"; Unit )
	| Operator (Leq,f, p)	-> let k = evalexp env store f fl in
								let j = evalexp env store p fl in
								( match k with
								| Cons x -> ( match j with 
											| Cons y 	-> if x <= y then Mtrue else Mfalse
											| Mstring y -> eprintf "Leq Not implemented"; Unit
											| _ 		-> eprintf "Leq Not implemented"; Unit )
								| Mstring x -> ( match j with
												| Cons y 	-> eprintf "Leq Not implemented"; Unit
												| Mstring y	-> eprintf "Leq Not implemented"; Unit
												| _			-> eprintf "Leq Not implemented"; Unit )
								| _ 		-> eprintf "Leq Not implemented"; Unit )
	| Operator (Geq,f, p)	-> let k = evalexp env store f fl in
								let j = evalexp env store p fl in
								( match k with
								| Cons x -> ( match j with 
											| Cons y 	-> if x >= y then Mtrue else Mfalse
											| Mstring y -> eprintf "Geq Not implemented"; Unit
											| _ 		-> eprintf "Geq Not implemented"; Unit )
								| Mstring x -> ( match j with
												| Cons y 	-> eprintf "Geq Not implemented"; Unit
												| Mstring y	-> eprintf "Geq Not implemented"; Unit
												| _			-> eprintf "Geq Not implemented"; Unit )
								| _ 		-> eprintf "Geq Not implemented"; Unit )
	| Operator (Equal,f,p)	-> let k = evalexp env store f fl in
								let j = evalexp env store p fl in
								( match k with
								| Cons x -> ( match j with 
											| Cons y 	-> if x = y then Mtrue else Mfalse
											| Mstring y -> if (string_of_int x) = y then Mtrue else Mfalse
											| _ 		-> eprintf "Eq Not implemented"; Unit )
								| Mstring x -> ( match j with
												| Cons y 	-> if x = (string_of_int y) then Mtrue else Mfalse
												| Mstring y	-> if x = y then Mtrue else Mfalse
												| _			-> eprintf "Eq Not implemented"; Unit )
								| _ 		-> eprintf "Eq Not implemented"; Unit )
	| Operator (Noteq,f,p)	-> let k = evalexp env store f fl in
								let j = evalexp env store p fl in
								( match k with
								| Cons x -> ( match j with 
											| Cons y 	-> if x != y then Mtrue else Mfalse
											| Mstring y -> if (string_of_int x) != y then Mtrue else Mfalse
											| _ 		-> eprintf "NotEq Not implemented"; Unit )
								| Mstring x -> ( match j with
												| Cons y 	-> if x != (string_of_int y) then Mtrue else Mfalse
												| Mstring y	-> if x != y then Mtrue else Mfalse
												| _			-> eprintf "NotEq Not implemented"; Unit )
								| _ 		-> eprintf "NotEq Not implemented"; Unit )
	| Operator (And,f,p)	-> let k = evalexp env store f fl in
								let j = evalexp env store p fl in
								( match k with
								| Mtrue -> ( match j with 
											| Mtrue 	-> Mtrue
											| Mfalse	-> Mfalse
											| _ 		-> eprintf "And Not implemented"; Unit )
								| Mfalse -> ( match j with
												| Mtrue		-> Mfalse
												| Mfalse	-> Mfalse
												| _			-> eprintf "And Not implemented"; Unit )
								| _ 		-> eprintf "And Not implemented"; Unit )
	| Operator (Or, f, p)	-> let k = evalexp env store f fl in
								let j = evalexp env store p fl in
								( match k with
								| Mtrue		-> ( match j with 
											| Mtrue 	-> Mtrue
											| Mfalse	-> Mtrue
											| _ 		-> eprintf "Or Not implemented"; Unit )
								| Mfalse	-> ( match j with
												| Mtrue		-> Mtrue
												| Mfalse	-> Mfalse
												| _			-> eprintf "Or Not implemented"; Unit )
								| _ 		-> eprintf "Or Not implemented"; Unit )
	| Not f				 	-> let k = evalexp env store f fl in
								( match k with
								| Mtrue		-> Mfalse 
								| Mstring s	-> ( match s with
													| "true" -> Mtrue
													| "True" -> Mtrue
													| "false" -> Mfalse
													| "False" -> Mfalse
													| _		-> eprintf "Not Not implemented"; Unit )
								| Mfalse	-> Mtrue
								| _ 		-> eprintf "Not Not implemented"; Unit )
	| Operator (Greater,f,p)-> let k = evalexp env store f fl in
								let j = evalexp env store p fl in
								( match k with
								| Cons x -> ( match j with 
											| Cons y 	-> if x > y then Mtrue else Mfalse
											| Mstring y -> if ( string_of_int x ) > y then Mtrue else Mfalse
											| _ 		-> eprintf "Greater Not implemented"; Unit )
								| Mstring x -> ( match j with
												| Cons y 	-> if x > ( string_of_int y ) then Mtrue else Mfalse
												| Mstring y	-> if x > y then Mtrue else Mfalse
												| _			-> eprintf "Greater Not implemented"; Unit )
								| _ 		-> eprintf "Greater Not implemented"; Unit )
	| Operator (Less,f,p)	-> let k = evalexp env store f fl in
								let j = evalexp env store p fl in
								( match k with
								| Cons x -> ( match j with 
											| Cons y 	-> if x < y then Mtrue else Mfalse
											| Mstring y -> if ( string_of_int x ) < y then Mtrue else Mfalse 
											| _ 		-> eprintf "Less Not implemented"; Unit )
								| Mstring x -> ( match j with
												| Cons y 	-> if x < ( string_of_int y ) then Mtrue else Mfalse
												| Mstring y	-> if x < y then Mtrue else Mfalse
												| _			-> eprintf "Less Not implemented"; Unit )
								| _ 		-> eprintf "Less Not implemented"; Unit )

	| Asg (Identifier x, e) -> let rhs = (evalexp env store e fl) in Hashtbl.add store x rhs; rhs

	| Let (x, e, f)			-> let v = evalexp env store e fl in 
								Hashtbl.add env x v;
								let k = evalexp env store f fl in
								Hashtbl.remove env x; k

	| New (x, e, f)			-> let v = evalexp env store e fl in
								Hashtbl.add store x v;
								evalexp env store f fl
								
	| Seq (f, p)			-> let _ = evalexp env store f fl in
								let v = evalexp env store p fl in v

	| Ifelse (e, p, f)		-> let ee = evalexp env store e fl in
								( match ee with
								| Cons x -> if x = 1 then evalexp env store p fl else evalexp env store f fl
								| Mtrue -> evalexp env store p fl
								| Mfalse -> evalexp env store f fl
								| _		-> eprintf "Not implemented"; Unit ) 

	| If (f, p)				-> let ee = evalexp env store f fl in
								( match ee with
								| Cons x -> if x = 1 then evalexp env store p fl else Unit
								| Mtrue -> evalexp env store p fl
								| Mfalse -> Unit
								| _		-> eprintf "Not implemented"; Unit ) 

	| While (f, p)			-> let r = ref Unit in
								while tobool (evalexp env store f fl) do
									r := evalexp env store p fl
								done;
								!r

	| Application (Identifier id, args) 		-> let (sl, exp) = findfunc fl id in
								argsexpcreate env (evalexplist env store args) sl;
								evalexp env store exp fl

	| Readint				-> Unit
	| Printint(e)			-> let k = evalexp env store e fl in printf "%d\n" (maybetoint k); k
	| Identifier x 			-> lookup (Identifier x) env
	| Deref (Identifier e) 	-> lookup (Identifier e) store
	| _						-> eprintf "Not implemented"; Unit )

let rec maybetostr f =
	match f with
		| Mstring s -> s
		| Cons x 	-> string_of_int x 
		| Mtrue 	-> "true"
		| Mfalse	-> "false"
		| Unit		-> "unit"

let rec storefuncs fl plc =
	match fl with
	| [] -> ()
	| ( s, sl, exp ) :: xs -> Hashtbl.add plc s (sl, exp); storefuncs xs plc

let rec evalexpprogram p =
	let ( Main (sl, exp) , fl) = p in
	maybetostr (evalexp (Hashtbl.create 100) (Hashtbl.create 100) exp fl)

let parsewitherror lexbuf =
	try Par.top Lex.read lexbuf with
	| SyntaxError msg -> prerr_string (msg ^ ": ");
						print_position lexbuf;
						exit (-1)
	| Par.Error -> prerr_string "Parse error: ";
						print_position lexbuf;
						exit (-1)

(*This code runs the tests for the program*)
let basicone =
	(load_file "tests/basic1.ml"
	|> Lexing.from_string 
	|> parsewitherror
	|> evalexpprogram)

let basictwo = 
	(load_file "tests/basic2.ml"
	|> Lexing.from_string 
	|> parsewitherror
	|> evalexpprogram)

let basicthree = 
	(load_file "tests/basic3.ml"
	|> Lexing.from_string 
	|> parsewitherror
	|> evalexpprogram)

let basicfour = 
	(load_file "tests/basic4.ml"
	|> Lexing.from_string 
	|> parsewitherror
	|> evalexpprogram)

let basicfive = 
	(load_file "tests/basic5.ml"
	|> Lexing.from_string 
	|> parsewitherror
	|> evalexpprogram)

let fibonacci = 
	(load_file "tests/fibonacci.ml"
	|> Lexing.from_string
	|> parsewitherror
	|> evalexpprogram)

let numtestone = 
	(load_file "tests/numtest1.ml"
	|> Lexing.from_string
	|> parsewitherror
	|> evalexpprogram)

let numtesttwo = 
	(load_file "tests/numtest2.ml"
	|> Lexing.from_string
	|> parsewitherror
	|> evalexpprogram)
(*
let numtestthree = 
	(load_file "tests/numtest3.ml"
	|> Lexing.from_string
	|> parsewitherror
	|> evalexpprogram)
*)
let numtestfour = 
	(load_file "tests/numtest4.ml"
	|> Lexing.from_string
	|> parsewitherror
	|> evalexpprogram)


let doalltests =
	"basic1: " ^ basicone ^ " should be 10\n" ^
	"basic2: " ^ basictwo ^ " should be 3\n" ^
	"basic3: " ^ basicthree ^ " should be 7\n" ^
	"basic4: " ^ basicfour ^ " should be 7\n" ^
	"basic5: " ^ basicfive ^ " should be 5\n" ^
	"fibonacci: " ^ fibonacci ^ " should be 55\n" ^
	"numtest1: " ^ numtestone ^ " should be 10\n" ^
	"numtest2: " ^ numtesttwo ^ " should be 4\n" ^
	"numtest3: " ^ "beep boop didn't work" ^ " should be error\n" ^
	"numtest4: " ^ numtestfour ^ " should be 7\n"

(* After parsewitherror we can change between evalexpprogram or outputprog *)
let _ = 
	doalltests
	|> print_string;
