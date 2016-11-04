open Types
open Lex
open Lexing
open Hashtbl
open Printf
open Helpers
open Eval

let rec read_to_empty buf = 
	let s = read_line() in
	if s = "" then buf
	else (Buffer.add_string buf s;
			Buffer.add_string buf "\n";
			read_to_empty buf)

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
	| _ 	-> false
	
let maybetoint e =
	match e with
	| Cons x -> x
	| Mstring s -> String.length s
	| Mtrue -> 1
	| Mfalse -> 0
	| _ 	-> 0

let lookup i store = 
	match i with
	| Identifier s -> (try Hashtbl.find store s with
											| Not_found -> eprintf "Variable '%s' is not defined\n" s; Unit
											| e -> raise e )
	| _ 	-> Unit

let rec evalexplist env store e = 
	(*printf "%s" (printexp e 0);*)
	match e with
	| Const x -> [Cons x]
	| True 	 -> [Mtrue]
	| False  -> [Mfalse]
	| String s -> [Mstring s]
	| Identifier x 			-> [lookup (Identifier x) env]
	| Deref (Identifier e) 	-> [lookup (Identifier e) store]
	| Seq(f, s) -> List.rev_append (evalexplist env store f) (evalexplist env store s)
	| _	-> eprintf "Error with argument type\n"; []

let rec printfl fl =
	match fl with
	| [] -> "fin\n"
	| Fun (s, sl, exp) :: xs -> s ^ "\n" ^ printfl xs

let rec findfunc fl n =
	match fl with
	| [] -> eprintf "Error, function does not exist\n"; ([],Ref Unit)
	| Fun (s, sl, exp) :: xs -> if s = n then (sl, exp) else findfunc xs n

let rec storefuncs fl plc =
	match fl with
	| [] -> ()
	| ( s, sl, exp ) :: xs -> Hashtbl.add plc s (sl, exp); storefuncs xs plc	

(*Optimisation code from here*)
	
let optimisetobool e =
	match e with
	| Const x -> if x = 0 then false else true
	| String s -> if s = "" then false else true
	| True -> true
	| False -> false
	| _		-> false

let optimiselookup i store = 
	match i with
	| Identifier s -> (try Hashtbl.find store s with
											| Not_found -> Identifier s
											| e -> Identifier s )
	| f 	-> f
	
let rec checkal a al = 
	match al with
	| [] -> false
	| x :: xs -> if a = x then true else checkal a xs 

let rec checkfor exp al env store =
	match exp with	
	| Readint -> true
	| Identifier x -> if checkal x al then true else 
						let k = (optimiselookup (Identifier x) env) in
						(match k with
						| Identifier y -> if x = y then true else false
						| f -> false )
	| Deref ( Identifier e ) -> let k = (optimiselookup (Identifier e) store) in
								( match k with
									| Identifier y -> if e = y then true else false
									| f -> false)
	| Seq(a,b) -> (checkfor a al env store) || (checkfor b al env store)
	| f -> false

let rec simpled exp =
	match exp with
	| Const x -> true
	| True -> true
	| False -> true
	| String s -> true
	| Seq(a,b) -> simpled a && simpled b
	| f -> false
	
let rec optimiseargslist env store e = 
	match e with
	| Const x -> [Const x]
	| True -> [True]
	| False -> [False]
	| String s -> [String s]
	| Identifier x -> [optimiselookup (Identifier x) env]
	| Deref (Identifier x) -> [ optimiselookup (Identifier x) store ]
	| Seq(a,b) -> List.rev_append (optimiseargslist env store a) (optimiseargslist env store b)
	| f -> [f]

let rec optimisehelp fl al exp store env =
	match exp with
	| Const x	 			-> Const x
	| True					-> True
	| False					-> False
	| String s				-> String s
	| Operator (Plus, f, p)	-> let k = optimisehelp fl al f store env in
								let j = optimisehelp fl al p store env in
								( match k with
								| Const x -> ( match j with 
											| Const y 	-> Const ( x + y )
											| String y -> String ( string_of_int x ^ y )
											| f 		-> f )
								| String x -> ( match j with
												| Const y 	-> String ( x ^ string_of_int y )
												| String y	-> String ( x ^ y )
												| f			-> f )
								| f 		-> f )
	| Operator (Minus, f, p)-> let k = optimisehelp fl al f store env in
								let j = optimisehelp fl al p store env in
								( match k with
								| Const x -> ( match j with 
											| Const y 	-> Const ( x - y )
											| String y -> String y
											| f 		-> f )
								| String x -> ( match j with
												| Const y 	-> String ( String.sub x 0 ((String.length x) - y) )
												| String y	-> String y
												| f			-> f)
								| f 		-> f )
	| Operator (Times, f, p)-> let k = optimisehelp fl al f store env in
								let j = optimisehelp fl al p store env in
								( match k with
								| Const x -> ( match j with 
											| Const y 	-> Const ( x * y )
											| String y -> String ( string_repeat y x )
											| f 		-> f )
								| String x -> ( match j with
												| Const y 	-> String ( string_repeat x y )
												| String y	-> String ( x ^ y )
												| f			-> f )
								| f 		-> f )
	| Operator (Divide, f,p)-> let k = optimisehelp fl al f store env in
								let j = optimisehelp fl al p store env in
								( match k with
								| Const x -> ( match j with 
											| Const y 	-> Const ( x / y )
											| String y -> String y
											| f 		-> f )
								| String x -> String x
								| f 		-> f )
	| Operator (Leq,f, p)	-> let k = optimisehelp fl al f store env in
								let j = optimisehelp fl al p store env in
								( match k with
								| Const x -> ( match j with 
											| Const y 	-> if x <= y then True else False
											| String y -> String y
											| f 		-> f )
								| String x -> String x 
								| f 		-> f )
	| Operator (Geq,f, p)	-> let k = optimisehelp fl al f store env in
								let j = optimisehelp fl al p store env in
								( match k with
								| Const x -> ( match j with 
											| Const y 	-> if x >= y then True else False
											| String y -> String y
											| f 		-> f )
								| String x -> String x
								| f 		-> f)
	| Operator (Equal,f,p)	-> let k = optimisehelp fl al f store env in
								let j = optimisehelp fl al p store env in
								( match k with
								| Const x -> ( match j with 
											| Const y 	-> if x = y then True else False
											| String y -> if (string_of_int x) = y then True else False
											| f 		-> f )
								| String x -> ( match j with
												| Const y 	-> if x = (string_of_int y) then True else False
												| String y	-> if x = y then True else False
												| f			-> f )
								| f 		-> f )
	| Operator (Noteq,f,p)	-> let k = optimisehelp fl al f store env in
								let j = optimisehelp fl al p store env in
								( match k with
								| Const x -> ( match j with 
											| Const y 	-> if x != y then True else False
											| String y -> if (string_of_int x) != y then True else False
											| f 		-> f )
								| String x -> ( match j with
												| Const y 	-> if x != (string_of_int y) then True else False
												| String y	-> if x != y then True else False
												| f			-> f )
								| f 		-> f )
	| Operator (And,f,p)	-> let k = optimisehelp fl al f store env in
								let j = optimisehelp fl al p store env in
								( match k with
								| True -> ( match j with 
											| True 	-> True
											| False	-> False
											| f 		-> f )
								| False -> ( match j with
												| True		-> False
												| False	-> False
												| f			-> f )
								| f 		-> f )
	| Operator (Or, f, p)	-> let k = optimisehelp fl al f store env in
								let j = optimisehelp fl al p store env in
								( match k with
								| True		-> ( match j with 
											| True 	-> True
											| False	-> True
											| f 		-> f )
								| False	-> ( match j with
												| True		-> True
												| False	-> False
												| f			-> f )
								| f 		-> f )
	| Not f				 	-> let k = optimisehelp fl al f store env in
								( match k with
								| True		-> False 
								| String s	-> ( match s with
													| "true" -> True
													| "True" -> True
													| "false" -> False
													| "False" -> False
													| f		-> String f )
								| False	-> True
								| f 		-> f )
	| Operator (Greater,f,p)-> let k = optimisehelp fl al f store env in
								let j = optimisehelp fl al p store env in
								( match k with
								| Const x -> ( match j with 
											| Const y 	-> if x > y then True else False
											| String y -> if ( string_of_int x ) > y then True else False
											| f 		-> f )
								| String x -> ( match j with
												| Const y 	-> if x > ( string_of_int y ) then True else False
												| String y	-> if x > y then True else False
												| f			-> f )
								| f 		-> f )
	| Operator (Less,f,p)	-> let k = optimisehelp fl al f store env in
								let j = optimisehelp fl al p store env in
								( match k with
								| Const x -> ( match j with 
											| Const y 	-> if x < y then True else False
											| String y -> if ( string_of_int x ) < y then True else False 
											| f 		-> f )
								| String x -> ( match j with
												| Const y 	-> if x < ( string_of_int y ) then True else False
												| String y	-> if x < y then True else False
												| f			-> f )
								| f 		-> f )

	| Asg (Identifier x, e) -> let rhs = optimisehelp fl al e store env in 
								if simpled rhs then (Hashtbl.replace store x rhs; rhs) else Asg (Identifier x, e)
	| While (f, p)			-> if (checkfor p al env store) then While (f, p) else
								let r = ref False in
								while optimisetobool (optimisehelp fl al f store env) do
									r := optimisehelp fl al p store env
								done;
								!r
	| Let (x, e, f)			-> let v = optimisehelp fl al e store env in 
								Hashtbl.add env x v;
								let k = optimisehelp fl al f store env in
								Hashtbl.remove env x; k

	| New (x, e, f)			-> if (checkfor e al env store) then New (x, e, optimisehelp fl al f store env) else
								if (checkfor f al env store) then New (x, optimisehelp fl al e store env, f) else 
								let v = optimisehelp fl al e store env in
								Hashtbl.add store x v;
								optimisehelp fl al f store env
							
	| Seq (f, p)			-> let l = optimisehelp fl al f store env in
								let v = optimisehelp fl al p store env in 
								(match l with
								| If (False, _) -> v
								| s -> (match v with
										| If (False, _) -> l
										| q -> Seq (l, v)))

	| Ifelse (e, p, q)		-> let ee = optimisehelp fl al e store env in
								( match ee with
								| Const x -> if x = 1 then optimisehelp fl al p store env else (optimisehelp fl al q store env)
								| True -> optimisehelp fl al p store env
								| False -> optimisehelp fl al q store env
								| f		-> Ifelse (f,p,q) ) 

	| If (f, p)				-> let ee = optimisehelp fl al f store env in
								( match ee with
								| Const x -> if x = 1 then optimisehelp fl al p store env else If (False, p)
								| True -> optimisehelp fl al p store env
								| False -> If (False, p)
								| q		-> If (q,p) ) 

	| Application (Identifier id, args) -> 
							let k = (optimisehelp fl al args store env) in
							let k = (optimiseargslist env store k) in
							if simpled args then (
							let (sl, exp) = findfunc fl id in
							(*	printf "Application %s\n" (printexp args 0); *)
							(*	let rec flatten l = (match l with
												| [] -> ""
												| x :: xs -> maybetostr x ^ " " ^ flatten xs) in 
								printf "After evalexplist %s" (flatten k); *)
								argsexpcreate env k sl;
								optimisehelp fl al exp store env ) else Application( Identifier id, args)
	| Readint				-> Readint
	| Printint(e)			-> Printint( optimisehelp fl al e store env)
	| Identifier x 			-> if checkal x al then Identifier x else optimiselookup (Identifier x) env
	| Deref (Identifier e) 	-> optimiselookup (Identifier e) store
	| f						-> f 

let rec optimiselist fl store =
	match fl with
	| [] -> []
	| Fun (s, sl, exp) :: xs ->
					Fun ( s, sl, optimisehelp fl sl exp store (Hashtbl.create 100) ) :: (optimiselist xs store)
(*
let rec optimiselisttwo fl store =
	match fl with
	| [] -> []
	| Fun (s, sl, exp) :: xs ->
					Fun ( s, sl, optimisehelptwo sl exp store (Hashtbl.create 100) ) :: (optimiselisttwo xs store)

let rec optimisetwo p store =
	let ( Main (sl, exp) , fl) = p in
	let fl = optimiselisttwo fl store in
	(Main (sl, optimisehelptwo sl exp store (Hashtbl.create 100)) , fl)
*)
let rec optimise p store =
	let ( Main (sl, exp) , fl) = p in
	let fl = optimiselist fl store in
	(Main (sl, optimisehelp fl sl exp store (Hashtbl.create 100)) , fl )
	
let rec printfunclist fl =
	match fl with
	| [] -> ()
	| Fun (s, sl, exp) :: xs -> printf "%s, %s\n" s (printexp exp 0); printfunclist xs
	
let rec evalexpprogram p =
	let env = (Hashtbl.create 100) in
	let store = (Hashtbl.create 100) in
	let ( Main (sl, pexp) , pfl) = p in
(*	printf "before %s\n" (printexp pexp 0); *)
	let k = optimise p store in
	(*let k = optimisetwo k store in*)
	let ( Main (sl, exp) , fl) = k in
	printfunclist fl;
	printf "after %s\n" (printexp exp 0);
	maybetostr (evalexp (Hashtbl.create 100) (Hashtbl.create 100) exp fl)

let parsewitherror lexbuf =
	try Par.top Lex.read lexbuf with
	| SyntaxError msg -> prerr_string (msg ^ ": ");
						print_position lexbuf;
						exit (-1)
	| Par.Error -> prerr_string "Parse error: ";
						print_position lexbuf;
						exit (-1)

let doalltests =
	(read_line () 
	|> load_file
	|> Lexing.from_string
	|> parsewitherror
	|> evalexpprogram)


(* After parsewitherror we can change between evalexpprogram or outputprog *)
let _ = 
	"Returned: " ^ doalltests
	|> print_string;
