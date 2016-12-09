open Types
open Lex
open Lexing
open Hashtbl
open Printf
open Helpers
	
let rec printfunclist fl =
	match fl with
	| [] -> ()
	| Fun (s, sl, exp) :: xs -> printf "%s,\n %s\n" s (printexp exp 0); printfunclist xs
	
let rec evalexpprogram p =
	let env = (Hashtbl.create 100) in
	let store = (Hashtbl.create 100) in
	let ( Main (sl, pexp) , pfl) = p in
	printf "before \n%s\n" (printexp pexp 0); 
	printfunclist pfl; 
	let k = optimise p store in
(*	let k = optimisetwo k store in*)
	let ( Main (sl, exp) , fl) = k in
	printf "after \n%s\n" (printexp exp 0);
	printfunclist fl; 
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
