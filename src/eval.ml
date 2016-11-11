open Types
open Lex
open Lexing
open Hashtbl
open Printf
open Helpers


(*Code that loads a file *)
let load_file f =
	let ic = open_in f in
	let n = in_channel_length ic in
	let s = Bytes.create n in
	really_input ic s 0 n;
	close_in ic;
	(s)

(*Error handling position code*)

(*This code evaluates a type of program*)

let doeval =
	(read_line () 
	|> load_file
	|> Lexing.from_string
	|> parsewitherror
	|> evalexpprograms)


(* After parsewitherror we can change between evalexpprogram or outputprog *)
let _ = 
	"Returned: " ^ doeval
	|> print_string;
