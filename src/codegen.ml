open Hashtbl
open Types
open Interprethelpers
open Helpers
open Printf

let ram = ((Hashtbl.create 100) : ((int,int) Hashtbl.t))
let acc = ref 0
let new_addr = let i = ref 0 in (fun () -> incr i; !i)

let string_of_operator = function
	| Plus -> "add"
	| Minus -> "sub"
	| Times -> "mul"
	| Divide -> "div"
	| Leq -> "leq"
	| Geq -> "geq"
	| Equal -> "eq"
	| And -> "and"
	| Or -> "or"
	| Not -> "not"
	| Noteq -> "neq"
	| Greater -> "greater"
	| Less -> "less"

let code = Buffer.create 100
let codegen_op (op, addr1, addr2) =
	(string_of_operator op) ^ " r" ^ (string_of_int addr1) ^ ", r" ^ (string_of_int addr2) ^ "\n"
	|> Buffer.add_string code
let codegen_st addr = "st r" ^ (string_of_int addr) ^ "\n" 
	|> Buffer.add_string code
let codegen_ldc n = "ld " ^ (string_of_int n) ^ "\n"
	|> Buffer.add_string code
	
let rec codegen symt = function
	| Operator (op, e1, e2) ->
		let addr1 = codegen symt e1 in
		let addr2 = codegen symt e2 in
		codegen_op (op, addr1, addr2);
		let addr3 = new_addr() in
		codegen_st addr3;
		addr3
	| Identifier x -> Interprethelpers.lookup x symt
	| Deref (Identifier x) -> Interprethelpers.lookup x symt
	(*| Ifelse (x, e1, e2) -> let xx = codegen symt x in*)			
	| Const n -> let addr = new_addr() in
		codegen_ldc n;
		codegen_st addr;
		addr
	| Let (x, e1, e2) ->
		let addr1 = codegen symt e1 in
		codegen ((x, addr1) :: symt) e2
	| New (x, e1, e2) ->
		let addr1 = codegen symt e1 in
		codegen ((x, addr1) :: symt) e2
	| Seq (a, b) ->
		let _ = codegen symt a in
		codegen symt b
	| a -> printf "Not implemented: %s\n" (printexp a 0); raise Not_found
		
let codegenprogram = function
	| (Main (sl, exp), fl) -> codegen [] exp
		
let codegener =
	(read_line ()
	|> load_file
	|> Lexing.from_string
	|> parsewitherror
	|> codegenprogram
	|> string_of_int)
		
let _ = let r = codegener in
		Buffer.output_buffer stdout code;
		"ld r" ^ r |> print_endline
