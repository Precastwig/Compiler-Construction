(* Configuration *)
open Hashtbl
open Types
open Helpers
open Printf
open Interprethelpers

let ram = ((Hashtbl.create 100) : ((int,int) Hashtbl.t))
let acc = ref 0
let new_addr = let i = ref 0 in (fun () -> incr i; !i)

(* Instruction execution *)
let op (op, addr1, addr2) = acc := op (Hashtbl.find ram addr1) (Hashtbl.find ram addr2)
let st addr = Hashtbl.replace ram addr !acc
let ldc n = acc := n

let pplus f p = f + p
let pminus f p = f - p
let ptimes f p = f * p
let pdiv f p = f / p
let pleq f p = if f <= p then 1 else 0
let pgeq f p = if f >= p then 1 else 0
let peq f p = if f == p then 1 else 0
let pneq f p =  if f != p then 1 else 0
let pand f p =  if f && p then 1 else 0
let por f p = if  f || p then 1 else 0
let pnot f = if  !f then 1 else 0
let pg f p =  if f > p then 1 else 0
let pl f p =  if f < p then 1 else 0

let fun_of_op = function
	| Plus -> pplus
	| Minus -> pminus
	| Times -> ptimes
	| Divide -> pdiv
	| Leq -> pleq
	| Geq -> pgeq
	| Equal -> peq
	| Noteq -> pneq
	| Greater -> pg
	| Less -> pl
	| _ -> raise Not_found

let rec interpret symt = function
	| Operator (oper, e1, e2) -> 
		let addr1 = interpret symt e1 in
		let addr2 = interpret symt e2 in
		op (fun_of_op oper, addr1, addr2);
		let addr3 = new_addr () in 
		st addr3;
		addr3
	| Identifier x -> lookup x symt
	| Deref (Identifier x) -> lookup x symt
	| Ifelse (x, e1, e2) -> let xx = interpret symt x in
							if (find ram xx) == 0 then 
							(interpret symt e2) else 
							(interpret symt e1)
	| Const n ->
		let addr = new_addr() in
		ldc n;
		st addr;
		addr
	| Let (x, e1, e2) ->
		let addr1 = interpret symt e1 in
		interpret ((x, addr1) :: symt) e2
	| New (x, e1, e2) ->
		let addr1 = interpret symt e1 in
		interpret ((x, addr1) :: symt) e2
	| _ -> raise Not_found

let interpretprogram = function
 	| (Main (sl, exp), fl) -> interpret [] exp
 	| _ -> raise Not_found

let interpretstr =
	(read_line ()
	|> load_file
	|> Lexing.from_string
	|> parsewitherror
	|> interpretprogram
	|> find ram
	|> string_of_int)
	
let _ = printf "Result: %s\n" interpretstr
