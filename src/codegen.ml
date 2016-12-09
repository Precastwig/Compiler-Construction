open Hashtbl
open Types
open Interprethelpers
open Helpers
open Printf

let code = Buffer.create 100
let sp = ref 0
let ram = ((Hashtbl.create 100) : ((int,int) Hashtbl.t))
let acc = ref 0
let new_addr = let i = ref 0 in (fun () -> incr i; !i)

let string_of_operator = function
	| Plus -> "add"
	| Minus -> "sub"
	| Times -> "mul"
	| Divide -> "idiv"
	| Leq -> "cmp"
	| Geq -> "cmp"
	| Equal -> "cmp"
	| And -> "cmp"
	| Or -> "cmp"
	| Not -> "not"
	| Noteq -> "cmp"
	| Greater -> "cmp"
	| Less -> "cmp"

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

let codegen_prefix = "	.section	.rodata
.LC0:
	.string	\"%d\\n\"
	.text
	.globl	print
	.type	print, @function
print:
.LFB2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %eax
	movl	%eax, %esi
	movl	$.LC0, %edi
	movl	$0, %eax
	call	printf
	movl	$0, %edi
	call	exit
	.cfi_endproc
.LFE2:
	.size	print, .-print
	.globl	main
	.type	main, @function
"

let codegen_infix = "main:
.LFB3:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
//End template code
"

let codegen_suffix = "//Begin templates again Moves top stack to edi
	popq %rdi
	//Below auto
	call	print
	movl	$1, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE3:
	.size	main, .-main
	.ident	\"GCC: (Ubuntu 5.4.0-6ubuntu1~16.04.2) 5.4.0 20160609\"
	.section	.note.GNU-stack,\"\",@progbits
"
	
	
let codegenx86_op op =
	"pop %rax\n" ^
	"pop %rbx\n" ^
	(string_of_operator op) ^ " %rax, %rbx\n" ^
	"push %rbx\n" |> Buffer.add_string code
	
let codegenx86_id addr =
	"//offset " ^ (string_of_int addr) ^ "\n" ^
	"mov " ^ (-16 - 8 * addr |> string_of_int) ^ "(%rbp), %rax\n" ^
	"push %rax\n"
	|> Buffer.add_string code
	
let codegenx86_st n =
	"push $" ^ (string_of_int n) ^ "\n" |> Buffer.add_string code
	
let codegenx86_let _ =
	"pop %rax\n" ^
	"pop %rbx\n" ^
	"push %rax\n"
	|> Buffer.add_string code

(*let codegenx86_if exp =
*)
let rec exptolist exp = 
	match exp with
	| Seq (a, b) -> (exptolist a) @ (exptolist b)
	| Const n -> [n]
	| _ -> printf "Not an argument list\n"; raise Not_found

let rec codegenx86_args k = 
	match k with
	| [] -> ()
	| x :: xs -> codegenx86_args xs; Buffer.add_string code ("push " ^ ( x |> string_of_int) ^ "\n")

let k = ref 0

let rec codegenx86 symt = function
	| True -> Buffer.add_string code "movq $0, %rax\ncmp %rax, %rax\n";
		sp := !sp - 1
	| False -> Buffer.add_string code "push $0\n";
		sp := !sp - 1
	| Operator (Divide, e1, e2) ->
		codegenx86 symt e1;
		Buffer.add_string code ("popq %rax\n");
		codegenx86 symt e2;
		Buffer.add_string code ("popq %rbx\nidiv %rbx\npush %rax\n");
	| Operator (op, e1, e2) ->
		codegenx86 symt e1;
		codegenx86 symt e2;
		codegenx86_op op;
		sp := !sp - 1	
	| Identifier x ->
		let addr = Interprethelpers.lookup x symt in
		codegenx86_id (addr);
		sp := !sp + 1
	| Deref (Identifier x) ->
		let addr = Interprethelpers.lookup x symt in
		codegenx86_id (addr);
		sp := !sp + 1
	| Const n ->
		codegenx86_st n;
		sp := !sp + 1
	| Let (x, e1, e2) ->
		codegenx86 symt e1;
		codegenx86 ((x, !sp) :: symt) e2;
		codegenx86_let ()
	| New (x, e1, e2) ->
		codegenx86 symt e1;
		codegenx86 ((x, !sp) :: symt) e2;
		codegenx86_let ()
	| Seq (a, b) ->
		codegenx86 symt a;
		codegenx86 symt b
	| Application (Identifier id, args) ->
		let k = exptolist args in
		codegenx86_args k;
		Buffer.add_string code ("call " ^ id ^ "\n");
	| If (x, e) ->
		codegenx86 symt x;
		Buffer.add_string code ("jnz IF" ^ (!k |> string_of_int) ^"\n");
		k := !k + 1;		
		codegenx86 symt e;
		k := !k - 1;
		Buffer.add_string code ("IF"^(!k |> string_of_int) ^":\n");
		k := !k + 1
	| Ifelse (x, e1, e2) -> 
		codegenx86 symt x;
		Buffer.add_string code ("jz IF" ^ (!k |> string_of_int) ^"\n");
		k := !k + 1;		
		codegenx86 symt e2;
		k := !k - 1;
		Buffer.add_string code ("jmp ENDIF"^(!k |> string_of_int) ^ "\nIF" ^ (!k |> string_of_int) ^ ":\n" );
		k := !k + 1;
		codegenx86 symt e1;
		k := !k - 1;
		Buffer.add_string code ("ENDIF"^(!k |> string_of_int) ^ ":\n");
		k := !k + 1
	| Asg (Identifier x, e) -> 
		let addr = Interprethelpers.lookup x symt in
		codegenx86 symt e;
		Buffer.add_string code ("pop " ^ (-16 - 8 * addr |> string_of_int) ^ "(%rbp)\n" )
	| While (x, e) ->
		Buffer.add_string code ("WHILE" ^ (!k |> string_of_int) ^":\n");
		k := !k + 1;
		codegenx86 symt x;
		k := !k - 1;
		Buffer.add_string code ("jz ENDWHILE" ^(!k |> string_of_int) ^ "\n");
		k := !k + 1;		
		codegenx86 symt e;
		k := !k - 1;
		Buffer.add_string code ("jmp WHILE"^(!k |> string_of_int) ^ "\nENDWHILE" ^ (!k |> string_of_int) ^ ":\n");
		k := !k + 1		
	| Printint(e) -> 
		codegenx86 symt e;
		Buffer.add_string code ("popq %rdi\ncall print\n")
	| a -> printf "Not implemented: %s\n" (printexp a 0); raise Not_found

let rec makearglist sl i =
	match sl with
	| [] -> []
	| x :: xs -> (x, i) :: makearglist xs (i+1)

let rec getlength p = 
	match p with
	| [] -> 0
	| x :: xs -> 1 + getlength xs
	
let makefun exp = 
	let Fun (s, sl, ex) = exp in
	let len = (getlength sl) * 8 + 16 in
	let fl = makearglist sl 1 in
	Buffer.add_string code (s ^ ":\n.cfi_startproc\n pushq %rbp\n movq %rsp, %rbp\n subq $" ^ (len |> string_of_int) ^ ", %rsp\n");
	codegenx86 fl ex;
	Buffer.add_string code ("movq %rbp, %rsp\n pop %rbp \n retq\n 	.cfi_endproc\n")

let rec codegenfunctions fl = 
	match fl with
	| [] -> ()
	| x :: [] -> makefun x
	| x :: xs -> makefun x; codegenfunctions xs

let codegenprogram p = 
	match p with
	| (Main (sl, exp), fl) -> codegenfunctions fl;
							Buffer.add_string code codegen_infix;
							codegenx86 [] exp
		
let codegener f =
	let p = ( f 
	|> Lexing.from_string
	|> parsewitherror) in
	let p = optimise p (Hashtbl.create 100) in
	codegenprogram p
	
let rec print_str oc str = Printf.fprintf oc "%s\n" str; ()

let explode s = 
	let rec exp i l = 
		if i < 0 then l else exp (i - 1) (s.[i] :: l) in
	exp (String.length s - 1) []
	
let implode l = 
	let res = Bytes.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l
  
let rec o e = (match e with
	| [] -> []
	| x :: xs -> if x == '.' then [] else x :: (o xs))

let nameget str = 
	let e = explode str in
	implode (o e)
		
let _ = Buffer.reset code;
		let filename = read_line() in
		let file = ( filename |> load_file ) in
		sp := 0;
		let filename = (nameget filename) ^ ".s" in
		let oc = open_out filename in
		Buffer.add_string code codegen_prefix;
		codegener file;
		Buffer.add_string code codegen_suffix;
		printf "%s\n" filename;
		Buffer.output_buffer oc (code);
