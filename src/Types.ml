type opcode =
  | Plus | Minus | Times | Divide
  | Leq | Geq | Equal | Noteq
  | And | Or | Not | Greater | Less
type expression =
  | Seq of expression * expression (* e; e *)
  | While of expression * expression (* while e do e *)
  | Ifelse of expression * expression * expression (* if e do e else e *)
  | If of expression * expression
  | Asg of expression * expression (* e := e *)
  | Deref of expression (* !e *)
  | Operator of opcode * expression * expression (* e + e *)
  | Application of expression * expression (* e(e) *)
  | String of string
  | Const of int
  | Readint (* read_int () *)
  | Printint of expression (* print_int (e) *)
  | Identifier of string (* x *)
  | Let of string * expression * expression (* let x = e in e *)
  | New of string * expression * expression (* new x = e in e *)
type fundef = string * string list * expression
type program = fundef list
