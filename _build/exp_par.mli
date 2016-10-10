
(* The type of tokens. *)

type token = 
  | TIMES
  | PLUS
  | OR
  | NOT
  | MINUS
  | LESS
  | INT of (int)
  | GREATER
  | EQUAL
  | EOF
  | DIVIDE
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (int)
