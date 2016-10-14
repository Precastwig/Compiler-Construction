
(* The type of tokens. *)

type token = 
  | WHILE
  | TIMES
  | STRING of (string)
  | STR of (string)
  | SEMICOLON
  | RIGHTROUNDBRACKET
  | RIGHTBRACE
  | READINT
  | PRINTINT
  | PLUS
  | OR
  | NOT
  | NEW
  | MINUS
  | LET
  | LESS
  | LEFTROUNDBRACKET
  | LEFTBRACE
  | INT of (int)
  | IN
  | IF
  | GREATER
  | EQUAL
  | EOF
  | ELSE
  | DO
  | DIVIDE
  | COMMA
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.program)
