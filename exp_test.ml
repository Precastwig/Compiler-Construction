let _ = 
	read_line() |> Lexing.from_string |> Exp_par.top Exp_lex.read |> print_int; print_newline()
