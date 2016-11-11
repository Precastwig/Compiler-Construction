open Hashtbl
open Printf

let rec lookup x = function
	| (y, addr) :: ys -> if x = y then addr else lookup x ys
	| _ -> printf "Cant find %s\n" x; raise Not_found
