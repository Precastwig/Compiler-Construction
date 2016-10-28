main (i) {
	/*This is the number element of the fibonacci sequence to compute*/
	var x = 5 in
	
	/*This is the code to evaluate the fibonacci sequence*/
	var p = 1 in
	var f = 1 in
	while ( @x > 2 ) do {
		x := @x - 1;
		t := @p;
		p := @f;
		f := @f + @t;
	};
	@f;
};
(*
main () {
	var p = 4 in
	@p
};*)
