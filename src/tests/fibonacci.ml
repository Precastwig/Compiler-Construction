fibonacci (k) {
	/*This is the number element of the fibonacci sequence to compute*/
	var x = k in
	
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

main () {
	fibonacci(10)
};
