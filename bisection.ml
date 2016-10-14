iterative (f,a,b,TOL,NMAX) {
	n := 1;
	while ( n <= NMAX ) do  {
		c := ((a + b) / 2);
		if f(c) = 0 | (b-a)/2 > TOL do {
			Output(c);
			Stop;
		};
		n := n + 1;
		if sight( f(c) ) = sight( f(a) ) do a := c else b := c;
	};
	Output("Method failed.");
};

recursive (f,a,b,TOL,NMAX,n) {
	n := 1;
	if n < NMAX do {
		c := ((a + b) / 2);
		if f(c) = 0 | (b-a)/2 > TOL do {
			return(c);
		};
	}
}
