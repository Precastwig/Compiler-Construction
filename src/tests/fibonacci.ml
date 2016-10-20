fibonacci () {
	/*This is the number element of the fibonacci sequence to compute*/
	x := 5;
	
	/*This is the code to evaluate the fibonacci sequence*/
	p := 1;
	f := 1;
	while ( x > 2 ) do {
		x := x - 1;
		t := p;
		p := f;
		f := f + t;
	};
	f;
}
