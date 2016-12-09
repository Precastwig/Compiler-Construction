main () {
	var x = 0 in
	var test = 0 in
	while (@x < 100) do {
		x := @x + 1;
		if ( ( x / 3 ) = 1 ) do {
			if ( ( x / 5 ) = 1 ) do {
				print_int(53);
			} else { 
				print_int(3); 
			} } 
			else {
				if ( (x / 5) = 1) do {
					print_int(5);
				} else {
					@x;
				};
			};
		}
};
