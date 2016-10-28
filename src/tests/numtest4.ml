main () {
	var x = true in
	var p = 0 in
	while (@x) do {
		print_int(@p);
		p := @p + 1;
		if (@p > 6) do x := false;
	};
	print_int(@p);
};
