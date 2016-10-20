main () {
	x := true;
	p := 0;
	while (x) do {
		print_int(p);
		p := p + 1;
		if (p > 6) do x := false;
	};
	print_int(p);
}
