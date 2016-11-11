functionone (x) {
	if (@x > 0) do {
		x := @x - 1;
		functionone(@x);
		print_int(@x);		
	} else {
		-1;
	};
};

main() {
	functionone(5);
}
