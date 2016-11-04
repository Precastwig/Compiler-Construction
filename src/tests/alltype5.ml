functionone (x) {
	if (@x > 0) {
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
