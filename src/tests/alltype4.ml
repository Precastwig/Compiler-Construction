f (x) {
	x * 10;
};

main () {
	var s = f(12) in
	print_int(@s);
	let s = 5 in (
		print_int(s);
	);
	print_int(@s);
};
