plus (x, y) {
	x + y;
};

minus (x, y) {
	x - y;
}

main() {
	var p = plus (5, 5) in
	p := minus(@p, 3);
	@p;
};
