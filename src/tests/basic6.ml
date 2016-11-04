main () {
	let x = (5+5) in (x := @x + 4);
	var p = @x in @p;
};
