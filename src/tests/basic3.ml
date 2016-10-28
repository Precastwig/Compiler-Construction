main () { 
	var x = 5 in
	var y = 0 in
	if @x=5 do (x:=@x+2) else x:=2;
	if @x=4 do y:=@x+2;
	if (@x=5) do (p:=@x+2);
	@x;
};

