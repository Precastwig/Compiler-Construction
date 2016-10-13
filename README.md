# CompilerConstructionExercise1
UoB school work, creating a compiler.

Syntax:
	Logical operators:
		+	- plus
		-	- minus
		/	- divide
		*	- times
		=	- equal
		!=	- not equal
		>=	- greater than or equal to
		<= 	- less than or equal to
		&	- and
		|	- or
	file structure:
		a list of functions, seperated with ; 
			e.g. main () { .. };
				secondfunction (args) { .. }
		the last function in the file does not end with a ;
	function definition:
		name (arguments, seperated, with, commas) { .. } 
	Inside a function:
		Inside the function we put our code seperated with a ;
			e.g. main () { 5 + 5; x := 400 }
		Again we do not put a ; before the final bracket }
	Loops and branches:
		while ( .. ) do ( .. )  - This is our while loop, the brackets are not necessary as the while syntax is more weakly bound than all other operations
		if ( .. ) do ( .. ) - This is our if statement, again the brackets are not strictly necessary. The first bracket must evaluate, and the last must be an expression. 
			The if statement can be extended with else
		if ( .. ) do ( .. ) else ( .. )
	Assignment:
		We can assign a value to a variable
			a := 5	- The left side must be a variable string and the right must evaluate
		We can assign a value to a string only in a particular expression
			let x = 5 in ( .. )
		We can create a new variable ???
			new x = 5 in ( .. )     //I'm not sure what this is actually meant to represent, memory management like malloc?
	Dereferencing an expression:
		!( .. ) will dereference an expression
	Application of a function:
		functionone(parameter)		//Again i wasn't sure what you meant by an application of an expression (this seems nonsensical?) So i changed to a function application with a single parameter
	Reading/printing:
		read_int()
		print_int( .. ) 

