p=1

k="$1"

eval "make clean"

if [ $k == "-o" ]; then
	run="./evaloptimise.native"
	maker="make o"
else
	if [ $k == "-i" ]; then
		run="./interpret.native"
		maker="make i"
	else 
		if [ $k == "-c" ]; then
			run="./codegen.native"
			maker="make c"
		else 
			if [ $k == "-co" ]; then
				run="./codegen.native"
				maker="make c"
				eval $maker
			else
				run="./eval.native"
				maker="make"
			fi		
		fi
	fi
fi

eval $maker

f="basic1"
echo "Running test $f should return 10:"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="basic2"
echo "Running test $f should return 3:"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="basic3"
echo "Running test $f should return 7:"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="basic4"
echo "Running test $f should return 103:"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="basic5"
echo "Running test $f should print 5 and return 5:"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="basic6"
echo "Running test $f should return unit and throw variable 'X' not defined:"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="basic7"
echo "Running test $f should return 5"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="basic8"
echo "Running test $f should return 140"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="bisection"
echo "Running test $f"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="fibonacci"
echo "Running test $f should return 55"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="numtest1"
echo "Running test $f should return 10"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="numtest2"
echo "Running test $f should return 4"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="numtest3"
echo "Running test $f should return unit and throw variable 'X' not defined:"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="numtest4"
echo "Running test $f should print from 0 to 7 then return 7"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="alltype1"
echo "Running test $f should return 12"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="alltype2"
echo "Running test $f should return 5"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="alltype3"
echo "Running test $f should return 7"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="alltype4"
echo "Running test $f should print 120, 5, 120 and return 120"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="alltype5"
echo "Running test $f should print 0, 1, 2, 3, 4 then return 4 (?)"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="otest1"
echo "Running test $f should take a value and return that + 4 * 7"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p

f="otest2"
echo "Running test $f should take a value then return 6 + that value until the result is above 10"
echo "tests/$f.ml" | eval $run
echo
if [ $k == "-co" ]; then
	if [ -f "tests/$f.s" ]; then
		eval "bash tests/assemblyrun.sh tests/$f"
		eval "./tests/$f"
	else
		echo "$f.s does not exist"
	fi
fi
sleep $p
