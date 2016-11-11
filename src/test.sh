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
			run="./eval.native"
			maker="make"
		fi
	fi
fi

eval $maker

echo "Running test basic1 should return 10:"
echo "tests/basic1.ml" | eval $run
echo
sleep $p

echo "Running test basic2 should return 3:"
echo "tests/basic2.ml" | eval $run
echo
sleep $p

echo "Running test basic3 should return 7:"
echo "tests/basic3.ml" | eval $run
echo
sleep $p

echo "Running test basic4 should return 103:"
echo "tests/basic4.ml" | eval $run
echo
sleep $p

echo "Running test basic5 should print 5 and return 5:"
(echo "tests/basic5.ml" | eval $run) <<< 4
echo
sleep $p

echo "Running test basic6 should return unit and throw variable 'X' not defined:"
echo "tests/basic6.ml" | eval $run
echo
sleep $p

echo "Running test basic7 should return 5"
echo "tests/basic7.ml" | eval $run
echo
sleep $p

echo "Running test basic8 should return 140"
echo "tests/basic8.ml" | eval $run
echo 
sleep $p

echo "Running test bisection"
echo "tests/bisection.ml" | eval $run
echo
sleep $p

echo "Running test fibonacci should return 55"
echo "tests/fibonacci.ml" | eval $run
echo
sleep $p

echo "Running test numtest1 should return 10"
echo "tests/numtest1.ml" | eval $run
echo
sleep $p

echo "Running test numtest2 should return 4"
echo "tests/numtest2.ml" | eval $run
echo
sleep $p

echo "Running test numtest3 should return unit and throw variable 'X' not defined:"
echo "tests/numtest3.ml" | eval $run
echo
sleep $p

echo "Running test numtest4 should print from 0 to 7 then return 7"
echo "tests/numtest4.ml" | eval $run
echo
sleep $p

echo "Running test alltype1 should return 12"
echo "tests/alltype1.ml" | eval $run
echo
sleep $p

echo "Running test alltype2 should return 5"
echo "tests/alltype2.ml" | eval $run
echo
sleep $p

echo "Running test alltype3 should return 7"
echo "tests/alltype3.ml" | eval $run
echo
sleep $p

echo "Running test alltype4 should print 120, 5, 120 and return 120"
echo "tests/alltype4.ml" | eval $run
echo
sleep $p

echo "Running test alltype5 should print 0, 1, 2, 3, 4 then return 4 (?)"
echo "tests/alltype5.ml" | eval $run
echo
sleep $p

echo "Running test otest1 should take a value and return that + 4 * 7"
echo "tests/otest1.ml" | eval $run
echo
sleep $p

echo "Running test otest2 should take a value then return 6 + that value until the result is above 10"
echo "tests/otest2.ml" | eval $run
echo
sleep $p
