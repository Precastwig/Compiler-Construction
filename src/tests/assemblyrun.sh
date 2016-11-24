k="$1"
gcc -c $k.s -o $k.o
gcc $k.o -o $k
