k="$1"
gcc -O0 -S $k.c
gcc -c $k.s -o $k.o
gcc $k.o -o $k
