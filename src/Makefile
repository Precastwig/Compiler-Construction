mainmake:
	ocamlbuild -use-menhir -use-ocamlfind eval.native
	ocamlbuild -use-menhir -use-ocamlfind evaloptimise.native
clean:
	ocamlbuild -use-menhir -use-ocamlfind eval.native -clean
	ocamlbuild -use-menhir -use-ocamlfind evaloptimise.native -clean
