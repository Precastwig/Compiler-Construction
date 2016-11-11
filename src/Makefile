mainmake:
	ocamlbuild -use-menhir -use-ocamlfind eval.native
o:
	ocamlbuild -use-menhir -use-ocamlfind evaloptimise.native
i:
	ocamlbuild -use-menhir -use-ocamlfind interpret.native
c:
	ocamlbuild -use-menhir -use-ocamlfind codegen.native
clean:
	ocamlbuild -use-menhir -use-ocamlfind eval.native -clean
	ocamlbuild -use-menhir -use-ocamlfind evaloptimise.native -clean
	ocamlbuild -use-menhir -use-ocamlfind interpret.native -clean
	ocamlbuild -use-menhir -use-ocamlfind compile.native -clean
