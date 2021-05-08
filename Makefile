SOURCE = syntax.ml eqparser.mli eqparser.ml eqlexer.ml lbapprox.ml approx.ml lb.ml main.ml

ub: $(SOURCE)
	ocamlopt -o ub $(SOURCE)

eqparser.ml: eqparser.mly
	ocamlyacc eqparser.mly

eqparser.mli: eqparser.mly
	ocamlyacc eqparser.mly


eqlexer.ml: eqlexer.mll
	ocamllex eqlexer.mll

top: $(SOURCE)
	ocamlmktop $(SOURCE)

clean: 
	rm a.out; rm *.cmi *.cmo *.o *.cmx eqparser.ml lexer.ml eqparser.mli

