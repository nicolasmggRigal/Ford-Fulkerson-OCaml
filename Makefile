
build:
	@echo "==== COMPILING ===="
	ocamlbuild ftest.native

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "==== EXECUTING ===="
	./ftest.native graphs/graph2 outfile
	@echo "==== RESULT ==== (content of outfile)"
	@echo ""
	@cat outfile
	@echo ""
	@dot -Tsvg graph.gv > outgraph.svg
	@eom outgraph.svg&

clean:
	-rm -rf _build/
	-rm ftest.native
