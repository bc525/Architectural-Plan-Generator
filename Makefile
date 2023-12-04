
.PHONY: test check

build:
	dune build src

code:
	-dune build
	code .
	! dune build --watch


test:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	OCAMLRUNPARAM=b dune exec main/main.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh