build:
	dune build src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	dune exec ./src/main.exe