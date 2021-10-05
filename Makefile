build:
	dune build

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	dune exec ./src/main.exe