.PHONY: test check

build:
	dune build

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	dune exec ./src/main.exe
	
utop:
	OCAMLRUNPARAM=b dune utop src
	
zip:
	rm -f chess.zip
	zip -r chess.zip . -x@exclude.lst