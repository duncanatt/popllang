run:
	dune exec bin/main.exe

build:
	dune build

install:
	opam install . --deps-only

clean:
	dune clean

utop:
	# dune utop lib/langone
	dune utop .

.PHONY: test

test:
	dune runtest
