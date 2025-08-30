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

docs:
	dune build @doc
	open _build/default/_doc/_html/index.html

.PHONY: test

test:
	dune runtest
