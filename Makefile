run:
	dune exec bin/main.exe

build:
	dune build

clean:
	dune clean

utop:
	# dune utop lib/langone
	dune utop .

# test:
# 	dune exec test/main.exe
