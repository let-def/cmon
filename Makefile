all:
	dune build

clean:
	dune clean

test:
	dune runtest

promote:
	dune promote

build-install:
	dune build @install
