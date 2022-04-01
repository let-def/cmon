test:
	dune runtest

all:
	dune build

clean:
	dune clean

promote:
	dune promote

build-install:
	dune build @install
