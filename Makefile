.PHONY: build clean test

build:
	dune build @install

test:
	dune runtest

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build *.install
