IMPL = "simple"

default: clean build

.PHONY: repl test clean build

repl:
	rlwrap dune exec TAPL -- -impl $(IMPL)

test:
	dune runtest

clean:
	dune clean

build:
	dune build
