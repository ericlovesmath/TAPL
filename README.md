# Types and Programming Languages

OCaml implementations of selected chapters in [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) by Benjamin C. Pierce, without direct references of the original ML implementations.

Run `dune test` to run [let_expect](https://github.com/janestreet/ppx_expect) tests, and `dune utop` to explore. `dune promote` will update tests.

Run `dune build` and `rlwrap ./_build/default/bin/main.exe -impl <IMPL>` for a simple REPL interface

The `Makefile` provides convenient wrappers for building and running code
