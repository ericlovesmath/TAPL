# Types and Programming Languages

OCaml implementations of selected chapters in [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) by Benjamin C. Pierce, without direct references of the original ML implementations.

Run `dune test` to run [let_expect](https://github.com/janestreet/ppx_expect) tests, and `dune utop` to explore. `dune promote` will update tests.

Run `dune build` and `rlwrap ./_build/default/bin/main.exe -impl <IMPL>` for a simple REPL interface

The `Makefile` provides convenient wrappers for building and running code

NOTE: A opam switch called "bonsai" is expected to exist with the [Bonsai](https://github.com/janestreet/bonsai) frontend library installed, in order for `web` to build with the website containing interactive features and visualization. If the web repl is not important, delete the `web` folder and `dune-workspace` files.
