# Types and Programming Languages

OCaml implementations of selected chapters in [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) by Benjamin C. Pierce, without direct references of the original ML implementations.

Run `dune test` to run [let_expect](https://github.com/janestreet/ppx_expect) tests, and `dune utop` to explore. `dune promote` will update tests.

Run `dune build` and `rlwrap ./_build/default/bin/main.exe -impl <IMPL>` for a simple REPL interface

The `Makefile` provides convenient wrappers for building and running code, and the nix flake is there to install the relevant Opam packages if needed.

NOTE: `web` depends on the [Bonsai](https://github.com/janestreet/bonsai) frontend library, which is only supported (as of (2025-12-10) on OCaml 5.1.1 until [Jane Street's js_of_ocaml_patches](https://github.com/janestreet/js_of_ocaml_patches/issues/1) issue is resolved. `web` will not build unless `ocaml <= 5.1.1` is used, but the rest of the library will build on later versions.
