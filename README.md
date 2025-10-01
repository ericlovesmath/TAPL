# Types and Programming Languages

OCaml implementations of selected chapters in [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) by Benjamin C. Pierce, without direct references of the original ML implementations.

Run `dune test` to run [let_expect](https://github.com/janestreet/ppx_expect) tests, and `dune utop` to explore. Most implementations will have a `Syntax` module for shorthand notation. Note that `utop` may want to qualify the names of variants, making it harder to read, so using `Sexp.to_string_hum` and `sexp_of_t` may be preferable.
