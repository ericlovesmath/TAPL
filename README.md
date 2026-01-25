# TAPL Implementations in OCaml

This repository contains OCaml implementations of various type systems described in [**Types and Programming Languages (TAPL)**](https://www.cis.upenn.edu/~bcpierce/tapl/). The project avoids direct reference to the original ML implementations provided with the book.

Each type system is implemented in its own directory under `lib/`. The only exceptions are the shared `lib/chomp`, a simple monadic parser combinator library, and `lib/general_interpreter`, a shared interpreter for the nameless lambda calculus. The codebase heavily uses the [core](https://github.com/janestreet/core) standard library with their monadic `let%bind` syntax, along with Jane Street's [ppx_expect](https://github.com/janestreet/ppx_expect) testing framework.

| Implementation              | TAPL Chapters | Description                                   |
| :-------------------------- | :------------ | :-------------------------------------------- |
| `untyped_lambda_calculus`     | 2-5           | Untyped Lambda Calculus (Named)               |
| `nameless_nameless`           | 6-7           | Untyped Lambda Calculus (Nameless/De Bruijn)  |
| `simply_typed_lambda_calculus`| 8-10          | Simply Typed Lambda Calculus                  |
| `simply_typed_extended`       | 11-14         | STLC with normalization, refs, and exceptions |
| `subtyping`                   | 15-18         | STLC with Subtyping and row polymorphism      |
| `featherweight_java`          | 19            | Featherweight Java                            |
| `recursive_types`             | 20-21         | Recursive Types (Iso-recursive)               |
| `hindley_milner`              | 22            | Hindley-Milner Type Inference                 |
| `system_f`                    | 23-25         | System F (Polymorphic Lambda Calculus)        |
| `f_sub`                       | 26-28         | System F-sub (Bounded Quantification)         |
| `f_omega`                     | 29-30         | System F-omega (Higher-order Polymorphism)    |

## Building and Running

```bash
# nix devshell with relevant packages
# If nix is not installed, the required opam packages are in TAPL.opam
nix develop
dune build

# Run REPL for fomega, see `bin/main.ml` for exhaustive list
make repl IMPL=fomega

# This project uses `ppx_expect` for integration testing
# You can see the expected output directly in the source
# (e.g. see lib/f_omega/f_omega.ml for f_omega tests)
# Run let%expect tests and update them with the new output with the following
dune test lib/fomega
dune promote
```

## Contributing

This repository is primarily for personal learning. If you find bugs or want to suggest improvements to the implementations, feel free to open an issue or a pull request.
