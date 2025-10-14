# Week 1

- Setting up `dune` project and environment
- Untyped lambda calculus and beta reduction in `lib/untyped_lambda_calculus`

    - Added sexp based parsing with `quickcheck` round trip tests

- Untyped nameless lambda calculus in `lib/untyped_nameless`

    - DeBruijn Indicies with `remove_names` and `restore_names`
    - Quickcheck roundtrip tests for removing and restoring variable names
    - Evaluation with `shift`/`subst`

- Start of simply typed lambda calculus in `simply_typed_lambda_calculus`

    - `bool` and `->` types only

- Refactoring codebase
- [Bonsai](https://github.com/janestreet/bonsai) based typing judgement visualizer, rendered with MathJax

# Week 2

- Extensions to Simple Types (Chapter 8/11/12) in `lib/simply_typed_extended`

    - `seq`, `let`, `as`
    - `tuples`, `record`s, and `variant`s with projection functions
    - Syntactic sugar (dropping some parens in let/fun, implicit `< variant > = < variant : #u >` 
    - (Exhaustive) pattern matching with `(case _ of _)`
    - `Nat`, `zero`, `succ`, `pred`, `iszero`
    - `fix` point with `letrec`

- Simple beta reduction `eval` (no closures)

    - Examples and test cases

- Some debugging on LaTeX generation
