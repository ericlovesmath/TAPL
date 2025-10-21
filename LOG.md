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

# Week 3

- Parser Combinator Library, porting from Abstract Machine project

    - Convert to use `Core`'s `Monad.Make` and `Applicative.Make`
    - Add `Alternative` and `Functor` syntax
    - Use `stream` of `(char * pos) Sequence.t` instead of `char list`

- Implement parser combinator for `ty` and `t` (lots of retries)

    - Big test function takes ~60 seconds to parse (guessing issue is with too many `strip`s)
    - Refactor to remove some backtracking (~30 seconds)
    - Implementation of Lexer to tokens

        - Convert `Parser` to be a functor `Chomp.Make(Lexer)`
        - Parses arbitrary tokens instead of `string`
        - Cuts down big test function to ~20 seconds

    - Refactor parser to use `commit` on first token (~12 seconds)
    - Refactor parser to postfix forms that have valid expressions as a prefix (~1 second)

- Simple REPL interface

# TODO

- REPL interface for type inference and evaluation
- Ref / List / Exceptions, maybe?
- Push Bonsai code
