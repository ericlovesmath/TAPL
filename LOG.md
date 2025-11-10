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

- Simple REPL interface (not a true repl, just expression evaluator)

# Week 4

- Completely move `sexp` based tests to `chomp` based test
- `shift`/`subst` based evaluation of `simply_typed_extended`

    - Closures naturally supported now, added tests
    - Refactor to use `eval1` to follow small step semantics more directly

- Merging typechecker and evaluation tests
- `ref`, `!`, and `:=` type checking and parsing, adding tests
- Refactoring using `Store` (state monad implementation of `mu`) for `ref`-related evaluation
- Fix issue in REPL where `Ctrl-D` causes error due to `In_channel.input_line_exn`

# Week 5

- Move `Lexer` to `chomp` since it will be shared for most impls, but keep generic `Parser` functor
- Start work on `subtyping`

    - Add parsers for `top` type and modified `variants` (no longer needs to be ascribed to type), patching tests
    - Implement subtyping `<:` and patch in terms that don't require `join`/`meet`, replacing most `equal_ty`s
    - `join` for `if` and `match`, adding tests
    - Minimal `bot` type implemented
    - Add `error` form with `bot` type, adding tests
    - Set `bool <: nat` (implicitly, `#f => 0` and `#t => 1`)

- Generic REPL implementation supporting `subtyping`
- Makefile

# Week 6

- `eval` for `subtyping`, adding `error/exception` forms
- `featherweight_java` (implementation was almost directly from the helper functions and rules in TAPL)

    - Parser for FJ, does not check if constructor has exact required order or contents
    - Tests for `term` and `class_decl` parsing
    - Typecheck `term`
    - Typecheck methods and class declarations
    - Evaluation of `term` given `class_decl`s (uses subtyping!)
    - Complete tests for typecheck/eval
    - REPL support

# Week 7

- Potential TODOs:

    - FJ: Better error messages in typechecker / evaluator
    - FJ: Less tolerant parser for constructors
    - FJ: Check for dependency cycles in subtyping
    - FJ: Extension exercises (assign field`ref`-style, `raise/try`, interfaces, `super`, primitives) 

# TODO

- REPL interface for type inference and evaluation
- List / Arrays / Exceptions
- Push Bonsai code
- `meet` (as in `join`/`meet`)
- Evaluator for `subtyping`, coercion semantics for runtime evaluation in `subtyping`?
- Upload notes on TAPL to Github
- Chapter 18/19 tests with objects
