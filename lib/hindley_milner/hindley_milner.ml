open Core
open Parser
module Types = Types
module Typecheck = Typecheck

let repl (s : string) =
  let t = s |> Lexer.of_string |> Lexer.lex |> Parser.run t_p in
  match t with
  | Error parse_err -> print_s [%message (parse_err : Error.t)]
  | Ok t ->
    (match Typecheck.typecheck t with
     | Error ty_error -> print_s [%message (ty_error : Error.t)]
     | Ok ty -> print_s (Types.sexp_of_ty ty))
;;

let%expect_test "typechecker tests" =
  repl "#u";
  repl "#t";
  repl "#f";
  repl "if #t then #f else #f";
  repl "fun x -> x";
  repl "fun x -> fun y -> x";
  repl "fun x -> fun y -> S Z";
  repl "fun x -> S x";
  repl "fun f -> fun x -> f x";
  repl "fun x -> fun f -> if x then f x else Z";
  [%expect
    {|
    unit
    bool
    bool
    bool
    ('a -> 'a)
    ('a -> ('b -> 'a))
    ('a -> ('b -> nat))
    (nat -> nat)
    (('a -> 'b) -> ('a -> 'b))
    (bool -> ((bool -> nat) -> nat))
    |}]
;;
