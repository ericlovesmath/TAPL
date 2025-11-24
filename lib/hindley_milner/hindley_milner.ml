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

let test s =
  Typecheck.Unique_id.For_testing.reset_counter ();
  repl s

let%expect_test "typechecker tests" =
  test "#u";
  test "#t";
  test "#f";
  test "if #t then #f else #f";
  test "fun x -> x";
  test "fun x -> fun y -> x";
  test "fun x -> S x";
  test "fun f -> fun x -> f x";
  test "fun x -> fun y -> if x then y else Z";
  [%expect
    {|
    unit
    bool
    bool
    bool
    ('v0 -> 'v0)
    ('v0 -> ('v1 -> 'v0))
    (nat -> nat)
    (('v1 -> 'v2) -> ('v1 -> 'v2))
    (bool -> (nat -> nat))
    |}]
;;
