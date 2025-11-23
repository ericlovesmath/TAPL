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
  repl "#t";
  [%expect {| bool |}];
  repl "if #t then #f else #f";
  [%expect {| bool |}];
