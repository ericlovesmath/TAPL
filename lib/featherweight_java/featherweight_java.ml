open Core
open Parser
module Types = Types

let repl (s : string) =
  let t = s |> Lexer.of_string |> Lexer.lex |> Parser.run t_p in
  match t with
  | Error parse_err -> print_s [%message (parse_err : Error.t)]
  | Ok t -> print_s (Types.sexp_of_t t)
;;

let%expect_test "featherweight java tests" =
  repl "#t";
  [%expect {| #t |}]
;;
