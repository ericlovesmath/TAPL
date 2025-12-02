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
     | Ok ty -> print_s [%message (ty : Types.ty)])
;;

let%expect_test "typechecker tests prior to extending" =
  repl "#t";
  [%expect {| (ty bool) |}];
  repl "if #t then #f else #f";
  [%expect {| (ty bool) |}];
  repl "if (fun x : bool -> x) then #t else #f";
  [%expect {| (ty_error ("[if] condition is not TyBool" (ty_c (bool -> bool)))) |}];
  repl "if #t then #t else (fun x : bool -> x)";
  [%expect
    {|
    (ty_error
     ("[if] branches have unequal types" (ty_t bool) (ty_f (bool -> bool))))
    |}];
  repl "y";
  [%expect {| (ty_error ("var not in context" y (ctx ()))) |}];
  let id = "(fun x : bool -> x)" in
  repl id;
  [%expect {| (ty (bool -> bool)) |}];
  repl [%string "(%{id} #t)"];
  [%expect {| (ty bool) |}];
  repl [%string "(%{id} %{id})"];
  [%expect
    {|
    (ty_error
     ("arg can't be applied to func" (ty_f (bool -> bool)) (ty_arg bool)))
    |}];
  repl "(#t #f)";
  [%expect {| (ty_error ("attempting to apply to non-arrow type" (ty_f bool))) |}];
  repl "(fun x : (bool -> bool) -> x)";
  [%expect {| (ty ((bool -> bool) -> (bool -> bool))) |}]
;;
