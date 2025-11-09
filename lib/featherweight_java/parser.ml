open Core
open Types
module Lexer = Chomp.Lexer
open Lexer
module Parser = Chomp.Make (Chomp.Lexer)
open Parser
open Parser.Let_syntax
open Parser.Infix_syntax

let semis p = sep_by1 (tok COMMA) p

let rec program_p =
  fun st ->
  (let%bind class_decls = many class_decl_p in
   let%bind t = term_p in
   return (Program (class_decls, t)))
    st

and class_decl_p =
  failwith "TODO"

and term_p =
  failwith "TODO"

(* let%expect_test "featherweight java parse tests" = *)
(*   let test s = *)
(*     s *)
(*     |> Chomp.Lexer.of_string *)
(*     |> Chomp.Lexer.lex *)
(*     |> run program_p *)
(*     |> Or_error.sexp_of_t sexp_of_t *)
(*     |> print_s *)
(*   in *)
(*   test "A"; *)
(*   test "bool"; *)
(*   test "< some : nat, none >"; *)
(*   [%expect *)
(*     {| *)
(*     (Ok A) *)
(*     (Ok bool) *)
(*     (Ok (< some : nat , none >)) *)
(*     |}]; *)
(* ;; *)
