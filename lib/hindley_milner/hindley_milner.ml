open Core
open Parser
module Types = Types
module Typecheck = Typecheck

(* TODO: Add more cases *)
(* TODO: Better checks for free vars, aside from syntactic *)
(* TODO: Row Polymorphism *)

let repl (s : string) =
  let t = s |> Lexer.of_string |> Lexer.lex |> Parser.run t_p in
  match t with
  | Error parse_err -> print_s [%message (parse_err : Error.t)]
  | Ok t ->
    (match Typecheck.typecheck t with
     | Error ty_error -> print_s [%message (ty_error : Error.t)]
     | Ok ty -> print_s (Types.sexp_of_ty ty))
;;

let%expect_test "basic constraint polymorphism" =
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
   |}];
  repl "fun x -> x x";
  [%expect {| (ty_error ("unify: recursive unification" (v v12) (ty ('v12 -> 'v13)))) |}]
;;

let%expect_test "let polymorphism tests" =
  repl "let f = fun x -> x in f Z";
  repl
    {|
    let f = fun x -> x in
    let g = f Z in
    f
    |};
  repl "let f = fun x -> x in f (fun x -> x)";
  [%expect
    {|
    nat
    ('a -> 'a)
    ('a -> 'a)
    |}]
;;

let%expect_test "recursive tests" =
  repl "letrec f = fun x -> if x then f x else Z in f";
  repl
    {|
     letrec f =
       fun x ->
         if iszero x
           then Z
           else f (pred x)
     in f (S (S Z))
     |};
  [%expect
    {|
    (bool -> nat)
    nat
    |}]
;;

let%expect_test "value restriction tests" =
  repl "let f = ref Z in f";
  repl "let f = ref (fun x -> x) in f";
  repl
    {|
    let f = ref (fun x -> x) in
    let g = (!f) Z in
    f
   |};
  (* TODO: Seq form *)
  repl
    {|
    let f = ref (fun x -> x) in
    let x = (f := fun x -> #t) in
    let y = (f := fun x -> Z) in
    f
    |};
  [%expect
    {|
    (nat ref)
    (('a -> 'a) ref)
    ((nat -> nat) ref)
    (ty_error ("unify: invalid equality constraint" (ty bool) (ty' nat)))
    |}]
;;
