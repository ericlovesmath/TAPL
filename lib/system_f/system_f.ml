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
  repl "if (fun (x : bool) -> x) then #t else #f";
  [%expect {| (ty_error ("[if] condition is not TyBool" (ty_c (bool -> bool)))) |}];
  repl "if #t then #t else (fun (x : bool) -> x)";
  [%expect
    {|
    (ty_error
     ("[if] branches have unequal types" (ty_t bool) (ty_f (bool -> bool))))
    |}];
  repl "y";
  [%expect {| (ty_error ("var not in context" y (ctx ()))) |}];
  let id = "(fun (x : bool) -> x)" in
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
  repl "(fun (x : bool -> bool) -> x)";
  [%expect {| (ty ((bool -> bool) -> (bool -> bool))) |}]
;;

let%expect_test "seq tuple and record tests" =
  repl "#t; #t";
  repl "#u; #u; #f";
  [%expect
    {|
    (ty_error ("[ESeq (t, t')] expected t to be unit" (ty_t bool)))
    (ty bool)
    |}];
  let tup = "{ #t, fun (x : bool) -> x, #f }" in
  repl tup;
  repl [%string "%{tup}.0"];
  repl [%string "%{tup}.1"];
  repl [%string "%{tup}.2"];
  repl [%string "%{tup}.3"];
  [%expect
    {|
    (ty ({ bool , (bool -> bool) , bool }))
    (ty bool)
    (ty (bool -> bool))
    (ty bool)
    (ty_error
     ("tuple projection on invalid index" (tys (bool (bool -> bool) bool)) (i 3)))
    |}];
  let record = "{ one = #t, two = { nest = fun (x : bool) -> x }}" in
  repl record;
  repl [%string "%{record}.one"];
  repl [%string "%{record}.two"];
  repl [%string "(%{record}.two).nest"];
  repl [%string "%{record}.three"];
  [%expect
    {|
    (ty ({ one : bool , two : ({ nest : (bool -> bool) }) }))
    (ty bool)
    (ty ({ nest : (bool -> bool) }))
    (ty (bool -> bool))
    (ty_error
     ("record missing field" (tys ((one bool) (two ({ nest : (bool -> bool) }))))
      (l three)))
    |}]
;;

let%expect_test "ref tests" =
  repl "let x = ref Z in !x";
  [%expect {| (ty nat) |}];
  repl "let not = ref (fun (x : bool) -> if x then #f else #t) in (!not) #t";
  [%expect {| (ty bool) |}];
  repl "let x = ref Z in (x := S Z); !x";
  [%expect {| (ty nat) |}]
;;

let%expect_test "universal types typechecking" =
  repl "fun X . fun (x : X) -> x";
  repl "let id = fun X . fun (x : X) -> x in id [nat]";
  [%expect
    {|
    (ty (forall X . (X -> X)))
    (ty (nat -> nat))
   |}];
  let nil = "fun X . (fun R . fun (c : X -> R -> R) -> fun (n : R) -> n)" in
  let cons =
    {|
    fun X .
      fun (hd : X) ->
          fun (tl : (forall R . (X -> R -> R) -> R -> R)) ->
            (fun R . fun (c : X -> R -> R) -> fun (n : R) -> c hd ((tl [R]) c n))
    |}
  in
  repl nil;
  repl cons;
  [%expect
    {|
    (ty (forall X . (forall R . ((X -> (R -> R)) -> (R -> R)))))
    (ty
     (forall X .
      (X ->
       ((forall R . ((X -> (R -> R)) -> (R -> R))) ->
        (forall R . ((X -> (R -> R)) -> (R -> R)))))))
    |}]
;;
