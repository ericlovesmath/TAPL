open Core
include Types
module Eval = Eval
module Typecheck = Typecheck

let repl (s : string) =
  let t = s |> Lexer.of_string |> Lexer.lex |> Parser.run Parser.t_p in
  match t with
  | Error parse_err -> print_s [%message (parse_err : Error.t)]
  | Ok t ->
    (match Typecheck.typecheck t with
     | Error ty_error -> print_s [%message (ty_error : Error.t)]
     | Ok ty ->
       let result = Eval.eval (Eval.remove_names t) in
       print_s [%message (ty : ty) (result : nameless)])
;;

let%expect_test "typechecker tests prior to extending" =
  repl "#t";
  [%expect {| ((ty bool) (result #t)) |}];
  repl "if #t then #f else #f";
  [%expect {| ((ty bool) (result #f)) |}];
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
  [%expect {| ((ty (bool -> bool)) (result (abs . 0))) |}];
  repl [%string "(%{id} #t)"];
  [%expect {| ((ty bool) (result #t)) |}];
  repl [%string "(%{id} %{id})"];
  [%expect
    {|
    (ty_error
     ("arg can't be applied to func" (ty_f (bool -> bool)) (ty_arg bool)))
    |}];
  repl "(#t #f)";
  [%expect {| (ty_error ("attempting to apply to non-arrow type" (ty_f bool))) |}];
  repl "(fun x : (bool -> bool) -> x)";
  [%expect {| ((ty ((bool -> bool) -> (bool -> bool))) (result (abs . 0))) |}]
;;

let%expect_test "extended typechecker tests" =
  repl "#u";
  [%expect {| ((ty unit) (result #u)) |}];
  repl "fun x : A -> x";
  [%expect {| ((ty (A -> A)) (result (abs . 0))) |}];
  repl "#t; #t";
  repl "#u; #u; #f";
  [%expect
    {|
    (ty_error ("[ESeq (t, t')] expected t to be unit" (ty_t bool)))
    ((ty bool) (result #f))
    |}];
  repl "(let x = #t in let f = (fun x : bool -> x) in f x)";
  repl "(let x = #t in let y = x in y)";
  repl "(let x = #t in let y = (#t #f) in x)";
  repl "(let y = x in let x = #t in x)";
  [%expect
    {|
    ((ty bool) (result #t))
    ((ty bool) (result #t))
    (ty_error ("attempting to apply to non-arrow type" (ty_f bool)))
    (ty_error ("var not in context" x (ctx ())))
     |}];
  repl "#t as bool";
  repl "(fun x : bool -> x) as (bool -> bool)";
  repl "(fun x : bool -> x) as bool";
  [%expect
    {|
    ((ty bool) (result #t))
    ((ty (bool -> bool)) (result (abs . 0)))
    (ty_error
     ("annotated and derived type differ" (ty_t (bool -> bool))
      (ty_annotated bool)))
    |}];
  repl "(fun x : A -> x) as (A -> A)";
  [%expect {| ((ty (A -> A)) (result (abs . 0))) |}];
  let tup = "{ #t, (fun x : bool -> x), #f }" in
  repl tup;
  [%expect
    {| ((ty ({ bool , (bool -> bool) , bool })) (result ({ #t , (abs . 0) , #f }))) |}];
  repl [%string "%{tup}.0"];
  repl [%string "%{tup}.1"];
  repl [%string "%{tup}.2"];
  repl [%string "%{tup}.3"];
  [%expect
    {|
    ((ty bool) (result #t))
    ((ty (bool -> bool)) (result (abs . 0)))
    ((ty bool) (result #f))
    (ty_error
     ("tuple projection on invalid index" (tys (bool (bool -> bool) bool)) (i 3)))
    |}];
  let record = "{ one = #t, two = { nest = (fun x : bool -> x) }}" in
  repl record;
  repl [%string "%{record}.one"];
  repl [%string "%{record}.two"];
  repl [%string "(%{record}.two).nest"];
  [%expect
    {|
    ((ty ({ one : bool , two : ({ nest : (bool -> bool) }) }))
     (result ({ one : #t , two : ({ nest : (abs . 0) }) })))
    ((ty bool) (result #t))
    ((ty ({ nest : (bool -> bool) })) (result ({ nest : (abs . 0) })))
    ((ty (bool -> bool)) (result (abs . 0)))
    |}];
  repl [%string "%{record}.three"];
  [%expect
    {|
    (ty_error
     ("record missing field" (tys ((one bool) (two ({ nest : (bool -> bool) }))))
      (l three)))
    |}];
  let option = "< some : bool , none >" in
  repl [%string "< none > as %{option}"];
  repl [%string "< some #t > as %{option}"];
  repl [%string "< some #t > as %{option}"];
  [%expect
    {|
    ((ty (< some : bool , none >)) (result (< none : #u >)))
    ((ty (< some : bool , none >)) (result (< some : #t >)))
    ((ty (< some : bool , none >)) (result (< some : #t >)))
    |}];
  repl [%string "< some #u > as %{option}"];
  repl [%string "< yes #t > as %{option}"];
  repl [%string "< some #t > as < some : bool , some : bool >"];
  [%expect
    {|
    (ty_error ("incorrect variant type" (ty_infer unit) (ty_anno bool)))
    (ty_error ("field missing in variant" (ty (< some : bool , none >)) (l yes)))
    (ty_error ("duplicated labels in fields" (fields (some some))))
    |}];
  let some_true = [%string "< some #t > as %{option}"] in
  repl [%string "match %{some_true} with | some x -> x | none -> #t"];
  [%expect {| ((ty bool) (result #t)) |}];
  repl [%string "match %{some_true} with | some x -> x | none -> #u"];
  [%expect {| (ty_error ("unequal types across branches" (ty_cases (bool unit)))) |}];
  repl [%string "match %{some_true} with | some x -> #t"];
  repl [%string "match %{some_true} with | some x -> #t | some x -> #t | none -> #t"];
  [%expect
    {|
    (ty_error
     ("unexpected cases for variant" (case_labels (some))
      (variant_labels (some none))))
    (ty_error ("duplicated labels in fields" (fields (some some none))))
     |}];
  repl "Z";
  repl "S Z";
  repl "S (pred (S Z))";
  repl "iszero (pred (S Z))";
  repl "if iszero Z then #t else #f";
  repl "S #t";
  [%expect
    {|
    ((ty nat) (result Z))
    ((ty nat) (result (S Z)))
    ((ty nat) (result (S Z)))
    ((ty bool) (result #t))
    ((ty bool) (result #t))
    (ty_error ("expected succ to take nat" (ty_t bool)))
    |}];
  repl
    {|
     letrec f : (nat -> nat) =
       fun x : nat ->
         if iszero x
           then Z
           else f (pred x)
     in f (S (S Z))
     |};
  [%expect {| ((ty nat) (result Z)) |}]
;;

let%expect_test "cool examples" =
  let weekday = "(< mon , tue , wed , thu , fri >)" in
  repl
    [%string
      {|
      let next =
        fun w : %{weekday} ->
          match w with
          | mon -> < tue > as %{weekday}
          | tue -> < wed > as %{weekday}
          | wed -> < thu > as %{weekday}
          | thu -> < fri > as %{weekday}
          | fri -> < mon > as %{weekday}
       in
       next (< thu > as %{weekday})
      |}];
  [%expect {| ((ty (< mon , tue , wed , thu , fri >)) (result (< fri : #u >))) |}]
;;

let%expect_test "addition (no closures)" =
  let pair = "({ x : nat , y : nat })" in
  repl
    [%string
      {|
      letrec add : (%{pair} -> nat) =
        (fun xy : %{pair} ->
          (if (iszero (xy.x))
            then (xy.y)
            else (add ({ x = (pred (xy.x)) , y = (S (xy.y)) }))))
      in add { x = S (S Z), y = S (S Z) }
      |}];
  [%expect {| ((ty nat) (result (S (S (S (S Z)))))) |}]
;;

let%expect_test "addition (closures)" =
  repl
    [%string
      {|
      letrec add : (nat -> nat -> nat) =
        (fun x : nat ->
          (fun y : nat ->
            (if (iszero x)
              then y
              else (add (pred x) (S y)))))
      in add (S (S Z)) (S (S Z))
      |}];
  [%expect {| ((ty nat) (result (S (S (S (S Z)))))) |}]
;;
