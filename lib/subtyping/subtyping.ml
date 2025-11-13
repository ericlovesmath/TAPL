open Core
open Parser
module Eval = Eval
module Types = Types
module Typecheck = Typecheck

(* NOTE: There is no evaluation, but it should be identical to [simply_typed_extended] if needed *)
let repl (s : string) =
  let t = s |> Lexer.of_string |> Lexer.lex |> Parser.run t_p in
  let sexp =
    match t with
    | Error parse_err -> [%message (parse_err : Error.t)]
    | Ok t ->
      (match Typecheck.typecheck t with
       | Error type_err -> [%message (type_err : Error.t)]
       | Ok ty ->
         let result = Eval.eval (Eval.remove_names t) in
         [%message (ty : Types.ty) (result : Types.nameless)])
  in
  print_s sexp
;;

let%expect_test "typechecker tests prior to extending" =
  repl "#t";
  [%expect {| ((ty bool) (result #t)) |}];
  repl "if #t then #f else #f";
  [%expect {| ((ty bool) (result #f)) |}];
  repl "if (fun x : bool -> x) then #t else #f";
  [%expect
    {| (type_err ("[if] condition doesn't subsume to TyBool" (ty_c (bool -> bool)))) |}];
  repl "if #t then #t else (fun x : bool -> x)";
  [%expect {| ((ty top) (result #t)) |}];
  repl "y";
  [%expect {| (type_err ("var not in context" y (ctx ()))) |}];
  let id = "(fun x : bool -> x)" in
  repl id;
  [%expect {| ((ty (bool -> bool)) (result (abs . 0))) |}];
  repl [%string "(%{id} #t)"];
  [%expect {| ((ty bool) (result #t)) |}];
  repl [%string "(%{id} %{id})"];
  [%expect
    {|
    (type_err
     ("arg's type does not subsume expected input type" (ty_f (bool -> bool))
      (ty_x (bool -> bool))))
    |}];
  repl "(#t #f)";
  [%expect {| (type_err ("attempting to apply to non-arrow type" (ty_f bool))) |}];
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
    (type_err ("[ESeq (t, t')] expected t to be unit" (ty_t bool)))
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
    (type_err ("attempting to apply to non-arrow type" (ty_f bool)))
    (type_err ("var not in context" x (ctx ())))
    |}];
  repl "#t as bool";
  repl "(fun x : bool -> x) as (bool -> bool)";
  repl "(fun x : bool -> x) as bool";
  [%expect
    {|
    ((ty bool) (result #t))
    ((ty (bool -> bool)) (result (abs . 0)))
    ((ty bool) (result (abs . 0)))
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
    (type_err
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
    (type_err
     ("record missing field" (tys ((one bool) (two ({ nest : (bool -> bool) }))))
      (l three)))
    |}];
  let option = "< some : bool , none >" in
  repl [%string "< none >"];
  repl [%string "< some #t >"];
  repl [%string "< some #t > as %{option}"];
  [%expect
    {|
    ((ty (< none >)) (result (< none : #u >)))
    ((ty (< some : bool >)) (result (< some : #t >)))
    ((ty (< some : bool , none >)) (result (< some : #t >)))
    |}];
  repl [%string "< some #u >"];
  repl [%string "< yes #t >"];
  repl [%string "< some #t > as < some : bool , some : bool >"];
  [%expect
    {|
    ((ty (< some >)) (result (< some : #u >)))
    ((ty (< yes : bool >)) (result (< yes : #t >)))
    ((ty (< some : bool , some : bool >)) (result (< some : #t >)))
    |}];
  let some_true = "< some #t >" in
  repl [%string "match %{some_true} with | some x -> x | none -> #t"];
  [%expect
    {|
    (type_err
     ("unexpected cases for variant" (case_labels (some none))
      (variant_labels (some))))
    |}];
  repl [%string "match %{some_true} with | some x -> x | none -> #u"];
  [%expect
    {|
    (type_err
     ("unexpected cases for variant" (case_labels (some none))
      (variant_labels (some))))
    |}];
  repl [%string "match %{some_true} with | some x -> #t"];
  repl [%string "match %{some_true} with | some x -> #t | some x -> #t | none -> #t"];
  [%expect
    {|
    ((ty bool) (result #t))
    (type_err ("duplicated labels in fields" (fields (some some none))))
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
    ((ty nat) (result (S #t)))
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

let%expect_test "ref tests" =
  repl "let x = ref Z in !x";
  [%expect {| ((ty nat) (result Z)) |}];
  repl "let not = ref (fun x : bool -> if x then #f else #t) in (!not) #t";
  [%expect {| ((ty bool) (result #f)) |}];
  repl "let x = ref Z in (x := S Z); !x";
  [%expect {| ((ty nat) (result (S Z))) |}]
;;

let%expect_test "subsumption tests" =
  repl "(fun x : { a : nat } -> x.a) { a = Z, b = S Z }";
  [%expect {| ((ty nat) (result Z)) |}];
  repl "(fun x : top -> x) (ref #t)";
  [%expect {| ((ty top) (result 0)) |}];
  repl "(fun x : top -> x as bool) (ref #t)";
  [%expect {| ((ty bool) (result 0)) |}];
  repl "(fun r : { y : bool, x : bool } -> { x = r.y, y = r.x }) { x = #t, y = #f }";
  [%expect {| ((ty ({ x : bool , y : bool })) (result ({ x : #f , y : #t }))) |}];
  repl
    {|
       (fun r : ref { y : bool, x : bool } ->
         r := { x = (!r).y, y = (!r).x })
       ref { x = #t, y = #f }
       |};
  [%expect {| ((ty unit) (result #u)) |}]
;;

let%expect_test "join tests" =
  repl "if #t then { x = Z } else { y = #f }";
  [%expect {| ((ty top) (result ({ x : Z }))) |}];
  repl "if #t then { x = Z, y = #f } else { z = #f, x = S Z }";
  [%expect {| ((ty ({ x : nat })) (result ({ x : Z , y : #f }))) |}];
  repl "if #t then < some #t > else < none >";
  [%expect {| ((ty (< none , some : bool >)) (result (< some : #t >))) |}];
  repl "if #t then < some #t > else < none >";
  [%expect {| ((ty (< none , some : bool >)) (result (< some : #t >))) |}];
  repl "if #t then #t as bot else #t";
  [%expect {| ((ty bool) (result #t)) |}];
  repl "if #t then error else #t";
  [%expect {| ((ty bool) (result error)) |}];
  repl
    [%string
      {|
      let next =
        fun w : <mon, tue, wed, thu, fri> ->
          match w with
          | mon -> < tue >
          | tue -> < wed >
          | wed -> < thu >
          | thu -> < fri >
          | fri -> < mon >
       in
       next (< thu >)
      |}];
  [%expect {| ((ty (< thu , mon , tue , fri , wed >)) (result (< fri : #u >))) |}]
;;
