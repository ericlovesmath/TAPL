open Core
open Parser
module Eval = Eval
module Typecheck = Typecheck
module Types = Types

let repl (s : string) =
  let t = s |> Lexer.of_string |> Lexer.lex |> Parser.run t_p in
  match t with
  | Error parse_err -> print_s [%message (parse_err : Error.t)]
  | Ok t ->
    (match Typecheck.typecheck t with
     | Error ty_error -> print_s [%message (ty_error : Error.t)]
     | Ok ty ->
       let result = Eval.eval (Eval.remove_names t) in
       print_s [%message (ty : Types.ty) (result : Simply_typed_extended.Types.nameless)])
;;

(* NOTE: This function is only so that each [expect_test]'s results does not depend
   on the number of tests coming before it that generates a recursive type *)
let test s =
  Typecheck.Unique_id.For_testing.reset_counter ();
  repl s
;;

let%expect_test "typechecker tests prior to extending" =
  test "#t";
  [%expect {| ((ty bool) (result #t)) |}];
  test "if #t then #f else #f";
  [%expect {| ((ty bool) (result #f)) |}];
  test "if (fun (x : bool) -> x) then #t else #f";
  [%expect
    {| (ty_error ("[if] condition doesn't subsume to TyBool" (ty_c (bool -> bool)))) |}];
  test "if #t then #t else (fun (x : bool) -> x)";
  [%expect {| ((ty top) (result #t)) |}];
  test "y";
  [%expect {| (ty_error ("var not in context" y (ctx ()))) |}];
  let id = "(fun (x : bool) -> x)" in
  test id;
  [%expect {| ((ty (bool -> bool)) (result (abs . 0))) |}];
  test [%string "(%{id} #t)"];
  [%expect {| ((ty bool) (result #t)) |}];
  test [%string "(%{id} %{id})"];
  [%expect
    {|
    (ty_error
     ("arg's type does not subsume expected input type" (ty_f (bool -> bool))
      (ty_x (bool -> bool))))
    |}];
  test "(#t #f)";
  [%expect {| (ty_error ("attempting to apply to non-arrow type" (ty_f bool))) |}];
  test "(fun (x : bool -> bool) -> x)";
  [%expect {| ((ty ((bool -> bool) -> (bool -> bool))) (result (abs . 0))) |}]
;;

let%expect_test "extended typechecker tests" =
  test "#u";
  [%expect {| ((ty unit) (result #u)) |}];
  test "fun (x : A) -> x";
  [%expect {| ((ty (A -> A)) (result (abs . 0))) |}];
  test "#t; #t";
  test "#u; #u; #f";
  [%expect
    {|
    (ty_error ("[ESeq (t, t')] expected t to be unit" (ty_t bool)))
    ((ty bool) (result #f))
    |}];
  test "(let x = #t in let f = (fun (x : bool) -> x) in f x)";
  test "(let x = #t in let y = x in y)";
  test "(let x = #t in let y = (#t #f) in x)";
  test "(let y = x in let x = #t in x)";
  [%expect
    {|
    ((ty bool) (result #t))
    ((ty bool) (result #t))
    (ty_error ("attempting to apply to non-arrow type" (ty_f bool)))
    (ty_error ("var not in context" x (ctx ())))
    |}];
  test "#t as bool";
  test "(fun (x : bool) -> x) as (bool -> bool)";
  test "(fun (x : bool) -> x) as bool";
  [%expect
    {|
    ((ty bool) (result #t))
    ((ty (bool -> bool)) (result (abs . 0)))
    ((ty bool) (result (abs . 0)))
    |}];
  test "(fun (x : A) -> x) as (A -> A)";
  [%expect {| ((ty (A -> A)) (result (abs . 0))) |}];
  let tup = "{ #t, (fun (x : bool) -> x), #f }" in
  test tup;
  [%expect
    {| ((ty ({ bool , (bool -> bool) , bool })) (result ({ #t , (abs . 0) , #f }))) |}];
  test [%string "%{tup}.0"];
  test [%string "%{tup}.1"];
  test [%string "%{tup}.2"];
  test [%string "%{tup}.3"];
  [%expect
    {|
    ((ty bool) (result #t))
    ((ty (bool -> bool)) (result (abs . 0)))
    ((ty bool) (result #f))
    (ty_error
     ("tuple projection on invalid index" (tys (bool (bool -> bool) bool)) (i 3)))
    |}];
  let record = "{ one = #t, two = { nest = (fun (x : bool) -> x) }}" in
  test record;
  test [%string "%{record}.one"];
  test [%string "%{record}.two"];
  test [%string "(%{record}.two).nest"];
  [%expect
    {|
    ((ty ({ one : bool , two : ({ nest : (bool -> bool) }) }))
     (result ({ one : #t , two : ({ nest : (abs . 0) }) })))
    ((ty bool) (result #t))
    ((ty ({ nest : (bool -> bool) })) (result ({ nest : (abs . 0) })))
    ((ty (bool -> bool)) (result (abs . 0)))
    |}];
  test [%string "%{record}.three"];
  [%expect
    {|
    (ty_error
     ("record missing field" (tys ((one bool) (two ({ nest : (bool -> bool) }))))
      (l three)))
    |}];
  test "< none >";
  test "< some #t >";
  [%expect
    {|
    ((ty (< none >)) (result (< none : #u >)))
    ((ty (< some : bool >)) (result (< some : #t >)))
    |}];
  test [%string "match < some #t > with | some x -> x | none -> #t"];
  [%expect
    {|
    (ty_error
     ("unexpected cases for variant" (case_labels (some none))
      (variant_labels (some))))
    |}];
  test [%string "match < some #t > with | some x -> x | none -> #u"];
  [%expect
    {|
    (ty_error
     ("unexpected cases for variant" (case_labels (some none))
      (variant_labels (some))))
    |}];
  test [%string "match < some #t > with | some x -> #t"];
  test [%string "match < some #t > with | some x -> #t | some x -> #t | none -> #t"];
  [%expect
    {|
    ((ty bool) (result #t))
    (ty_error ("duplicated labels in fields" (fields (some some none))))
    |}];
  test "Z";
  test "S Z";
  test "S (pred (S Z))";
  test "iszero (pred (S Z))";
  test "if iszero Z then #t else #f";
  test "S #t";
  [%expect
    {|
    ((ty nat) (result Z))
    ((ty nat) (result (S Z)))
    ((ty nat) (result (S Z)))
    ((ty bool) (result #t))
    ((ty bool) (result #t))
    (ty_error ("expected succ to take term subsumed to nat" (ty_t bool)))
    |}];
  test
    {|
     letrec (f : nat -> nat) =
       fun (x : nat) ->
         if iszero x
           then Z
           else f (pred x)
     in f (S (S Z))
     |};
  [%expect {| ((ty nat) (result Z)) |}]
;;

let%expect_test "cool examples" =
  let weekday = "(< mon , tue , wed , thu , fri >)" in
  test
    [%string
      {|
      let next =
        fun (w : %{weekday}) ->
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
  test
    [%string
      {|
      letrec (add : %{pair} -> nat) =
        (fun (xy : %{pair}) ->
          (if (iszero (xy.x))
            then (xy.y)
            else (add ({ x = (pred (xy.x)) , y = (S (xy.y)) }))))
      in add { x = S (S Z), y = S (S Z) }
      |}];
  [%expect {| ((ty nat) (result (S (S (S (S Z)))))) |}]
;;

let%expect_test "addition (closures)" =
  test
    [%string
      {|
      letrec (add : nat -> nat -> nat) =
        (fun (x : nat) ->
          (fun (y : nat) ->
            (if (iszero x)
              then y
              else (add (pred x) (S y)))))
      in add (S (S Z)) (S (S Z))
      |}];
  [%expect {| ((ty nat) (result (S (S (S (S Z)))))) |}]
;;

let%expect_test "ref tests" =
  test "let x = ref Z in !x";
  [%expect {| ((ty nat) (result Z)) |}];
  test "let not = ref (fun (x : bool) -> if x then #f else #t) in (!not) #t";
  [%expect {| ((ty bool) (result #f)) |}];
  test "let x = ref Z in (x := S Z); !x";
  [%expect {| ((ty nat) (result (S Z))) |}]
;;

let%expect_test "recursive type tests" =
  test "(fun (f : rec x . x -> nat) -> f f)";
  [%expect {| ((ty ((rec x . (x -> nat)) -> nat)) (result (abs . (0 0)))) |}];
  let check_type_equal ty =
    test
      [%string
        {|
        let test = fun (f : rec x . x -> nat) -> f f in
        let typecheck = (test as (%{ty})) in
        #u
        |}]
  in
  check_type_equal "rec a . a -> nat";
  check_type_equal "rec x . (x -> nat) -> nat";
  check_type_equal "rec x . ((x -> nat) -> nat) -> nat";
  check_type_equal "rec x . (rec x . (x -> nat) -> nat) -> nat";
  [%expect
    {|
    ((ty unit) (result #u))
    ((ty unit) (result #u))
    ((ty unit) (result #u))
    ((ty unit) (result #u))
    |}]
;;

let%expect_test "inductive list tests" =
  let nats = "(rec a . < nil, cons : { nat, a } >)" in
  let nil = [%string "(< nil > as %{nats})"] in
  let cons x y = [%string "(< cons { %{x} , %{y} } > as %{nats})"] in
  test (cons "S Z" nil);
  [%expect
    {|
    ((ty (rec a . (< nil , cons : ({ nat , a }) >)))
     (result (< cons : ({ (S Z) , (< nil : #u >) }) >)))
    |}];
  test
    [%string
      {|
      let empty =
        fun (xs : %{nats}) ->
          match xs with
          | nil -> #t
          | cons x -> #f
      in
      empty %{cons "Z" (cons "S Z" nil)}
      |}];
  [%expect {| ((ty bool) (result #f)) |}];
  test
    [%string
      {|
      letrec (map : (nat -> nat) -> %{nats} -> %{nats}) =
        fun (f : nat -> nat) ->
          fun (xs : %{nats}) ->
            match xs with
            | nil -> %{nil}
            | cons t -> %{cons "f (t.0)" "map f (t.1)"}
      in
      let inc =
        fun (n : nat) -> S n
      in
      map inc %{cons "Z" (cons "S Z" nil)}
      |}];
  [%expect
    {|
    ((ty (rec a . (< nil , cons : ({ nat , a }) >)))
     (result
      (< cons : ({ (S Z) , (< cons : ({ (S (S Z)) , (< nil : #u >) }) >) }) >)))
    |}]
;;

let%expect_test "subsumption tests" =
  test "(fun (x : { a : nat }) -> x.a) { a = Z, b = S Z }";
  [%expect {| ((ty nat) (result Z)) |}];
  test "(fun (x : top) -> x) (ref #t)";
  [%expect {| ((ty top) (result 0)) |}];
  test "(fun (x : top) -> x as bool) (ref #t)";
  [%expect {| ((ty bool) (result 0)) |}];
  test "(fun (r : { y : bool, x : bool }) -> { x = r.y, y = r.x }) { x = #t, y = #f }";
  [%expect {| ((ty ({ x : bool , y : bool })) (result ({ x : #f , y : #t }))) |}];
  test
    {|
       (fun (r : ref { y : bool, x : bool }) ->
         r := { x = (!r).y, y = (!r).x })
       ref { x = #t, y = #f }
       |};
  [%expect {| ((ty unit) (result #u)) |}]
;;

let%expect_test "join tests" =
  test "if #t then { x = Z } else { y = #f }";
  [%expect {| ((ty top) (result ({ x : Z }))) |}];
  test "if #t then { x = Z, y = #f } else { z = #f, x = S Z }";
  [%expect {| ((ty ({ x : nat })) (result ({ x : Z , y : #f }))) |}];
  test "if #t then < some #t > else < none >";
  [%expect {| ((ty (< none , some : bool >)) (result (< some : #t >))) |}];
  test "if #t then < some #t > else < none >";
  [%expect {| ((ty (< none , some : bool >)) (result (< some : #t >))) |}];
  test "if #t then #t as bot else #t";
  [%expect {| ((ty bool) (result #t)) |}];
  test
    [%string
      {|
      let next =
        fun (w : <mon, tue, wed, thu, fri>) ->
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

let%expect_test "recursive subtyping and join tests" =
  test
    {|
    fun (x : unit) ->
      if #t
        then (fix (fun (s : rec a . { head : nat, tail : a, val : nat }) -> s))
        else (fix (fun (s : rec b . { head : nat, tail : b, info : bool }) -> s))
    |};
  [%expect
    {|
    ((ty (unit -> (rec v0 . ({ head : nat , tail : v0 }))))
     (result (abs . (if #t then (fix (abs . 0)) else (fix (abs . 0))))))
    |}];
  test
    {|
    if #f
      then (< a Z > as rec v . < a : nat, c : v >)
      else (< b #t > as rec v . < b : bool, c : v >)
    |};
  [%expect
    {| ((ty (rec v0 . (< c : v0 , b : bool , a : nat >))) (result (< b : #t >))) |}];
  test
    {|
    fun (x : unit) ->
      if #t
        then (fix (fun (f : rec a . nat -> a) -> f))
        else (fun (x : nat) -> (fix (fun (f : rec a . nat -> a) -> f)))
    |};
  [%expect
    {|
    ((ty (unit -> (nat -> (rec a . (nat -> a)))))
     (result (abs . (if #t then (fix (abs . 0)) else (abs . (fix (abs . 0)))))))
    |}]
;;

(* test "if #t then error else #t"; *)
(* [%expect {| ((ty bool) (result error)) |}]; *)
