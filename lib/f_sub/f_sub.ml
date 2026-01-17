open Core
open Parser
module Types = Types
module Typecheck = Typecheck
module Eval = Eval

let repl (s : string) =
  let t = s |> Lexer.of_string |> Lexer.lex |> Parser.run t_p in
  let sexp =
    match t with
    | Error parse_err -> [%message (parse_err : Error.t)]
    | Ok t ->
      (match Typecheck.typecheck t with
       | Error ty_error -> [%message (ty_error : Error.t)]
       | Ok ty ->
         let result = Eval.eval (Eval.remove_names t) in
         [%message (ty : Types.ty) (result : General_interpreter.t)])
  in
  print_s sexp
;;

let%expect_test "typechecker tests prior to extending" =
  repl "#t";
  [%expect {| ((ty bool) (result #t)) |}];
  repl "if #t then #f else #f";
  [%expect {| ((ty bool) (result #f)) |}];
  repl "if (fun (x : bool) -> x) then #t else #f";
  [%expect {| (ty_error ("[if] cond doesn't subsume to TyBool" (ty_c (bool -> bool)))) |}];
  repl "if #t then #t else (fun (x : bool) -> x)";
  [%expect {| ((ty top) (result #t)) |}];
  repl "y";
  [%expect {| (ty_error ("var not in context" y (ctx ()))) |}];
  let id = "(fun (x : bool) -> x)" in
  repl id;
  [%expect {| ((ty (bool -> bool)) (result (abs . 0))) |}];
  repl [%string "(%{id} #t)"];
  [%expect {| ((ty bool) (result #t)) |}];
  repl [%string "(%{id} %{id})"];
  [%expect
    {|
    (ty_error
     ("arg can't be applied to func" (ty_f (bool -> bool)) (ty_x (bool -> bool))))
    |}];
  repl "(#t #f)";
  [%expect {| (ty_error ("can't to apply to non-arrow type" (ty_f bool))) |}];
  repl "(fun (x : bool -> bool) -> x)";
  [%expect {| ((ty ((bool -> bool) -> (bool -> bool))) (result (abs . 0))) |}]
;;

let%expect_test "seq tuple and record tests" =
  repl "#t; #t";
  repl "#u; #u; #f";
  [%expect
    {|
    (ty_error ("[ESeq (t, t')] expected t to be unit" (ty_t bool)))
    ((ty bool) (result #f))
    |}];
  let tup = "{ #t, fun (x : bool) -> x, #f }" in
  repl tup;
  repl [%string "%{tup}.0"];
  repl [%string "%{tup}.1"];
  repl [%string "%{tup}.2"];
  repl [%string "%{tup}.3"];
  [%expect
    {|
    ((ty ({ bool , (bool -> bool) , bool })) (result ({ #t , (abs . 0) , #f })))
    ((ty bool) (result #t))
    ((ty (bool -> bool)) (result (abs . 0)))
    ((ty bool) (result #f))
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
    ((ty ({ one : bool , two : ({ nest : (bool -> bool) }) }))
     (result ({ one : #t , two : ({ nest : (abs . 0) }) })))
    ((ty bool) (result #t))
    ((ty ({ nest : (bool -> bool) })) (result ({ nest : (abs . 0) })))
    ((ty (bool -> bool)) (result (abs . 0)))
    (ty_error
     ("record missing field" (tys ((one bool) (two ({ nest : (bool -> bool) }))))
      (l three)))
    |}]
;;

let%expect_test "ref tests" =
  repl "let x = ref Z in !x";
  [%expect {| ((ty nat) (result Z)) |}];
  repl "let not = ref (fun (x : bool) -> if x then #f else #t) in (!not) #t";
  [%expect {| ((ty bool) (result #f)) |}];
  repl "let x = ref Z in (x := S Z); !x";
  [%expect {| ((ty nat) (result (S Z))) |}]
;;

let%expect_test "universal types typechecking" =
  repl "fun X . fun (x : X) -> x";
  repl "let id = fun X . fun (x : X) -> x in id [nat]";
  [%expect
    {|
    ((ty (forall A <: top . (A -> A))) (result (abs . 0)))
    ((ty (nat -> nat)) (result (abs . 0)))
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
    ((ty (forall A <: top . (forall B <: top . ((A -> (B -> B)) -> (B -> B)))))
     (result (abs . (abs . 0))))
    ((ty
      (forall A <: top .
       (A ->
        ((forall C <: top . ((A -> (C -> C)) -> (C -> C))) ->
         (forall B <: top . ((A -> (B -> B)) -> (B -> B)))))))
     (result (abs . (abs . (abs . (abs . ((1 3) ((2 1) 0))))))))
    |}]
;;

let%expect_test "existential types typechecking" =
  repl
    {|
    {*nat, { a = Z, f = fun (x : nat) -> S x }}
      as { exists X, { a : X, f: X -> X } }
    |};
  [%expect
    {|
    ((ty ({ exists A <: top , ({ a : A , f : (A -> A) }) }))
     (result ({ a : Z , f : (abs . (S 0)) })))
    |}];
  repl "{ *nat, Z } as { exists X , X }";
  repl "{ *bool, #t } as { exists X , X }";
  repl "{ *bool, Z } as { exists X , X }";
  repl "{ *nat, Z } as { exists X , Y }";
  repl "{ *bool, #t } as { exists X , bool }";
  repl
    "if #t then ({ *nat, Z } as { exists X , X }) else ({ *bool, #t } as { exists Y , Y \
     })";
  [%expect
    {|
    ((ty ({ exists A <: top , A })) (result Z))
    ((ty ({ exists A <: top , A })) (result #t))
    (ty_error
     ("pack term does not match declared existential type" (ty_t nat)
      (expected_ty bool)))
    (ty_error ("failed to find" Y (ctx ((X top)))))
    ((ty ({ exists A <: top , bool })) (result #t))
    ((ty ({ exists A <: top , A })) (result Z))
    |}];
  let ty_counter = "{ exists C, { init : C, get : C -> nat, inc : C -> C } }" in
  let counter =
    [%string
      {|
        {*nat, { init = Z, get = fun (x : nat) -> x, inc = fun (x : nat) -> S x }}
          as %{ty_counter}
      |}]
  in
  repl counter;
  repl [%string "let { Counter, c } = (%{counter}) in c"];
  repl [%string "let { Counter, c } = (%{counter}) in ((c.get) ((c.inc) (c.init)))"];
  [%expect
    {|
    ((ty
      ({ exists A <: top , ({ init : A , get : (A -> nat) , inc : (A -> A) }) }))
     (result ({ init : Z , get : (abs . 0) , inc : (abs . (S 0)) })))
    (ty_error
     ("existential type variable escapes scope"
      (ty_res ({ init : 0 , get : (0 -> nat) , inc : (0 -> 0) }))))
    ((ty nat) (result (S Z)))
    |}]
;;

let%expect_test "subsumption tests" =
  repl "(fun (x : { a : nat }) -> x.a) { a = Z, b = S Z }";
  [%expect {| ((ty nat) (result Z)) |}];
  repl "(fun (x : top) -> x) (ref #t)";
  [%expect {| ((ty top) (result 0)) |}];
  repl "(fun (r : { y : bool, x : bool }) -> { x = r.y, y = r.x }) { x = #t, y = #f }";
  [%expect {| ((ty ({ x : bool , y : bool })) (result ({ x : #f , y : #t }))) |}];
  repl
    {|
    (fun (r : ref { y : bool, x : bool }) ->
      r := { x = (!r).y, y = (!r).x })
    ref { x = #t, y = #f }
    |};
  [%expect {| ((ty unit) (result #u)) |}];
  (* Function Contravariance and Covariance *)
  let app_f = "fun (f : { x : bool, y : bool } -> bool) -> f { x = #t, y = #f }" in
  let my_f = "fun (r : { x : bool }) -> r.x" in
  repl [%string "(%{app_f}) (%{my_f})"];
  [%expect {| ((ty bool) (result #t)) |}];
  let app_g = "fun (g : bool -> { x : bool }) -> (g #t).x" in
  let my_g = "fun (b : bool) -> { x = b, y = Z }" in
  repl [%string "(%{app_g} %{my_g})"];
  [%expect {| ((ty bool) (result #t)) |}]
;;

let%expect_test "join tests" =
  repl "if #t then { x = Z } else { y = #f }";
  [%expect {| ((ty top) (result ({ x : Z }))) |}];
  repl "if #t then { x = Z, y = #f } else { z = #f, x = S Z }";
  [%expect {| ((ty ({ x : nat })) (result ({ x : Z , y : #f }))) |}]
;;

let%expect_test "bounded quantification (forall)" =
  let id_sub = "(fun X <: { a : bool } . fun (x : X) -> x.a)" in
  repl [%string "(%{id_sub} [{ a : bool, b : nat }]) { a = #t, b = Z }"];
  [%expect {| ((ty bool) (result #t)) |}];
  repl [%string "(%{id_sub} [nat]) Z"];
  [%expect
    {|
    (ty_error
     ("type argument does not satisfy bound" (ty_arg nat)
      (ty_sub ({ a : bool }))))
    |}];
  let id_top = "(fun X . fun (x : X) -> x)" in
  repl [%string "(%{id_top} [nat]) Z"];
  [%expect {| ((ty nat) (result Z)) |}]
;;

let%expect_test "bounded existential types (exists)" =
  let ty_ex = "{ exists X <: { get : nat }, { state : X } }" in
  repl
    [%string
      {|
      {*
        { get : nat, set : nat -> unit },
        { state = { get = Z, set = fun (n:nat) -> #u } }
      } as %{ty_ex}
      |}];
  [%expect
    {|
    ((ty ({ exists A <: ({ get : nat }) , ({ state : A }) }))
     (result ({ state : ({ get : Z , set : (abs . #u) }) })))
    |}];
  repl [%string "{* { other : nat }, { state = { other = Z } } } as %{ty_ex}"];
  [%expect
    {|
    (ty_error
     ("witness type does not satisfy bound" (ty_real ({ other : nat }))
      (ty_bound ({ get : nat }))))
    |}]
;;

let%expect_test "quantifier subtyping" =
  repl
    {|
    (fun (g : forall X <: {a : bool} . X -> {a:bool}) -> #t)
    (fun X <: {a : bool} . fun (x : X) -> x)
    |};
  [%expect {| ((ty bool) (result #t)) |}]
;;
