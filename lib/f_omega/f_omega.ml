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
  [%expect {| (ty_error ("attempting to apply to non-arrow type" (ty_f bool))) |}];
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
    ((ty (forall A . (A -> A))) (result (abs . 0)))
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
    ((ty (forall A . (forall B . ((A -> (B -> B)) -> (B -> B)))))
     (result (abs . (abs . 0))))
    ((ty
      (forall A .
       (A ->
        ((forall C . ((A -> (C -> C)) -> (C -> C))) ->
         (forall B . ((A -> (B -> B)) -> (B -> B)))))))
     (result (abs . (abs . (abs . (abs . ((1 3) ((2 1) 0))))))))
    |}]
;;

let%expect_test "existential types typechecking" =
  repl
    {|
    {*nat, { a = Z, f = fun (x : nat) -> S x }}
      as { exists X, { a : X, f: X -> X } }
    |};
  [%expect {|
    ((ty ({ exists A , ({ a : A , f : (A -> A) }) }))
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
    ((ty ({ exists A , A })) (result Z))
    ((ty ({ exists A , A })) (result #t))
    (ty_error
     ("pack term does not match declared existential type" (ty_t nat)
      (expected_ty bool)))
    (ty_error ("failed to find variable" Y (ctx (X))))
    ((ty ({ exists A , bool })) (result #t))
    ((ty ({ exists A , A })) (result Z))
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
    ((ty ({ exists A , ({ init : A , get : (A -> nat) , inc : (A -> A) }) }))
     (result ({ init : Z , get : (abs . 0) , inc : (abs . (S 0)) })))
    (ty_error
     ("existential type variable escapes scope"
      (result_ty ({ init : 0 , get : (0 -> nat) , inc : (0 -> 0) }))))
    ((ty nat) (result (S Z)))
    |}]
;;
