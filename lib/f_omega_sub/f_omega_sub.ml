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
  [%expect {| (ty_error ("can't apply to non-arrow type" (ty_f bool))) |}];
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
  [%expect
    {|
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
    (ty_error ("failed to find type variable" Y (ctx ((X top *)))))
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
      (ty_res ({ init : 0 , get : (0 -> nat) , inc : (0 -> 0) }))))
    ((ty nat) (result (S Z)))
    |}]
;;

let%expect_test "type-level computation" =
  repl "fun (x : (fun X :: * . X) bool) -> x";
  [%expect {| ((ty (bool -> bool)) (result (abs . 0))) |}];
  repl "fun (x : (fun X :: * . X -> X) bool) -> x";
  [%expect {| ((ty ((bool -> bool) -> (bool -> bool))) (result (abs . 0))) |}];
  repl "(fun (x : (fun X :: * . X) bool) -> x) #t";
  [%expect {| ((ty bool) (result #t)) |}];
  repl "fun (x : (fun F :: * => * . F bool) (fun X :: * . X)) -> x";
  [%expect {| ((ty (bool -> bool)) (result (abs . 0))) |}];
  repl "fun (x : (fun F :: * => * . F bool) (fun X :: * . X -> X)) -> x";
  [%expect {| ((ty ((bool -> bool) -> (bool -> bool))) (result (abs . 0))) |}]
;;

let%expect_test "higher-order polymorphism" =
  repl "fun F :: * => * . fun (x : F bool) -> x";
  [%expect
    {| ((ty (forall A :: (* => *) . ((A bool) -> (A bool)))) (result (abs . 0))) |}];
  let id_f = "(fun F :: * => * . fun (x : F bool) -> x)" in
  repl [%string "(%{id_f} [fun X :: * . X]) #t"];
  [%expect {| ((ty bool) (result #t)) |}];
  repl [%string "(%{id_f} [fun X :: * . X -> X]) (fun (b : bool) -> b)"];
  [%expect {| ((ty (bool -> bool)) (result (abs . 0))) |}]
;;

let%expect_test "kind errors" =
  repl "fun (x : (fun X :: * . X) [bool]) -> x";
  [%expect
    {|
    (parse_err
     ((chomp_error "satisfy_fail on token LBRACKET at 1:27")
      (contexts ("t_abs at 1:1" "t_atom at 1:1"))))
    |}];
  repl "fun (x : bool nat) -> x";
  [%expect
    {|
    (ty_error
     ("attempting to apply a type of kind * as a type operator" (t1 bool)))
    |}];
  repl "fun (x : (fun X :: * => * . X) bool) -> x";
  [%expect
    {|
    (ty_error
     ("kind mismatch in type application" (t1 (fun :: (* => *) . 0))
      (k1 ((* => *) => (* => *))) (t2 bool) (k2 *)))
    |}]
;;

let%expect_test "Church Encodings" =
  let c_bool = "(forall X :: * . X -> X -> X)" in
  let c_true = "(fun X :: * . fun (t : X) -> fun (f : X) -> t)" in
  let c_false = "(fun X :: * . fun (t : X) -> fun (f : X) -> f)" in
  let c_and =
    [%string
      "(fun (a : %{c_bool}) -> fun (b : %{c_bool}) -> (a [%{c_bool}] b) %{c_false})"]
  in
  repl c_true;
  repl c_false;
  repl [%string "(%{c_and} %{c_true} %{c_false})"];
  [%expect
    {|
    ((ty (forall A . (A -> (A -> A)))) (result (abs . (abs . 1))))
    ((ty (forall A . (A -> (A -> A)))) (result (abs . (abs . 0))))
    ((ty (forall A . (A -> (A -> A)))) (result (abs . (abs . 0))))
    |}];
  let c_nat_ty = "(forall X :: * . (X -> X) -> X -> X)" in
  let c_zero = "(fun X :: * . fun (s : X -> X) -> fun (z : X) -> z)" in
  let c_succ =
    [%string
      {|
      (fun (n : %{c_nat_ty}) ->
        fun X :: * . fun (s : X -> X) ->
          fun (z : X) -> s (n [X] s z))
      |}]
  in
  let c_plus =
    [%string
      {|
      (fun (m : %{c_nat_ty}) ->
        fun (n : %{c_nat_ty}) ->
          fun X :: * . fun (s : X -> X) ->
            fun (z : X) ->
              m [X] s (n [X] s z))
      |}]
  in
  repl c_zero;
  repl [%string "(%{c_succ} %{c_zero})"];
  repl [%string "((%{c_plus} (%{c_succ} %{c_zero})) (%{c_succ} %{c_zero}))"];
  [%expect
    {|
    ((ty (forall A . ((A -> A) -> (A -> A)))) (result (abs . (abs . 0))))
    ((ty (forall A . ((A -> A) -> (A -> A))))
     (result (abs . (abs . (1 (((abs . (abs . 0)) 1) 0))))))
    ((ty (forall A . ((A -> A) -> (A -> A))))
     (result
      (abs .
       (abs .
        (((abs . (abs . (1 (((abs . (abs . 0)) 1) 0)))) 1)
         (((abs . (abs . (1 (((abs . (abs . 0)) 1) 0)))) 1) 0))))))
    |}]
;;

let%expect_test "Option type" =
  let opt_op = "(fun T :: * . forall R :: * . (T -> R) -> R -> R)" in
  let none = "(fun T :: * . fun R :: * . fun (s : T -> R) -> fun (n : R) -> n)" in
  let some =
    "(fun T :: * . fun (v : T) -> fun R :: * . fun (s : T -> R) -> fun (n : R) -> s v)"
  in
  let is_none =
    [%string
      "(fun T :: * . fun (o : (%{opt_op} T)) -> ((o [bool]) (fun (v : T) -> #f)) #t)"]
  in
  repl [%string "(%{none} [nat])"];
  repl [%string "(%{some} [nat] Z)"];
  repl [%string "(%{is_none} [nat]) (%{none} [nat])"];
  repl [%string "(%{is_none} [nat]) (%{some} [nat] Z)"];
  [%expect
    {|
    ((ty (forall A . ((nat -> A) -> (A -> A)))) (result (abs . (abs . 0))))
    ((ty (forall A . ((nat -> A) -> (A -> A)))) (result (abs . (abs . (1 Z)))))
    ((ty bool) (result #t))
    ((ty bool) (result #f))
    |}]
;;

let%expect_test "List type" =
  let list_op = "(fun T :: * . forall R :: * . (T -> R -> R) -> R -> R)" in
  let nil = "(fun T :: * . fun R :: * . fun (c : T -> R -> R) -> fun (n : R) -> n)" in
  let cons =
    [%string
      {|
      (fun T :: * . fun (hd : T) ->
        fun (tl : (%{list_op} T)) ->
          fun R :: * . fun (c : T -> R -> R) ->
            fun (n : R) ->
              c hd (tl [R] c n))
      |}]
  in
  let length =
    [%string
      {|
      (fun T :: * . fun (l : (%{list_op} T)) ->
        ((l [nat]) (fun (x : T) -> fun (acc : nat) -> S acc)) Z)
      |}]
  in
  repl
    [%string
      {|
      let nilNat = %{nil} [nat] in
      let consNat = %{cons} [nat] in
      let lenNat = %{length} [nat] in
      lenNat (consNat (S Z) (consNat Z nilNat))
      |}];
  [%expect "((ty nat) (result (S (S Z))))"]
;;

let%expect_test "f sub tests" =
  repl "fun X <: nat . fun (x : X) -> x";
  [%expect {| ((ty (forall A <: nat . (A -> A))) (result (abs . 0))) |}];
  repl
    {|
    let f = fun X <: nat . fun (x : X) -> x in
    f [nat] (S Z)
    |};
  [%expect {| ((ty nat) (result (S Z))) |}];
  repl "let f = fun X <: nat . fun (x : X) -> x in f [bool] #t";
  [%expect
    {|
    (ty_error
     ("type argument does not satisfy bound" (ty_arg bool) (ty_sub nat)))
    |}];
  repl
    {|
    let f = fun F <: (fun X :: * . top) :: (* => *) . fun (x : nat) -> x in
    f [fun Y :: * . nat]
    |};
  [%expect {| ((ty (nat -> nat)) (result (abs . 0))) |}];
  repl
    {|
    let p = {*nat, S Z} as {exists X <: nat, X} in
    let {X, x} = p in
    S x
    |};
  [%expect {| ((ty nat) (result (S (S Z)))) |}];
  repl "let p = {*bool, #t} as {exists X <: nat, X} in p";
  [%expect
    {|
    (ty_error
     ("witness type does not satisfy bound" (ty_real bool) (ty_bound nat)))
    |}]
;;

let%expect_test "higher-kinded top" =
  repl "fun (x : top :: * => *) -> x";
  [%expect
    {|
    (ty_error
     ("abstraction parameter must have kind *" (ty_v (top :: (* => *)))
      (k (* => *))))
    |}]
;;

let%expect_test "Operator Subtyping" =
  let op = "fun X :: * . { x : X }" in
  repl
    [%string
      {|
      let f = fun (r : (%{op}) nat) -> r.x in
      let record = { x = Z, y = S Z } in
      f record
      |}];
  [%expect {| ((ty nat) (result Z)) |}];
  repl [%string "(fun (f : (fun F :: * => * . F nat) (%{op})) -> f.x) { x = Z, y = #t }"];
  [%expect {| ((ty nat) (result Z)) |}]
;;
