open Core
module U = Untyped_lambda_calculus
module Unique_id = Unique_id.Int ()

type t =
  | Var of int [@quickcheck.generator Int.gen_incl 0 5]
  | Abs of t
  | App of t * t
[@@deriving sexp, equal, quickcheck]

type context = string list

let find (ctx : context) (v : string) : int option =
  let rec find' acc = function
    | [] -> None
    | h :: _ when String.equal h v -> Some acc
    | _ :: t -> find' (acc + 1) t
  in
  find' 0 ctx
;;

let rec remove_names (ctx : context) (t : U.t) : t option =
  let open Option.Let_syntax in
  match t with
  | Var v ->
    let%map index = find ctx v in
    Var index
  | Lam (v, t) ->
    let%map t = remove_names (v :: ctx) t in
    Abs t
  | App (f, x) ->
    let%bind f = remove_names ctx f in
    let%bind x = remove_names ctx x in
    return (App (f, x))
;;

let gen_sym () = "x." ^ Unique_id.to_string (Unique_id.create ())

let rec restore_names (ctx : context) (t : t) : U.t option =
  let open Option.Let_syntax in
  match t with
  | Var i ->
    let%map v = List.nth ctx i in
    U.Var v
  | Abs t ->
    let sym = gen_sym () in
    let%map t = restore_names (sym :: ctx) t in
    U.Lam (sym, t)
  | App (f, x) ->
    let%bind f = restore_names ctx f in
    let%bind x = restore_names ctx x in
    return (U.App (f, x))
;;

let shift d t =
  let rec aux c t =
    match t with
    | Var i -> if i >= c then Var (i + d) else Var i
    | Abs t -> Abs (aux (c + 1) t)
    | App (f, x) -> App (aux c f, aux c x)
  in
  aux 0 t
;;

let subst j s t =
  let rec aux c t =
    match t with
    | Var i -> if i = j + c then shift c s else Var i
    | Abs t -> Abs (aux (c + 1) t)
    | App (f, x) -> App (aux c f, aux c x)
  in
  aux 0 t
;;

let rec eval t =
  match t with
  | Abs _ | Var _ -> t
  | App (Abs t, ((Var _ | Abs _) as x)) -> eval (shift (-1) (subst 0 (shift 1 x) t))
  | App (f, x) -> eval (App (eval f, eval x))
;;

module Syntax = struct
  let ( $ ) f x = App (f, x)
  let v i = Var i
  let h t = Abs t
end

(* Examples selected from [Exercise 6.1.1] *)
let%expect_test "remove and restore names" =
  let open Untyped_lambda_calculus.Syntax in
  let test original =
    Unique_id.For_testing.reset_counter ();
    let nameless = remove_names [] original in
    let renamed = Option.bind ~f:(restore_names []) nameless in
    [%message (original : U.t) (nameless : t option) (renamed : U.t option)]
    |> Sexp.to_string_hum
    |> print_endline
  in
  let s, z, f, x, y = v "s", v "z", v "f", v "x", v "y" in
  (* simple = λx. x *)
  test ("x" > x);
  [%expect
    {|
    ((original (Lam x (Var x))) (nameless ((Abs (Var 0))))
     (renamed ((Lam x.0 (Var x.0)))))
    |}];
  (* c_0 = λs. λz. z *)
  test ("s" > ("z" > z));
  [%expect
    {|
    ((original (Lam s (Lam z (Var z)))) (nameless ((Abs (Abs (Var 0)))))
     (renamed ((Lam x.0 (Lam x.1 (Var x.1))))))
    |}];
  (* c_2 = λs. λz. s (s z) *)
  test ("s" > ("z" > (s $ (s $ z))));
  [%expect
    {|
    ((original (Lam s (Lam z (App (Var s) (App (Var s) (Var z))))))
     (nameless ((Abs (Abs (App (Var 1) (App (Var 1) (Var 0)))))))
     (renamed ((Lam x.0 (Lam x.1 (App (Var x.0) (App (Var x.0) (Var x.1))))))))
    |}];
  (* plus = λm. λn. λs. λz. m s (n z s) *)
  test ("s" > ("z" > (s $ (s $ z))));
  [%expect
    {|
    ((original (Lam s (Lam z (App (Var s) (App (Var s) (Var z))))))
     (nameless ((Abs (Abs (App (Var 1) (App (Var 1) (Var 0)))))))
     (renamed ((Lam x.0 (Lam x.1 (App (Var x.0) (App (Var x.0) (Var x.1))))))))
    |}];
  (* fix = λf. (λx. f (λy. (x x) y)) (λx. f (λy. (x x) y)) *)
  test ("f" > ("x" > (f $ ("y" > (x $ x $ y))) $ ("x" > (f $ ("y" > (x $ x $ y))))));
  [%expect
    {|
    ((original
      (Lam f
       (App (Lam x (App (Var f) (Lam y (App (App (Var x) (Var x)) (Var y)))))
        (Lam x (App (Var f) (Lam y (App (App (Var x) (Var x)) (Var y))))))))
     (nameless
      ((Abs
        (App (Abs (App (Var 1) (Abs (App (App (Var 1) (Var 1)) (Var 0)))))
         (Abs (App (Var 1) (Abs (App (App (Var 1) (Var 1)) (Var 0)))))))))
     (renamed
      ((Lam x.0
        (App
         (Lam x.1
          (App (Var x.0) (Lam x.2 (App (App (Var x.1) (Var x.1)) (Var x.2)))))
         (Lam x.3
          (App (Var x.0) (Lam x.4 (App (App (Var x.3) (Var x.3)) (Var x.4))))))))))
    |}];
  (* foo = (λx. (λx. x)) (λx. x) *)
  test ("x" > ("x" > x) $ ("x" > x));
  [%expect
    {|
    ((original (App (Lam x (Lam x (Var x))) (Lam x (Var x))))
     (nameless ((App (Abs (Abs (Var 0))) (Abs (Var 0)))))
     (renamed ((App (Lam x.0 (Lam x.1 (Var x.1))) (Lam x.2 (Var x.2))))))
    |}]
;;

let%quick_test "quickcheck round trip restore and remove names" =
  fun (t : t) ->
  let open Option.Let_syntax in
  assert (
    let result =
      let%bind named = restore_names [] t in
      let%bind nameless = remove_names [] named in
      Some (equal nameless t)
    in
    (* TODO: Quickcheck generator sometimes generates invalid expressions,
       capping [Var i] to [0 <= i <= 5] results in ~7% of inputs being valid.
       Right now, we just ignore invalid inputs.

       Rewrite a proper Quickcheck generator that only emits valid forms. *)
    Option.value result ~default:true)
;;

open Untyped_lambda_calculus.Syntax

let print ctx t =
  t
  |> remove_names ctx
  |> Option.value_exn
  |> eval
  |> sexp_of_t
  |> Sexp.to_string_hum
  |> print_endline
;;

let%expect_test "eval simple" =
  print [ "x" ] @@ v "x";
  [%expect "(Var 0)"];
  print [ "x" ] @@ ("x" > v "x" $ v "x");
  [%expect "(Var 0)"]
;;

(* We test with [gamma = [ "f"; "t" ]], so ["f" = Var 0] and ["t" = Var 1] *)
let%expect_test "eval booleans" =
  let tru = "t" > ("f" > v "t") in
  let fls = "t" > ("f" > v "f") in
  let andb = "b" > ("c" > (v "b" $ v "c" $ fls)) in
  let test b =
    let test = "l" > ("m" > ("n" > (v "l" $ v "m" $ v "n"))) in
    print [ "f"; "t" ] (test $ b $ v "t" $ v "f")
  in
  test tru;
  test fls;
  [%expect
    {|
    (Var 1)
    (Var 0)
    |}];
  test (andb $ tru $ tru);
  test (andb $ tru $ fls);
  test (andb $ fls $ tru);
  test (andb $ fls $ fls);
  [%expect
    {|
    (Var 1)
    (Var 0)
    (Var 0)
    (Var 0)
    |}]
;;
