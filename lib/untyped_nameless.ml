open Core

type t =
  | Var of int
  | Abs of t
  | App of t * t
[@@deriving sexp]

type lambda = Untyped_lambda_calculus.t [@@deriving sexp]
type context = string list

let find (ctx : context) (v : string) : int option =
  let rec find' acc = function
    | [] -> None
    | h :: _ when String.equal h v -> Some acc
    | _ :: t -> find' (acc + 1) t
  in
  find' 0 ctx
;;

(** [Exercise 6.1.5] Converts to DeBruijn indices *)
let rec remove_names (ctx : context) (t : lambda) : t option =
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

(** [Exercise 6.1.5] Converts from DeBruijn indices *)
let rec restore_names (ctx : context) (t : t) : lambda option =
  let open Option.Let_syntax in
  match t with
  | Var i ->
    let%map v = List.nth ctx i in
    Untyped_lambda_calculus.Var v
  | Abs t ->
    let sym = Utils.gen_sym () in
    let%map t = restore_names (sym :: ctx) t in
    Untyped_lambda_calculus.Lam (sym, t)
  | App (f, x) ->
    let%bind f = restore_names ctx f in
    let%bind x = restore_names ctx x in
    return (Untyped_lambda_calculus.App (f, x))
;;

(* TODO Quickcheck circle? *)

(* Examples selected from [Exercise 6.1.1] *)
let%expect_test "remove and restore names" =
  let open Untyped_lambda_calculus.Syntax in
  let test original =
    Utils.counter := 0;
    let nameless = remove_names [] original in
    let renamed = Option.bind ~f:(restore_names []) nameless in
    [%message (original : lambda) (nameless : t option) (renamed : lambda option)]
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
