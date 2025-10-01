open Core

type t =
  | Var of int
  | Abs of t
  | App of t * t
[@@deriving sexp]

type lambda = Untyped_lambda_calculus.t
type context = string list

let find (ctx : context) (v : string) : int option =
  let rec find' acc = function
    | [] -> None
    | h :: _ when String.equal h v -> Some acc
    | _ :: t -> find' (acc + 1) t
  in
  find' 0 ctx
;;

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

(* TODO Quickcheck circle? *)

let%expect_test "remove names" =
  let open Untyped_lambda_calculus.Syntax in
  let test name t =
    t
    |> remove_names []
    |> Option.sexp_of_t sexp_of_t
    |> Sexp.to_string_hum
    |> Printf.printf "%s: %s\n" name
  in
  let s, z, f, x, y = v "s", v "z", v "f", v "x", v "y" in
  (* From [Exercise 6.1.1] *)
  test "simple" ("x" > x);
  test "c_0" ("s" > ("z" > z));
  test "c_3" ("s" > ("z" > (s $ (s $ z))));
  test "plus" ("s" > ("z" > (s $ (s $ z))));
  test "fix" ("f" > ("x" > (f $ ("y" > (x $ x $ y))) $ ("x" > (f $ ("y" > (x $ x $ y))))));
  [%expect
    {|
    simple: ((Abs (Var 0)))
    c_0: ((Abs (Abs (Var 0))))
    c_3: ((Abs (Abs (App (Var 1) (App (Var 1) (Var 0))))))
    plus: ((Abs (Abs (App (Var 1) (App (Var 1) (Var 0))))))
    fix: ((Abs
      (App (Abs (App (Var 1) (Abs (App (App (Var 1) (Var 1)) (Var 0)))))
       (Abs (App (Var 1) (Abs (App (App (Var 1) (Var 1)) (Var 0))))))))
    |}]
;;
