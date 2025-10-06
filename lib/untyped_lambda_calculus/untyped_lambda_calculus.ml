open Core
include Types

let rec subst (v : string) (t : t) (redex : t) =
  match redex with
  | Var v' when String.equal v v' -> t
  | Var v' -> Var v'
  | Lam (v', _) when String.equal v v' -> Lam (v', t)
  | Lam (v', t') -> Lam (v', subst v t t')
  | App (f, x) -> App (subst v t f, subst v t x)
;;

(* Call-by-name reduction *)
let rec beta = function
  | Var v -> Var v
  | Lam (v, t) -> Lam (v, beta t)
  | App (f, x) ->
    let f = beta f in
    let x = beta x in
    (match f with
     | Lam (v, t) -> beta (subst v x t)
     | _ -> App (f, x))
;;

let parse s = t_of_sexp (Sexp.of_string s)
let test t = t |> beta |> sexp_of_t |> Sexp.to_string_hum |> print_endline

let%expect_test "syntax" =
  test (parse "x");
  [%expect "x"]
;;

let%expect_test "boolean" =
  let tru = "(fun t -> fun f -> t)" in
  let fls = "(fun t -> fun f -> f)" in
  let andb = [%string "(fun b -> fun c -> b c %{fls})"] in
  let branch b =
    let test = "(fun l -> fun m -> fun n -> l m n)" in
    [%string "(%{test} %{b} t f)"]
  in
  let test_branch b = test (parse (branch b)) in
  test_branch tru;
  test_branch fls;
  [%expect
    {|
    t
    f
    |}];
  test_branch [%string "(%{andb} %{tru} %{tru})"];
  test_branch [%string "(%{andb} %{tru} %{fls})"];
  test_branch [%string "(%{andb} %{fls} %{tru})"];
  test_branch [%string "(%{andb} %{fls} %{fls})"];
  [%expect
    {|
    t
    f
    f
    f
    |}]
;;
