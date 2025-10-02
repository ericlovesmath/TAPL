open Core
open Sexplib.Sexp

type t =
  | Var of string
  | Lam of string * t
  | App of t * t
[@@deriving equal, quickcheck]

let rec sexp_of_t = function
  | Var v -> Atom v
  | Lam (v, t) -> List [ Atom "fun"; Atom v; Atom "->"; sexp_of_t t ]
  | App (f, x) -> List [ sexp_of_t f; sexp_of_t x ]
;;

let t_of_sexp sexp =
  let fail = Fn.compose failwith Sexp.to_string in
  let rec parse = function
    | Atom v -> Var v
    | List [ x ] -> parse x
    | List (Atom "fun" :: Atom v :: Atom "->" :: rest) -> Lam (v, parse (List rest))
    (* Left-associative application syntax *)
    | List (h :: t) -> List.fold_left ~f:(fun f x -> App (f, parse x)) ~init:(parse h) t
    | List xs -> fail [%message "Unknown list" (xs : Sexp.t list)]
  in
  parse sexp
;;

let%quick_test "quickcheck round trip sexp parser" =
  fun (t : t) ->
  (* TODO: [@quickcheck.generator String.gen_nonempty] isn't working? Fix *)
  let rec is_valid = function
    | Var "" | Lam ("", _) -> false
    | Var _ -> true
    | Lam (_, t) -> is_valid t
    | App (f, x) -> is_valid f && is_valid x
  in
  if is_valid t then assert (equal t (t_of_sexp (sexp_of_t t)))
;;

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
