open Core

type t =
  | Var of string
  | Lam of string * t
  | App of t * t
[@@deriving sexp]

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

module Syntax = struct
  let ( $ ) f x = App (f, x)
  let v x = Var x
  let ( > ) v t = Lam (v, t)
end

include Syntax

let print t = t |> beta |> sexp_of_t |> Sexp.to_string_hum |> print_endline

let%expect_test "syntax" =
  print @@ v "x";
  [%expect "(Var x)"]
;;

let%expect_test "boolean" =
  let tru = "t" > ("f" > v "t") in
  let fls = "t" > ("f" > v "f") in
  let andb = "b" > ("c" > (v "b" $ v "c" $ fls)) in
  let test b =
    let test = "l" > ("m" > ("n" > (v "l" $ v "m" $ v "n"))) in
    test $ b $ v "t" $ v "f"
  in
  print (test tru);
  print (test fls);
  [%expect
    {|
    (Var t)
    (Var f)
  |}];
  print (test (andb $ tru $ tru));
  print (test (andb $ tru $ fls));
  print (test (andb $ fls $ tru));
  print (test (andb $ fls $ fls));
  [%expect
    {|
    (Var t)
    (Var f)
    (Var f)
    (Var f)
    |}]
;;
