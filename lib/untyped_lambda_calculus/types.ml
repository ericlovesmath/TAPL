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
  fun (t : t) -> assert (equal t (t_of_sexp (sexp_of_t t)))
;;
