open Core
open Sexplib.Sexp

type context = string list

type t =
  | Var of int [@quickcheck.generator Int.gen_incl 0 5]
  | Abs of t
  | App of t * t
[@@deriving sexp, equal, quickcheck]

let rec sexp_of_t = function
  | Var i -> Atom (Int.to_string i)
  | Abs t -> List [ Atom "abs"; sexp_of_t t ]
  | App (f, x) -> List [ sexp_of_t f; sexp_of_t x ]
;;

let t_of_sexp sexp =
  let fail = Fn.compose failwith Sexp.to_string in
  let rec parse = function
    | Atom atom ->
      (match Int.of_string_opt atom with
       | Some i -> Var i
       | None -> fail [%message "Unknown atom" atom])
    | List [ x ] -> parse x
    | List (Atom "abs" :: rest) -> Abs (parse (List rest))
    (* Left-associative application syntax *)
    | List (h :: t) -> List.fold_left ~f:(fun f x -> App (f, parse x)) ~init:(parse h) t
    | List xs -> fail [%message "Unknown list" (xs : Sexp.t list)]
  in
  parse sexp
;;

let%quick_test "quickcheck round trip sexp parser" =
  fun (t : t) -> assert (equal t (t_of_sexp (sexp_of_t t)))
;;
