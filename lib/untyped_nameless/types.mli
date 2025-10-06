type context = string list

type t =
  | Var of int
  | Abs of t
  | App of t * t
[@@deriving sexp, equal, quickcheck]

