(** Simplest version of beta reduction on an untyped lambda calculus *)
type t =
  | Var of string
  | Lam of string * t
  | App of t * t
[@@deriving sexp]

(** Call-by-name beta reduction *)
val beta : t -> t
