open Core

type ty =
  | TyBool
  | TyArrow of ty * ty
[@@deriving sexp, equal]

(* Church-style simply typed lambda calculus *)
type t =
  | ETrue
  | EFalse
  | EIf of t * t * t
  | EVar of string
  | EAbs of string * ty * t
  | EApp of t * t
[@@deriving sexp]

type context = ty String.Map.t [@@deriving sexp]
