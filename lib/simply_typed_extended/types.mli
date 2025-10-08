open Core

type ty =
  | TyBase of char
  | TyUnit
  | TyBool
  | TyTuple of ty list
  | TyArrow of ty * ty
[@@deriving sexp, equal]

(* Church-style simply typed lambda calculus *)
type t =
  | EUnit
  | ETrue
  | EFalse
  | ETuple of t list
  | EProj of t * int
  | ESeq of t * t
  | EIf of t * t * t
  | ELet of string * t * t
  | EVar of string
  | EAbs of string * ty * t
  | EApp of t * t
  | EAs of t * ty
[@@deriving sexp]

type context = ty String.Map.t [@@deriving sexp]
