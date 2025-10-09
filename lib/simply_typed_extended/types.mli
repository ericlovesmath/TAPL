open Core

type ty =
  | TyBase of char
  | TyUnit
  | TyBool
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyArrow of ty * ty
[@@deriving sexp, equal]

(* Church-style simply typed lambda calculus *)
type t =
  | EUnit
  | ETrue
  | EFalse
  | ETuple of t list
  | EProjTuple of t * int
  | ERecord of (string * t) list
  | EProjRecord of t * string
  | ESeq of t * t
  | EIf of t * t * t
  | ELet of string * t * t
  | EVar of string
  | EAbs of string * ty * t
  | EApp of t * t
  | EAs of t * ty
[@@deriving sexp]

type context = ty String.Map.t [@@deriving sexp]
