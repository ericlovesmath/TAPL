open Core

type ty =
  | TyBase of char
  | TyUnit
  | TyBool
  | TyNat
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyArrow of ty * ty
[@@deriving sexp_of, equal]

(* Church-style simply typed lambda calculus *)
type t =
  | EUnit
  | ETrue
  | EFalse
  | ETuple of t list
  | EProjTuple of t * int
  | ERecord of (string * t) list
  | EProjRecord of t * string
  | EVariant of string * ty * t
  | EMatch of t * (string * string * t) list
  | ESeq of t * t
  | EIf of t * t * t
  | ELet of string * t * t
  | EVar of string
  | EAbs of string * ty * t
  | EApp of t * t
  | EAs of t * ty
  | EZero
  | ESucc of t
  | EPred of t
  | EIsZero of t
  | EFix of t
[@@deriving sexp_of]

type nameless =
  | UUnit
  | UTrue
  | UFalse
  | UTuple of nameless list
  | UProjTuple of nameless * int
  | URecord of (string * nameless) list
  | UProjRecord of nameless * string
  | UVariant of string * nameless
  | UMatch of nameless * (string * nameless) list
  | USeq of nameless * nameless
  | UIf of nameless * nameless * nameless
  | ULet of string * nameless * nameless
  | UVar of int
  | UAbs of nameless
  | UApp of nameless * nameless
  | UZero
  | USucc of nameless
  | UPred of nameless
  | UIsZero of nameless
  | UFix of nameless
[@@deriving sexp_of]

type context = ty String.Map.t [@@deriving sexp_of]
