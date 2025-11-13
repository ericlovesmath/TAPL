type ty =
  | TyTop
  | TyBottom
  | TyBase of char
  | TyUnit
  | TyBool
  | TyNat
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyArrow of ty * ty
  | TyRef of ty
  | TyVar of string
  | TyRec of string * ty
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
  | EVariant of string * t
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
  | ERef of t
  | EDeref of t
  | EAssign of string * t
  | EError
[@@deriving sexp_of]
