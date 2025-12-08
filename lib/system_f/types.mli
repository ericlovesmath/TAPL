type ty =
  | TyVar of string
  | TyUnit
  | TyBool
  | TyNat
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyArrow of ty * ty
  | TyRef of ty
  | TyForall of string * ty
  | TyExists of string * ty
[@@deriving sexp_of, equal]

type ty_nameless =
  | UTyVar of int
  | UTyUnit
  | UTyBool
  | UTyNat
  | UTyTuple of ty_nameless list
  | UTyRecord of (string * ty_nameless) list
  | UTyArrow of ty_nameless * ty_nameless
  | UTyRef of ty_nameless
  | UTyForall of ty_nameless
  | UTyExists of ty_nameless
[@@deriving sexp_of, equal]

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
  | EZero
  | ESucc of t
  | EPred of t
  | EIsZero of t
  | ERef of t
  | EDeref of t
  | EAssign of string * t
  | ETyAbs of string * t
  | ETyApp of t * ty
  | EPack of ty * t * ty
  | EUnpack of string * string * t * t
[@@deriving sexp_of]
