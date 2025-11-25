type ty =
  | TyVar of string
  | TyUnit
  | TyBool
  | TyNat
  | TyArrow of ty * ty
  | TyRef of ty
[@@deriving equal, sexp_of]

type t =
  | EUnit
  | ETrue
  | EFalse
  | EIf of t * t * t
  | ELet of string * t * t
  | EVar of string
  | EAbs of string * t
  | EApp of t * t
  | EZero
  | ESucc of t
  | EPred of t
  | EIsZero of t
  | EFix of t
  | ERef of t
  | EDeref of t
  | EAssign of string * t
[@@deriving sexp_of]
