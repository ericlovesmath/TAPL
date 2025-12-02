type ty =
  | TyVar of string
  | TyUnit
  | TyBool
  | TyNat
  | TyArrow of ty * ty
  | TyForall of string * ty
  | TyExists of string * ty
[@@deriving sexp_of, equal]

type t =
  | EUnit
  | ETrue
  | EFalse
  | EIf of t * t * t
  | ELet of string * t * t
  | EVar of string
  | EAbs of string * ty * t
  | EApp of t * t
  | EZero
  | ESucc of t
  | EPred of t
  | EIsZero of t
  | ETyAbs of string * t
  | ETyApp of t * ty
  | EPack of ty * t * ty
  | EUnpack of string * string * t * t
[@@deriving sexp_of]
