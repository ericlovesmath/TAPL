open Core
open Sexplib.Sexp

type ty =
  | TyVar of string
  | TyUnit
  | TyBool
  | TyNat
  | TyArrow of ty * ty
  | TyRef of ty
[@@deriving equal]

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

let sexp_of_ty ty =
  let rec parse = function
    | TyVar v -> Atom ("'" ^ v)
    | TyUnit -> Atom "unit"
    | TyBool -> Atom "bool"
    | TyNat -> Atom "nat"
    | TyArrow (a, b) -> List [ parse a; Atom "->"; parse b ]
    | TyRef ty -> List [ parse ty; Atom "ref" ]
  in
  parse ty
;;

let sexp_of_t t =
  let rec parse = function
    | EUnit -> Atom "#u"
    | ETrue -> Atom "#t"
    | EFalse -> Atom "#f"
    | EIf (c, t, EUnit) -> List [ Atom "if"; parse c; Atom "then"; parse t ]
    | EIf (c, t, f) ->
      List [ Atom "if"; parse c; Atom "then"; parse t; Atom "else"; parse f ]
    | ELet (v, b, t) -> List [ Atom "let"; Atom v; Atom "="; parse b; Atom "in"; parse t ]
    | EVar v -> Atom v
    | EAbs (v, t) -> List [ Atom "fun"; Atom v; Atom "->"; parse t ]
    | EApp (f, x) -> List [ parse f; parse x ]
    | EZero -> Atom "Z"
    | ESucc t -> List [ Atom "S"; parse t ]
    | EPred t -> List [ Atom "pred"; parse t ]
    | EIsZero t -> List [ Atom "iszero"; parse t ]
    | EFix t -> List [ Atom "fix"; parse t ]
    | ERef t -> List [ Atom "ref"; parse t ]
    | EDeref t -> List [ Atom "!"; parse t ]
    | EAssign (v, t) -> List [ Atom v; Atom ":="; parse t ]
  in
  parse t
;;
