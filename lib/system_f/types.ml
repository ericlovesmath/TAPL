open Core
open Sexplib.Sexp

type ty =
  | TyVar of string
  | TyUnit
  | TyBool
  | TyNat
  | TyArrow of ty * ty
  | TyForall of string * ty
  | TyExists of string * ty
[@@deriving equal]

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

let sexp_of_ty ty =
  let rec parse = function
    | TyUnit -> Atom "unit"
    | TyBool -> Atom "bool"
    | TyNat -> Atom "nat"
    | TyArrow (a, b) -> List [ parse a; Atom "->"; parse b ]
    | TyVar v -> Atom v
    | TyForall (v, ty) -> List [ Atom"forall"; Atom v; Atom "."; parse ty ]
    | TyExists (v, ty) -> List [ Atom "{"; Atom "exists"; Atom v; Atom ","; parse ty; Atom "}" ]
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
    | EAbs (v, ty, t) ->
      List [ Atom "fun"; Atom v; Atom ":"; sexp_of_ty ty; Atom "->"; parse t ]
    | EApp (f, x) -> List [ parse f; parse x ]
    | EZero -> Atom "Z"
    | ESucc t -> List [ Atom "S"; parse t ]
    | EPred t -> List [ Atom "pred"; parse t ]
    | EIsZero t -> List [ Atom "iszero"; parse t ]
    | ETyAbs (v, t) -> List [ Atom "fun"; Atom v; Atom "."; parse t ]
    | ETyApp (t, ty) -> List [ parse t; Atom "["; sexp_of_ty ty; Atom "]" ]
    | EPack (ty, t, ty') ->
      List
        [ Atom "{*"
        ; sexp_of_ty ty
        ; Atom ","
        ; parse t
        ; Atom "}"
        ; Atom "as"
        ; sexp_of_ty ty'
        ]
    | EUnpack (v_ty, v_t, t, b) ->
      List
        [ Atom "let"
        ; Atom "{"
        ; Atom v_ty
        ; Atom ","
        ; Atom v_t
        ; Atom "}"
        ; Atom "="
        ; parse t
        ; Atom "in"
        ; parse b
        ]
  in
  parse t
;;
