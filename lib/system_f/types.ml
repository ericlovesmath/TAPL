open Core
open Sexplib.Sexp

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
[@@deriving equal]

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
[@@deriving equal]

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

let sexp_of_ty ty =
  let rec parse = function
    | TyUnit -> Atom "unit"
    | TyBool -> Atom "bool"
    | TyNat -> Atom "nat"
    | TyArrow (a, b) -> List [ parse a; Atom "->"; parse b ]
    | TyVar v -> Atom v
    | TyForall (v, ty) -> List [ Atom "forall"; Atom v; Atom "."; parse ty ]
    | TyExists (v, ty) ->
      List [ Atom "{"; Atom "exists"; Atom v; Atom ","; parse ty; Atom "}" ]
    | TyTuple tys ->
      List
        ([ Atom "{" ]
         @ List.intersperse ~sep:(Atom ",") (List.map ~f:parse tys)
         @ [ Atom "}" ])
    | TyRecord record ->
      let fields =
        record
        |> List.map ~f:(fun (l, ty) -> [ Atom l; Atom ":"; parse ty ])
        |> List.intersperse ~sep:[ Atom "," ]
        |> List.concat
      in
      List ([ Atom "{" ] @ fields @ [ Atom "}" ])
    | TyRef ty -> List [ parse ty; Atom "ref" ]
  in
  parse ty
;;

let sexp_of_ty_nameless ty =
  let rec parse = function
    | UTyUnit -> Atom "unit"
    | UTyBool -> Atom "bool"
    | UTyNat -> Atom "nat"
    | UTyArrow (a, b) -> List [ parse a; Atom "->"; parse b ]
    | UTyVar i -> Atom (Int.to_string i)
    | UTyForall ty -> List [ Atom "forall"; Atom "."; parse ty ]
    | UTyExists ty -> List [ Atom "{"; Atom "exists"; parse ty; Atom "}" ]
    | UTyTuple tys ->
      List
        ([ Atom "{" ]
         @ List.intersperse ~sep:(Atom ",") (List.map ~f:parse tys)
         @ [ Atom "}" ])
    | UTyRecord record ->
      let fields =
        record
        |> List.map ~f:(fun (l, ty) -> [ Atom l; Atom ":"; parse ty ])
        |> List.intersperse ~sep:[ Atom "," ]
        |> List.concat
      in
      List ([ Atom "{" ] @ fields @ [ Atom "}" ])
    | UTyRef ty -> List [ parse ty; Atom "ref" ]
  in
  parse ty
;;

let sexp_of_t t =
  let rec parse = function
    | EUnit -> Atom "#u"
    | ETrue -> Atom "#t"
    | EFalse -> Atom "#f"
    | ETuple ts ->
      List
        ([ Atom "{" ]
         @ List.intersperse ~sep:(Atom ",") (List.map ~f:parse ts)
         @ [ Atom "}" ])
    | EProjTuple (t, i) -> List [ parse t; Atom "."; Atom (Int.to_string i) ]
    | ERecord record ->
      let sexp_of_fields fields =
        fields
        |> List.map ~f:(fun (l, t) -> [ Atom l; Atom ":"; parse t ])
        |> List.intersperse ~sep:[ Atom "," ]
        |> List.concat
      in
      List ([ Atom "{" ] @ sexp_of_fields record @ [ Atom "}" ])
    | EProjRecord (t, l) -> List [ parse t; Atom "."; Atom l ]
    | ESeq (t, t') -> List [ parse t; Atom ";"; parse t' ]
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
    | ERef t -> List [ Atom "ref"; parse t ]
    | EDeref t -> List [ Atom "!"; parse t ]
    | EAssign (v, t) -> List [ Atom v; Atom ":="; parse t ]
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
