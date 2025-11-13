open Core
open Sexplib.Sexp

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
[@@deriving equal]

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

let sexp_of_ty ty =
  let rec parse = function
    | TyTop -> Atom "top"
    | TyBottom -> Atom "bot"
    | TyBase c -> Atom (String.of_char c)
    | TyUnit -> Atom "unit"
    | TyBool -> Atom "bool"
    | TyNat -> Atom "nat"
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
    | TyVariant vs ->
      let fields =
        vs
        |> List.map ~f:(function
          | l, TyUnit -> [ Atom l ]
          | l, ty -> [ Atom l; Atom ":"; parse ty ])
        |> List.intersperse ~sep:[ Atom "," ]
        |> List.concat
      in
      List ([ Atom "<" ] @ fields @ [ Atom ">" ])
    | TyArrow (a, b) -> List [ parse a; Atom "->"; parse b ]
    | TyRef ty -> List [ parse ty; Atom "ref" ]
    | TyVar v -> Atom v
    | TyRec (v, ty) -> List [ Atom "rec"; Atom v; Atom "."; parse ty ]
  in
  parse ty
;;

let sexp_of_t t =
  let rec parse =
    let sexp_of_fields fields =
      fields
      |> List.map ~f:(fun (l, t) -> [ Atom l; Atom ":"; parse t ])
      |> List.intersperse ~sep:[ Atom "," ]
      |> List.concat
    in
    function
    | EUnit -> Atom "#u"
    | ETrue -> Atom "#t"
    | EFalse -> Atom "#f"
    | ETuple ts ->
      List
        ([ Atom "{" ]
         @ List.intersperse ~sep:(Atom ",") (List.map ~f:parse ts)
         @ [ Atom "}" ])
    | EProjTuple (t, i) -> List [ parse t; Atom "."; Atom (Int.to_string i) ]
    | ERecord record -> List ([ Atom "{" ] @ sexp_of_fields record @ [ Atom "}" ])
    | EProjRecord (t, l) -> List [ parse t; Atom "."; Atom l ]
    | EVariant (l, t) -> List [ Atom "<"; Atom l; Atom ":"; parse t; Atom ">" ]
    | EMatch (t, cases) ->
      let sexp_of_case (l, v, t) = List [ Atom l; Atom v; Atom "->"; parse t ] in
      List ([ Atom "match"; parse t; Atom "with" ] @ List.map cases ~f:sexp_of_case)
    | ESeq (t, t') -> List [ parse t; Atom ";"; parse t' ]
    | EIf (c, t, EUnit) -> List [ Atom "if"; parse c; Atom "then"; parse t ]
    | EIf (c, t, f) ->
      List [ Atom "if"; parse c; Atom "then"; parse t; Atom "else"; parse f ]
    | ELet (v, b, t) -> List [ Atom "let"; Atom v; Atom "="; parse b; Atom "in"; parse t ]
    | EVar v -> Atom v
    | EAbs (v, ty, t) ->
      List [ Atom "fun"; Atom v; Atom ":"; sexp_of_ty ty; Atom "->"; parse t ]
    | EApp (f, x) -> List [ parse f; parse x ]
    | EAs (t, ty) -> List [ parse t; Atom "as"; sexp_of_ty ty ]
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
