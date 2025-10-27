open Core
open Sexplib.Sexp

type ty =
  | TyBase of char
  | TyUnit
  | TyBool
  | TyNat
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyArrow of ty * ty
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

let sexp_of_ty ty =
  let rec parse = function
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
    | EVariant (l, ty, t) ->
      List [ Atom "<"; Atom l; Atom ":"; parse t; Atom ">"; Atom "as"; sexp_of_ty ty ]
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
  in
  parse t
;;

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
  | UVar of int
  | UAbs of nameless
  | UApp of nameless * nameless
  | UZero
  | USucc of nameless
  | UPred of nameless
  | UIsZero of nameless
  | UFix of nameless

let sexp_of_nameless t =
  let rec parse = function
    | UUnit -> Atom "#u"
    | UTrue -> Atom "#t"
    | UFalse -> Atom "#f"
    | UTuple ts ->
      List
        ([ Atom "{" ]
         @ List.intersperse ~sep:(Atom ",") (List.map ~f:parse ts)
         @ [ Atom "}" ])
    | UProjTuple (t, i) -> List [ parse t; Atom "."; Atom (Int.to_string i) ]
    | URecord record ->
      let sexp_of_fields fields =
        fields
        |> List.map ~f:(fun (l, t) -> [ Atom l; Atom ":"; parse t ])
        |> List.intersperse ~sep:[ Atom "," ]
        |> List.concat
      in
      List ([ Atom "{" ] @ sexp_of_fields record @ [ Atom "}" ])
    | UProjRecord (t, l) -> List [ parse t; Atom "."; Atom l ]
    | UVariant (l, t) -> List [ Atom "<"; Atom l; Atom ":"; parse t; Atom ">" ]
    | UMatch (t, cases) ->
      let sexp_of_case (l, t) = List [ Atom l; Atom "->"; parse t ] in
      List ([ Atom "match"; parse t; Atom "with" ] @ List.map cases ~f:sexp_of_case)
    | USeq (t, t') -> List [ parse t; Atom ";"; parse t' ]
    | UIf (c, t, UUnit) -> List [ Atom "if"; parse c; Atom "then"; parse t ]
    | UIf (c, t, f) ->
      List [ Atom "if"; parse c; Atom "then"; parse t; Atom "else"; parse f ]
    | UVar v -> Atom (Int.to_string v)
    | UAbs t -> List [ Atom "abs"; Atom "."; parse t ]
    | UApp (f, x) -> List [ parse f; parse x ]
    | UZero -> Atom "Z"
    | USucc t -> List [ Atom "S"; parse t ]
    | UPred t -> List [ Atom "pred"; parse t ]
    | UIsZero t -> List [ Atom "iszero"; parse t ]
    | UFix t -> List [ Atom "fix"; parse t ]
  in
  parse t
;;

type context = ty String.Map.t [@@deriving sexp_of]
