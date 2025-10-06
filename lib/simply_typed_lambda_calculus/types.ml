open Core
open Sexplib.Sexp

type ty =
  | TyBool
  | TyArrow of ty * ty
[@@deriving equal, quickcheck]

let rec sexp_of_ty = function
  | TyBool -> Atom "bool"
  | TyArrow (a, b) -> List [ sexp_of_ty a; Atom "->"; sexp_of_ty b ]
;;

let ty_of_sexp sexp =
  let fail = Fn.compose failwith Sexp.to_string in
  let rec parse = function
    | Atom "bool" -> TyBool
    | Atom a -> fail [%message "Unknown atom" a]
    | List [ x ] -> parse x
    | List (x :: Atom "->" :: rest) ->
      let left = parse x in
      let right = parse (List rest) in
      TyArrow (left, right)
    | List xs -> fail [%message "Unknown list" (xs : Sexp.t list)]
  in
  parse sexp
;;

let%quick_test "quickcheck round trip sexp parser ty" =
  fun (ty : ty) -> assert (equal_ty ty (ty_of_sexp (sexp_of_ty ty)))
;;

(* Church-style simply typed lambda calculus *)
type t =
  | ETrue
  | EFalse
  | EIf of t * t * t
  | EVar of string
  | EAbs of string * ty * t
  | EApp of t * t
[@@deriving equal, quickcheck]

let rec sexp_of_t = function
  | ETrue -> Atom "#t"
  | EFalse -> Atom "#f"
  | EIf (c, t, f) -> List [ Atom "if"; sexp_of_t c; sexp_of_t t; sexp_of_t f ]
  | EVar v -> Atom v
  | EAbs (v, ty, t) ->
    List [ Atom "fun"; Atom v; Atom ":"; sexp_of_ty ty; Atom "->"; sexp_of_t t ]
  | EApp (f, x) -> List [ sexp_of_t f; sexp_of_t x ]
;;

let t_of_sexp sexp =
  let fail = Fn.compose failwith Sexp.to_string in
  let rec parse = function
    | Atom "#t" -> ETrue
    | Atom "#f" -> EFalse
    | Atom v -> EVar v
    | List [ x ] -> parse x
    | List [ Atom "if"; c; t; f ] -> EIf (parse c, parse t, parse f)
    | List (Atom "fun" :: Atom v :: Atom ":" :: ty :: Atom "->" :: body) ->
      EAbs (v, ty_of_sexp ty, parse (List body))
    | List (h :: t) -> List.fold_left ~f:(fun f x -> EApp (f, parse x)) ~init:(parse h) t
    | List xs -> fail [%message "Unknown list" (xs : Sexp.t list)]
  in
  parse sexp
;;

let%quick_test "quickcheck round trip sexp parser t" =
  fun (t : t) -> assert (equal t (t_of_sexp (sexp_of_t t)))
;;

type context = ty String.Map.t [@@deriving sexp]
