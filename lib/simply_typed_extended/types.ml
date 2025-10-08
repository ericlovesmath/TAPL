open Core
open Sexplib.Sexp

type ty =
  | TyBase of (char[@quickcheck.generator Char.gen_uppercase])
  | TyUnit
  | TyBool
  | TyTuple of ty list
  | TyArrow of ty * ty
[@@deriving equal, quickcheck]

let rec sexp_of_ty = function
  | TyBase c -> Atom (String.of_char c)
  | TyUnit -> Atom "unit"
  | TyBool -> Atom "bool"
  | TyTuple tys ->
    List
      ([ Atom "{" ]
       @ List.intersperse ~sep:(Atom ",") (List.map ~f:sexp_of_ty tys)
       @ [ Atom "}" ])
  | TyArrow (a, b) -> List [ sexp_of_ty a; Atom "->"; sexp_of_ty b ]
;;

let ty_of_sexp sexp =
  let fail = Fn.compose failwith Sexp.to_string in
  let rec parse = function
    | Atom "unit" -> TyUnit
    | Atom "bool" -> TyBool
    | Atom c when String.length c = 1 -> TyBase (Char.of_string c)
    | Atom a -> fail [%message "Unknown atom" a]
    | List [ x ] -> parse x
    | List (Atom "{" :: rest as tup) ->
      let rec parse_tuple_tail = function
        | [ Atom "}" ] -> []
        | [ ty; Atom "}" ] -> [ parse ty ]
        | ty :: Atom "," :: rest -> parse ty :: parse_tuple_tail rest
        | _ -> fail [%message "Unknown tuple form" (tup : Sexp.t list)]
      in
      TyTuple (parse_tuple_tail rest)
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
  | EUnit
  | ETrue
  | EFalse
  | ETuple of t list
  | EProj of t * int
  | ESeq of t * t
  | EIf of t * t * t
  | ELet of string * t * t
  | EVar of (string[@quickcheck.generator String.gen_with_length 5 Char.gen_alpha])
  | EAbs of string * ty * t
  | EApp of t * t
  | EAs of t * ty
[@@deriving equal, quickcheck]

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
    | EProj (t, i) -> List [ parse t; Atom "."; Atom (Int.to_string i) ]
    | ESeq (t, t') -> List [ Atom "seq"; parse t; parse t' ]
    | EIf (c, t, f) -> List [ Atom "if"; parse c; parse t; parse f ]
    | ELet (v, b, t) -> List [ Atom "let"; Atom v; Atom "="; parse b; Atom "in"; parse t ]
    | EVar v -> Atom v
    | EAbs (v, ty, t) ->
      List [ Atom "fun"; Atom v; Atom ":"; sexp_of_ty ty; Atom "->"; parse t ]
    | EApp (f, x) -> List [ parse f; parse x ]
    | EAs (t, ty) -> List [ parse t; Atom "as"; sexp_of_ty ty ]
  in
  parse t
;;

let t_of_sexp sexp =
  let fail = Fn.compose failwith Sexp.to_string in
  let rec parse = function
    | Atom "#u" | Atom "seq" -> EUnit
    | Atom "#t" -> ETrue
    | Atom "#f" -> EFalse
    | Atom v -> EVar v
    | List [ x ] -> parse x
    | List (Atom "{" :: rest as tup) ->
      let rec parse_tuple_tail = function
        | [ Atom "}" ] -> []
        | [ ty; Atom "}" ] -> [ parse ty ]
        | ty :: Atom "," :: rest -> parse ty :: parse_tuple_tail rest
        | _ -> fail [%message "Unknown tuple form" (tup : Sexp.t list)]
      in
      ETuple (parse_tuple_tail rest)
    | List [ t; Atom "."; Atom i ] ->
      (match Int.of_string_opt i with
       | None -> fail [%message "expected tuple projected by const int" i]
       | Some i -> EProj (parse t, i))
    | List [ Atom "seq"; t ] -> parse t
    | List (Atom "seq" :: hd :: tl) -> ESeq (parse hd, parse (List (Atom "seq" :: tl)))
    | List [ Atom "if"; c; t; f ] -> EIf (parse c, parse t, parse f)
    | List (Atom "let" :: Atom v :: Atom "=" :: b :: Atom "in" :: t) ->
      ELet (v, parse b, parse (List t))
    | List (Atom "fun" :: Atom v :: Atom ":" :: ty :: Atom "->" :: body) ->
      EAbs (v, ty_of_sexp ty, parse (List body))
    | List [ t; Atom "as"; ty ] -> EAs (parse t, ty_of_sexp ty)
    | List (h :: t) -> List.fold_left ~f:(fun f x -> EApp (f, parse x)) ~init:(parse h) t
    | List xs -> fail [%message "Unknown list" (xs : Sexp.t list)]
  in
  parse sexp
;;

let%quick_test "quickcheck round trip sexp parser t" =
  fun (t : t) -> assert (equal t (t_of_sexp (sexp_of_t t)))
;;

type context = ty String.Map.t [@@deriving sexp]
