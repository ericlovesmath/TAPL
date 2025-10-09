open Core
open Sexplib.Sexp

type ty =
  | TyBase of (char[@generator Char.gen_uppercase])
  | TyUnit
  | TyBool
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyArrow of ty * ty
[@@deriving equal, quickcheck]

let sexp_of_ty ty =
  let rec parse =
    let sexp_of_fields fields =
      fields
      |> List.map ~f:(fun (l, ty) -> [ Atom l; Atom ":"; parse ty ])
      |> List.intersperse ~sep:[ Atom "," ]
      |> List.concat
    in
    function
    | TyBase c -> Atom (String.of_char c)
    | TyUnit -> Atom "unit"
    | TyBool -> Atom "bool"
    | TyTuple tys ->
      List
        ([ Atom "{" ]
         @ List.intersperse ~sep:(Atom ",") (List.map ~f:parse tys)
         @ [ Atom "}" ])
    | TyRecord record -> List ([ Atom "|" ] @ sexp_of_fields record @ [ Atom "|" ])
    | TyVariant vs -> List ([ Atom "<" ] @ sexp_of_fields vs @ [ Atom ">" ])
    | TyArrow (a, b) -> List [ parse a; Atom "->"; parse b ]
  in
  parse ty
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
    | List (Atom "|" :: rest as record) ->
      let rec parse_record_tail = function
        | [ Atom "|" ] -> []
        | [ Atom l; Atom ":"; ty; Atom "|" ] -> [ l, parse ty ]
        | Atom l :: Atom ":" :: ty :: Atom "," :: rest ->
          (l, parse ty) :: parse_record_tail rest
        | _ -> fail [%message "Unknown record form" (record : Sexp.t list)]
      in
      TyRecord (parse_record_tail rest)
    | List (Atom "<" :: rest as variant) ->
      let rec parse_variant_tail = function
        | [ Atom ">" ] -> []
        | [ Atom l; Atom ">" ] -> [ l, TyUnit ]
        | [ Atom l; Atom ":"; ty; Atom ">" ] -> [ l, parse ty ]
        | Atom l :: Atom "," :: rest -> (l, TyUnit) :: parse_variant_tail rest
        | Atom l :: Atom ":" :: ty :: Atom "," :: rest ->
          (l, parse ty) :: parse_variant_tail rest
        | _ -> fail [%message "Unknown variant form" (variant : Sexp.t list)]
      in
      TyVariant (parse_variant_tail rest)
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

let gen_label = String.gen_with_length 5 Char.gen_alpha

(* Church-style simply typed lambda calculus *)
type t =
  | EUnit
  | ETrue
  | EFalse
  | ETuple of t list
  | EProjTuple of t * int
  | ERecord of ((string[@generator gen_label]) * t) list
  | EProjRecord of t * (string[@generator gen_label])
  | EVariant of (string[@generator gen_label]) * ty * t
  | EMatch of t * (string * string * t) list
  | ESeq of t * t
  | EIf of t * t * t
  | ELet of (string[@generator gen_label]) * t * t
  | EVar of (string[@generator gen_label])
  | EAbs of string * ty * t
  | EApp of t * t
  | EAs of t * ty
[@@deriving equal, quickcheck]

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
    | ERecord record -> List ([ Atom "|" ] @ sexp_of_fields record @ [ Atom "|" ])
    | EProjRecord (t, l) -> List [ parse t; Atom "."; Atom l ]
    | EVariant (l, ty, t) ->
      List [ Atom "<"; Atom l; Atom ":"; parse t; Atom ">"; Atom "as"; sexp_of_ty ty ]
    | EMatch (t, cases) ->
      let sexp_of_case (l, v, t) = List [ Atom l; Atom v; Atom "=>"; parse t ] in
      List ([ Atom "case"; parse t; Atom "of" ] @ List.map cases ~f:sexp_of_case)
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
    | List (Atom "|" :: rest as tup) ->
      let rec parse_record_tail = function
        | [ Atom "|" ] -> []
        | [ Atom l; Atom ":"; ty; Atom "|" ] -> [ l, parse ty ]
        | Atom l :: Atom ":" :: ty :: Atom "," :: rest ->
          (l, parse ty) :: parse_record_tail rest
        | _ -> fail [%message "Unknown tuple form" (tup : Sexp.t list)]
      in
      ERecord (parse_record_tail rest)
    | List [ Atom "<"; Atom l; Atom ":"; t; Atom ">"; Atom "as"; ty ] ->
      EVariant (l, ty_of_sexp ty, parse t)
    | List [ Atom "<"; Atom l; Atom ">"; Atom "as"; ty ] ->
      EVariant (l, ty_of_sexp ty, EUnit)
    | List (Atom "case" :: t :: Atom "of" :: cases) ->
      let case_of_sexp = function
        | List (Atom l :: Atom v :: Atom "=>" :: t) -> l, v, parse (List t)
        | List (Atom l :: Atom "=>" :: t) -> l, "$ignore", parse (List t)
        | case -> fail [%message "unexpected case" (case : Sexp.t)]
      in
      EMatch (parse t, List.map ~f:case_of_sexp cases)
    | List [ t; Atom "."; Atom l ] ->
      (match Int.of_string_opt l with
       | Some i -> EProjTuple (parse t, i)
       | None -> EProjRecord (parse t, l))
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
