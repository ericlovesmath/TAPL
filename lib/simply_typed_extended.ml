open Core
open Sexplib.Sexp

type ty =
  | TyBase of char
  | TyUnit
  | TyBool
  | TyArrow of ty * ty
[@@deriving equal, quickcheck]

let rec sexp_of_ty = function
  | TyBase c -> Atom (String.of_char c)
  | TyUnit -> Atom "unit"
  | TyBool -> Atom "bool"
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
    | List (x :: Atom "->" :: rest) ->
      let left = parse x in
      let right = parse (List rest) in
      TyArrow (left, right)
    | List xs -> fail [%message "Unknown list" (xs : Sexp.t list)]
  in
  parse sexp
;;

let%quick_test "quickcheck round trip sexp parser" =
  fun (ty : ty) -> assert (equal_ty ty (ty_of_sexp (sexp_of_ty ty)))
;;

type context = ty String.Map.t [@@deriving sexp]

(* Church-style simply typed lambda calculus *)
type t =
  | EUnit
  | ETrue
  | EFalse
  | ESeq of t * t
  | EIf of t * t * t
  | ELet of string * t * t
  | EVar of string
  | EAbs of string * ty * t
  | EApp of t * t
  | EAs of t * ty
[@@deriving sexp]

let rec type_of (ctx : context) (t : t) : ty Or_error.t =
  let open Or_error.Let_syntax in
  match t with
  | EUnit -> Ok TyUnit
  | ETrue | EFalse -> Ok TyBool
  | ESeq (t, t') ->
    let%bind ty_t = type_of ctx t in
    if equal_ty ty_t TyUnit
    then type_of ctx t'
    else error_s [%message "[ESeq (t, t')] expected t to be unit" (ty_t : ty)]
  | EIf (c, t, f) ->
    let%bind ty_c = type_of ctx c in
    if not (equal_ty ty_c TyBool)
    then error_s [%message "[if] condition is not TyBool" (ty_c : ty)]
    else (
      let%bind ty_t = type_of ctx t in
      let%bind ty_f = type_of ctx f in
      if not (equal_ty ty_t ty_f)
      then error_s [%message "[if] branches have unequal types" (ty_t : ty) (ty_f : ty)]
      else Ok ty_t)
  | ELet (v, b, t) ->
      let%bind ty_b = type_of ctx b in
      let ctx = Map.set ctx ~key:v ~data:ty_b in
      type_of ctx t
  | EVar v ->
    (match Map.find ctx v with
     | Some ty -> Ok ty
     | None -> error_s [%message "var not in context" v (ctx : context)])
  | EAbs (v, ty_v, t) ->
    let ctx = Map.set ctx ~key:v ~data:ty_v in
    let%map ty_t = type_of ctx t in
    TyArrow (ty_v, ty_t)
  | EApp (f, x) ->
    let%bind ty_f = type_of ctx f in
    (match ty_f with
     | TyUnit | TyBase _ | TyBool ->
       error_s [%message "attempting to apply to non-arrow type" (ty_f : ty)]
     | TyArrow (ty_arg, ty_body) ->
       let%bind ty_x = type_of ctx x in
       if equal_ty ty_arg ty_x
       then Ok ty_body
       else error_s [%message "arg can't be applied to func" (ty_f : ty) (ty_arg : ty)])
  | EAs (t, ty_annotated) ->
    let%bind ty_t = type_of ctx t in
    if equal_ty ty_t ty_annotated
    then Ok ty_t
    else
      error_s
        [%message "annotated and derived type differ" (ty_t : ty) (ty_annotated : ty)]
;;

let%expect_test "typechecker basic tests" =
  let test ?(ctx : context = String.Map.empty) (t : t) =
    t
    |> type_of ctx
    |> Or_error.sexp_of_t sexp_of_ty
    |> Sexp.to_string_hum
    |> print_endline
  in
  test ETrue;
  [%expect {| (Ok bool) |}];
  test (EIf (ETrue, EFalse, ETrue));
  [%expect {| (Ok bool) |}];
  test (EIf (EAbs ("x", TyBool, EVar "x"), ETrue, EFalse));
  [%expect {| (Error ("[if] condition is not TyBool" (ty_c (bool -> bool)))) |}];
  test (EIf (ETrue, ETrue, EAbs ("x", TyBool, EVar "x")));
  [%expect
    {|
    (Error
     ("[if] branches have unequal types" (ty_t bool) (ty_f (bool -> bool))))
    |}];
  test ~ctx:(String.Map.of_alist_exn [ "x", TyBool ]) (EVar "x");
  [%expect {| (Ok bool) |}];
  test (EVar "y");
  [%expect {| (Error ("var not in context" y (ctx ()))) |}];
  let id = EAbs ("x", TyBool, EVar "x") in
  test id;
  [%expect {| (Ok (bool -> bool)) |}];
  test (EApp (id, ETrue));
  [%expect {| (Ok bool) |}];
  test (EApp (id, id));
  [%expect
    {| (Error ("arg can't be applied to func" (ty_f (bool -> bool)) (ty_arg bool))) |}];
  test (EApp (ETrue, EFalse));
  [%expect {| (Error ("attempting to apply to non-arrow type" (ty_f bool))) |}];
  test (EAbs ("x", TyArrow (TyBool, TyBool), EVar "x"));
  [%expect {| (Ok ((bool -> bool) -> (bool -> bool))) |}]
;;
