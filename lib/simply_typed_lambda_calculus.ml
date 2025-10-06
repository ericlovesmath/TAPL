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

let%quick_test "quickcheck round trip sexp parser" =
  fun (ty : ty) -> assert (equal_ty ty (ty_of_sexp (sexp_of_ty ty)))
;;

type context = ty String.Map.t [@@deriving sexp]

(* Church-style simply typed lambda calculus *)
type t =
  | ETrue
  | EFalse
  | EIf of t * t * t
  | EVar of string
  | EAbs of string * ty * t
  | EApp of t * t
[@@deriving sexp, equal, quickcheck]

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

let rec type_of (ctx : context) (t : t) : ty Or_error.t =
  let open Or_error.Let_syntax in
  match t with
  | ETrue | EFalse -> Ok TyBool
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
     | TyBool -> error_s [%message "attempting to apply to non-arrow type" (ty_f : ty)]
     | TyArrow (ty_arg, ty_body) ->
       let%bind ty_x = type_of ctx x in
       if equal_ty ty_arg ty_x
       then Ok ty_body
       else error_s [%message "arg can't be applied to func" (ty_f : ty) (ty_arg : ty)])
;;

let%expect_test "typechecker basic tests" =
  let test ?(ctx : context = String.Map.empty) (t : string) =
    t
    |> Sexp.of_string
    |> t_of_sexp
    |> type_of ctx
    |> Or_error.sexp_of_t sexp_of_ty
    |> Sexp.to_string_hum
    |> print_endline
  in
  test "#t";
  [%expect {| (Ok bool) |}];
  test "(if #t #f #f)";
  [%expect {| (Ok bool) |}];
  test "(if (fun x : bool -> x) #t #f)";
  [%expect {| (Error ("[if] condition is not TyBool" (ty_c (bool -> bool)))) |}];
  test "(if #t #t (fun x : bool -> x))";
  [%expect
    {|
    (Error
     ("[if] branches have unequal types" (ty_t bool) (ty_f (bool -> bool))))
    |}];
  test ~ctx:(String.Map.of_alist_exn [ "x", TyBool ]) "x";
  [%expect {| (Ok bool) |}];
  test "y";
  [%expect {| (Error ("var not in context" y (ctx ()))) |}];
  let id = "(fun x : bool -> x)" in
  test id;
  [%expect {| (Ok (bool -> bool)) |}];
  test [%string "(%{id} #t)"];
  [%expect {| (Ok bool) |}];
  test [%string "(%{id} %{id})"];
  [%expect
    {| (Error ("arg can't be applied to func" (ty_f (bool -> bool)) (ty_arg bool))) |}];
  test "(#t #f)";
  [%expect {| (Error ("attempting to apply to non-arrow type" (ty_f bool))) |}];
  test "(fun x : (bool -> bool) -> x)";
  [%expect {| (Ok ((bool -> bool) -> (bool -> bool))) |}]
;;
