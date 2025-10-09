open Core
include Types

let rec type_of (ctx : context) (t : t) : ty Or_error.t =
  let open Or_error.Let_syntax in
  match t with
  | EUnit -> Ok TyUnit
  | ETrue | EFalse -> Ok TyBool
  | ETuple ts ->
    let%map tys = Or_error.all (List.map ~f:(type_of ctx) ts) in
    TyTuple tys
  | ERecord record ->
    let type_of_field (l, t) =
      let%map ty = type_of ctx t in
      l, ty
    in
    let%map fields = Or_error.all (List.map ~f:type_of_field record) in
    TyRecord fields
  | EProjTuple (t, i) ->
    (match%bind type_of ctx t with
     | TyTuple tys ->
       (match List.nth tys i with
        | Some ty -> Ok ty
        | None ->
          error_s [%message "tuple projection on invalid index" (tys : ty list) (i : int)])
     | _ -> error_s [%message "expected tuple to project from" (t : t)])
  | EProjRecord (t, l) ->
    (match%bind type_of ctx t with
     | TyRecord tys ->
       (match List.Assoc.find tys l ~equal:String.equal with
        | Some ty -> Ok ty
        | None ->
          error_s
            [%message "record missing field" (tys : (string * ty) list) (l : string)])
     | _ -> error_s [%message "expected record to project from" (t : t)])
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
     | TyArrow (ty_arg, ty_body) ->
       let%bind ty_x = type_of ctx x in
       if equal_ty ty_arg ty_x
       then Ok ty_body
       else error_s [%message "arg can't be applied to func" (ty_f : ty) (ty_arg : ty)]
     | TyUnit | TyBase _ | TyBool | TyTuple _ | TyRecord _ ->
       error_s [%message "attempting to apply to non-arrow type" (ty_f : ty)])
  | EAs (t, ty_annotated) ->
    let%bind ty_t = type_of ctx t in
    if equal_ty ty_t ty_annotated
    then Ok ty_t
    else
      error_s
        [%message "annotated and derived type differ" (ty_t : ty) (ty_annotated : ty)]
;;

let test ?(ctx : context = String.Map.empty) (t : string) =
  t
  |> Sexp.of_string
  |> t_of_sexp
  |> type_of ctx
  |> Or_error.sexp_of_t sexp_of_ty
  |> Sexp.to_string_hum
  |> print_endline
;;

let%expect_test "typechecker tests prior to extending" =
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

let%expect_test "extended typechecker tests" =
  test "#u";
  [%expect {| (Ok unit) |}];
  test "(fun x : A -> x)";
  [%expect {| (Ok (A -> A)) |}];
  test "seq";
  test "(seq #t)";
  test "(seq #t #t)";
  test "(seq #u #t)";
  [%expect
    {|
    (Ok unit)
    (Ok bool)
    (Error ("[ESeq (t, t')] expected t to be unit" (ty_t bool)))
    (Ok bool)
    |}];
  test "(let x = #t in let f = (fun x : bool -> x) in f x)";
  test "(let x = #t in let y = x in y)";
  test "(let x = #t in let y = (#t #f) in x)";
  test "(let y = x in let x = #t in x)";
  [%expect
    {|
    (Ok bool)
    (Ok bool)
    (Error ("attempting to apply to non-arrow type" (ty_f bool)))
    (Error ("var not in context" x (ctx ())))
    |}];
  test "(#t as bool)";
  test "((fun x : bool -> x) as (bool -> bool))";
  test "((fun x : bool -> x) as bool)";
  [%expect
    {|
    (Ok bool)
    (Ok (bool -> bool))
    (Error
     ("annotated and derived type differ" (ty_t (bool -> bool))
      (ty_annotated bool)))
    |}];
  test "((fun x : A -> x) as (A -> A))";
  [%expect {| (Ok (A -> A)) |}];
  let tup = "({ #t , (fun x : bool -> x) , #f })" in
  test tup;
  [%expect {| (Ok ({ bool , (bool -> bool) , bool })) |}];
  test [%string "(%{tup} . 0)"];
  test [%string "(%{tup} . 1)"];
  test [%string "(%{tup} . 2)"];
  test [%string "(%{tup} . -1)"];
  test [%string "(%{tup} . 3)"];
  [%expect
    {|
    (Ok bool)
    (Ok (bool -> bool))
    (Ok bool)
    (Error
     ("tuple projection on invalid index" (tys (bool (bool -> bool) bool))
      (i -1)))
    (Error
     ("tuple projection on invalid index" (tys (bool (bool -> bool) bool)) (i 3)))
  |}];
  let record = "(| one : #t , two : (| nest : (fun x : bool -> x) |) |)" in
  test record;
  test [%string "(%{record} . one)"];
  test [%string "(%{record} . two)"];
  test [%string "((%{record} . two) . nest)"];
  test [%string "(((%{record} . two) . nest) #t)"];
  [%expect {|
    (Ok (| one : bool , two : (| nest : (bool -> bool) |) |))
    (Ok bool)
    (Ok (| nest : (bool -> bool) |))
    (Ok (bool -> bool))
    (Ok bool)
    |}];
  test [%string "(%{record} . three)"];
  [%expect {|
    (Error
     ("record missing field" (tys ((one bool) (two (| nest : (bool -> bool) |))))
      (l three)))
    |}];
;;
