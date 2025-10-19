open Core
include Types

let assert_unique_fields fields =
  if Set.length (String.Set.of_list fields) = List.length fields
  then Ok ()
  else error_s [%message "duplicated labels in fields" (fields : string list)]
;;

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
    let%bind fields = Or_error.all (List.map ~f:type_of_field record) in
    let%bind () = assert_unique_fields (List.map ~f:fst fields) in
    Ok (TyRecord fields)
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
       let%bind () = assert_unique_fields (List.map ~f:fst tys) in
       (match List.Assoc.find tys l ~equal:String.equal with
        | Some ty -> Ok ty
        | None ->
          error_s
            [%message "record missing field" (tys : (string * ty) list) (l : string)])
     | _ -> error_s [%message "expected record to project from" (t : t)])
  | EVariant (l, ty, t) ->
    (match ty with
     | TyVariant tys ->
       let%bind () = assert_unique_fields (List.map ~f:fst tys) in
       (match List.Assoc.find tys l ~equal:String.equal with
        | Some ty_anno ->
          let%bind ty_infer = type_of ctx t in
          if equal_ty ty_infer ty_anno
          then Ok ty
          else error_s [%message "incorrect variant type" (ty_infer : ty) (ty_anno : ty)]
        | None -> error_s [%message "field missing in variant" (ty : ty) (l : string)])
     | _ -> error_s [%message "expected annotation to be a variant" (ty : ty)])
  | EMatch (t, cases) ->
    let%bind ty = type_of ctx t in
    (match ty with
     | TyVariant tys ->
       let case_labels = List.map ~f:(fun (l, _, _) -> l) cases in
       let%bind () = assert_unique_fields case_labels in
       let%bind () =
         let variant_labels = List.map ~f:fst tys in
         if String.Set.(equal (of_list case_labels) (of_list variant_labels))
         then Ok ()
         else
           error_s
             [%message
               "unexpected cases for variant"
                 (case_labels : string list)
                 (variant_labels : string list)]
       in
       let ty_of_case (l, v, t) =
         match List.Assoc.find tys l ~equal:String.equal with
         | Some ty ->
           let ctx = Map.set ctx ~key:v ~data:ty in
           type_of ctx t
         | None -> error_s [%message "field missing in variant" (ty : ty) (l : string)]
       in
       let%bind ty_cases = Or_error.all (List.map ~f:ty_of_case cases) in
       (match ty_cases with
        | [] -> error_s (Atom "case statement needs to have at least one branch")
        | hd :: tl ->
          if List.for_all tl ~f:(equal_ty hd)
          then Ok hd
          else error_s [%message "unequal types across branches" (ty_cases : ty list)])
     | _ -> error_s [%message "expected match on variant" (ty : ty)])
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
     | _ -> error_s [%message "attempting to apply to non-arrow type" (ty_f : ty)])
  | EAs (t, ty_annotated) ->
    let%bind ty_t = type_of ctx t in
    if equal_ty ty_t ty_annotated
    then Ok ty_t
    else
      error_s
        [%message "annotated and derived type differ" (ty_t : ty) (ty_annotated : ty)]
  | EZero -> Ok TyNat
  | ESucc t ->
    (match%bind type_of ctx t with
     | TyNat -> Ok TyNat
     | ty_t -> error_s [%message "expected succ to take nat" (ty_t : ty)])
  | EPred t ->
    (match%bind type_of ctx t with
     | TyNat -> Ok TyNat
     | ty_t -> error_s [%message "expected pred to take nat" (ty_t : ty)])
  | EIsZero t ->
    (match%bind type_of ctx t with
     | TyNat -> Ok TyBool
     | ty_t -> error_s [%message "expected iszero to take nat" (ty_t : ty)])
  | EFix t ->
    (match%bind type_of ctx t with
     | TyArrow (ty_l, ty_r) when equal_ty ty_l ty_r -> Ok ty_l
     | ty_t -> error_s [%message "fix expects function of 'a -> 'a" (ty_t : ty)])
;;

let rec eval (ctx : t String.Map.t) (t : t) : t =
  let fail () = raise_s [%message "eval failed" (ctx : t String.Map.t) (t : t)] in
  match t with
  | EUnit | ETrue | EFalse | EZero | EVariant _ | EAbs _ -> t
  | ETuple t -> ETuple (List.map ~f:(eval ctx) t)
  | ERecord t -> ERecord (List.map ~f:(fun (l, t) -> l, eval ctx t) t)
  | EIf (c, t, f) ->
    (match eval ctx c with
     | ETrue -> eval ctx t
     | EFalse -> eval ctx f
     | _ -> fail ())
  | EProjTuple (t, i) ->
    (match eval ctx t with
     | ETuple tup ->
       (match List.nth tup i with
        | Some t -> t
        | None -> fail ())
     | _ -> fail ())
  | EProjRecord (t, l) ->
    (match eval ctx t with
     | ERecord tup ->
       (match List.Assoc.find tup l ~equal:String.equal with
        | Some t -> t
        | None -> fail ())
     | _ -> fail ())
  | ESeq (t, t') ->
    ignore (eval ctx t);
    eval ctx t'
  | ESucc t -> ESucc (eval ctx t)
  | EPred t ->
    (match eval ctx t with
     | EZero -> EZero
     | ESucc t -> t
     | _ -> fail ())
  | EIsZero t ->
    (match eval ctx t with
     | EZero -> ETrue
     | ESucc _ -> EFalse
     | _ -> fail ())
  | EAs (t, _) -> eval ctx t
  | EMatch (t, cases) ->
    (match eval ctx t with
     | EVariant (l, _, variant) ->
       (match List.find cases ~f:(fun (l', _, _) -> String.equal l' l) with
        | Some (_, v, body) ->
          let ctx = Map.set ctx ~key:v ~data:variant in
          eval ctx body
        | None -> fail ())
     | _ -> fail ())
  | ELet (v, b, t) ->
    let b = eval ctx b in
    eval (Map.set ctx ~key:v ~data:b) t
  | EVar v ->
    (match Map.find ctx v with
     | Some t -> t
     | None -> fail ())
  | EApp (f, x) ->
    let x = eval ctx x in
    let f = eval ctx f in
    (match f with
     | EAbs (v, _, t) ->
       let ctx = Map.set ctx ~key:v ~data:x in
       eval ctx t
     | _ -> fail ())
  | EFix f ->
    let f = eval ctx f in
    (match f with
     | EAbs (param, _, body) ->
       let rec_fix = EFix f in
       eval (Map.set ctx ~key:param ~data:rec_fix) body
     | _ -> fail ())
;;

let test (s : string) =
  let t = 
    s |> Lexer.of_string |> Lexer.lex |> Parser.run Parser.t_p
  in
  match t with
  | Error parse_err -> print_s [%message (parse_err : Error.t)]
  | Ok t ->
    (match type_of String.Map.empty t with
     | Error ty_error -> print_s [%message (ty_error : Error.t)]
     | Ok ty ->
       let result = eval String.Map.empty t in
       print_s [%message (ty : ty) (result : t)])
;;

let%expect_test "typechecker tests prior to extending" =
  test "#t";
  [%expect {| ((ty bool) (result #t)) |}];
  test "if #t then #f else #f";
  [%expect {| ((ty bool) (result #f)) |}];
  test "if (fun x : bool -> x) then #t ekse #f";
  [%expect {| (ty_error ("[if] condition is not TyBool" (ty_c (bool -> bool)))) |}];
  test "if #t then #t else (fun x : bool -> x)";
  [%expect
    {|
    (ty_error
     ("[if] branches have unequal types" (ty_t bool) (ty_f (bool -> bool))))
    |}];
  test "y";
  [%expect {| (ty_error ("var not in context" y (ctx ()))) |}];
  let id = "(fun x : bool -> x)" in
  test id;
  [%expect {| ((ty (bool -> bool)) (result (fun x : bool -> x))) |}];
  test [%string "(%{id} #t)"];
  [%expect {| ((ty bool) (result #t)) |}];
  test [%string "(%{id} %{id})"];
  [%expect
    {|
    (ty_error
     ("arg can't be applied to func" (ty_f (bool -> bool)) (ty_arg bool)))
    |}];
  test "(#t #f)";
  [%expect {| (ty_error ("attempting to apply to non-arrow type" (ty_f bool))) |}];
  test "(fun x : (bool -> bool) -> x)";
  [%expect
    {|
    ((ty ((bool -> bool) -> (bool -> bool)))
     (result (fun x : (bool -> bool) -> x)))
    |}]
;;

let%expect_test "extended typechecker tests" =
  test "#u";
  [%expect {| ((ty unit) (result #u)) |}];
  test "fun x : A -> x";
  [%expect {| ((ty (A -> A)) (result (fun x : A -> x))) |}];
  test "#t; #t";
  test "#u; #u; #f";
  [%expect
    {|
    (ty_error ("[ESeq (t, t')] expected t to be unit" (ty_t bool)))
    ((ty bool) (result #f))
    |}];
  test "(let x = #t in let f = (fun x : bool -> x) in f x)";
  test "(let x = #t in let y = x in y)";
  test "(let x = #t in let y = (#t #f) in x)";
  test "(let y = x in let x = #t in x)";
  [%expect
    {|
    ((ty bool) (result #t))
    ((ty bool) (result #t))
    (ty_error ("attempting to apply to non-arrow type" (ty_f bool)))
    (ty_error ("var not in context" x (ctx ())))
     |}];
  test "#t as bool";
  test "(fun x : bool -> x) as (bool -> bool)";
  test "(fun x : bool -> x) as bool";
  [%expect
    {|
    ((ty bool) (result #t))
    ((ty (bool -> bool)) (result (fun x : bool -> x)))
    (ty_error
     ("annotated and derived type differ" (ty_t (bool -> bool))
      (ty_annotated bool)))
   |}];
  test "(fun x : A -> x) as (A -> A)";
  [%expect {| ((ty (A -> A)) (result (fun x : A -> x))) |}];
  let tup = "{ #t, (fun x : bool -> x), #f }" in
  test tup;
  [%expect
    {|
    ((ty ({ bool , (bool -> bool) , bool }))
     (result ({ #t , (fun x : bool -> x) , #f })))
    |}];
  test [%string "%{tup}.0"];
  test [%string "%{tup}.1"];
  test [%string "%{tup}.2"];
  test [%string "%{tup}.3"];
  [%expect
    {|
    ((ty bool) (result #t))
    ((ty (bool -> bool)) (result (fun x : bool -> x)))
    ((ty bool) (result #f))
    (ty_error
     ("tuple projection on invalid index" (tys (bool (bool -> bool) bool)) (i 3)))
     |}];
  let record = "{ one = #t, two = { nest = (fun x : bool -> x) }}" in
  test record;
  test [%string "%{record}.one"];
  test [%string "%{record}.two"];
  test [%string "(%{record}.two).nest"];
  [%expect
    {|
    ((ty (| one : bool , two : (| nest : (bool -> bool) |) |))
     (result (| one : #t , two : (| nest : (fun x : bool -> x) |) |)))
    ((ty bool) (result #t))
    ((ty (| nest : (bool -> bool) |)) (result (| nest : (fun x : bool -> x) |)))
    ((ty (bool -> bool)) (result (fun x : bool -> x)))
     |}];
  test [%string "%{record}.three"];
  [%expect
    {|
    (ty_error
     ("record missing field" (tys ((one bool) (two (| nest : (bool -> bool) |))))
      (l three)))
    |}];
  let option = "< some : bool , none >" in
  test [%string "< none > as %{option}"];
  test [%string "< some #t > as %{option}"];
  test [%string "< some #t > as %{option}"];
  [%expect
    {|
    ((ty (< some : bool , none >))
     (result (< none : #u > as (< some : bool , none >))))
    ((ty (< some : bool , none >))
     (result (< some : #t > as (< some : bool , none >))))
    ((ty (< some : bool , none >))
     (result (< some : #t > as (< some : bool , none >))))
    |}];
  test [%string "< some #u > as %{option}"];
  test [%string "< yes #t > as %{option}"];
  test [%string "< some #t > as < some : bool , some : bool >"];
  [%expect
    {|
    (ty_error ("incorrect variant type" (ty_infer unit) (ty_anno bool)))
    (ty_error ("field missing in variant" (ty (< some : bool , none >)) (l yes)))
    (ty_error ("duplicated labels in fields" (fields (some some))))
    |}];
  let some_true = [%string "< some #t > as %{option}"] in
  test [%string "match %{some_true} with | some x -> x | none -> #t"];
  [%expect {| ((ty bool) (result #t)) |}];
  test [%string "match %{some_true} with | some x -> x | none -> #u"];
  [%expect {| (ty_error ("unequal types across branches" (ty_cases (bool unit)))) |}];
  test [%string "match %{some_true} with | some x -> #t"];
  test [%string "match %{some_true} with | some x -> #t | some x -> #t | none -> #t"];
  [%expect
    {|
    (ty_error
     ("unexpected cases for variant" (case_labels (some))
      (variant_labels (some none))))
    (ty_error ("duplicated labels in fields" (fields (some some none))))
     |}];
  test "Z";
  test "S Z";
  test "S (pred (S Z))";
  test "iszero (pred (S Z))";
  test "if iszero Z then #t else #f";
  test "S #t";
  [%expect
    {|
    ((ty nat) (result 0))
    ((ty nat) (result (succ 0)))
    ((ty nat) (result (succ 0)))
    ((ty bool) (result #t))
    ((ty bool) (result #t))
    (ty_error ("expected succ to take nat" (ty_t bool)))
     |}];
  test "fix (fun x : bool -> x)";
  [%expect {| ((ty bool) (result (fix (fun x : bool -> x)))) |}];
  test
    {|
     letrec f : (nat -> nat) =
       fun x : nat ->
         if iszero x
           then Z
           else f (pred x)
     in f (S (S Z))
     |};
  [%expect {| ((ty nat) (result 0)) |}]
;;

let%expect_test "cool examples" =
  let weekday = "(< mon , tue , wed , thu , fri >)" in
  test
    [%string
      {|
      let next =
        fun w : %{weekday} ->
          match w with
          | mon -> < tue > as %{weekday}
          | tue -> < wed > as %{weekday}
          | wed -> < thu > as %{weekday}
          | thu -> < fri > as %{weekday}
          | fri -> < mon > as %{weekday}
       in
       next (< thu > as %{weekday})
      |}];
  [%expect
    {|
    ((ty (< mon , tue , wed , thu , fri >))
     (result (< fri : #u > as (< mon , tue , wed , thu , fri >))))
    |}]
;;

let%expect_test "addition (no closures)" =
  let pair = "({ x : nat , y : nat })" in
  test
    [%string
      {|
      letrec add : (%{pair} -> nat) =
        (fun xy : %{pair} ->
          (if (iszero (xy.x))
            then (xy.y)
            else (add ({ x = (pred (xy.x)) , y = (S (xy.y)) }))))
      in add { x = S (S Z), y = S (S Z) }
      |}];
  [%expect {| ((ty nat) (result (succ (succ (succ (succ 0)))))) |}]
;;
