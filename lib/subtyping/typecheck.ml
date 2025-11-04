open Core
open Types

let assert_unique_fields fields =
  if Set.length (String.Set.of_list fields) = List.length fields
  then Ok ()
  else error_s [%message "duplicated labels in fields" (fields : string list)]
;;

let rec ( <: ) (ty : ty) (ty' : ty) =
  let is_subtype_fields r r' =
    List.for_all r' ~f:(fun (l, ty') ->
      match List.Assoc.find r l ~equal:String.equal with
      | Some ty -> ty <: ty'
      | None -> false)
  in
  if equal_ty ty ty'
  then true
  else (
    match ty, ty' with
    | _, TyTop -> true
    | TyBottom, _ -> true
    | TyBool, TyNat -> true
    | TyTuple ts, TyTuple ts' ->
      (match List.for_all2 ts ts' ~f:( <: ) with
       | Ok true -> true
       | Ok false | Unequal_lengths -> false)
    | TyRecord r, TyRecord r' -> is_subtype_fields r r'
    | TyVariant v, TyVariant v' ->
      (* NOTE: The switch of [v] and [v'] is intentional *)
      is_subtype_fields v' v
    | TyArrow (a, b), TyArrow (a', b') -> a <: a' && b <: b'
    | TyRef t, TyRef t' -> t <: t' && t' <: t
    | _ -> false)
;;

let rec join (ty : ty) (ty' : ty) =
  if ty <: ty'
  then ty'
  else if ty' <: ty
  then ty
  else (
    match ty, ty' with
    | TyBottom, _ -> ty'
    | _, TyBottom -> ty
    | TyRecord r, TyRecord r' ->
      let r'' =
        List.filter_map r' ~f:(fun (l, ty) ->
          let%map.Option ty' = List.Assoc.find r l ~equal:String.equal in
          l, join ty ty')
      in
      if List.is_empty r'' then TyTop else TyRecord r''
    | TyVariant v, TyVariant v' ->
      let rec join_fields r r' =
        match r' with
        | [] -> r
        | (l, ty) :: tl ->
          (match List.Assoc.find r l ~equal:String.equal with
           | None -> join_fields ((l, ty) :: r) tl
           | Some ty' ->
             let ty'' = List.Assoc.add r l (join ty ty') ~equal:String.equal in
             join_fields ty'' tl)
      in
      TyVariant (join_fields v v')
    | TyTuple ts, TyTuple ts' ->
      (match List.map2 ts ts' ~f:join with
       | Ok ts'' -> TyTuple ts''
       | Unequal_lengths -> TyTop)
    | TyArrow (a, b), TyArrow (a', b') -> TyArrow (join a a', join b b')
    | TyRef t, TyRef t' -> TyRef (join t t')
    | _ -> TyTop)
;;

let rec type_of (ctx : ty String.Map.t) (t : t) : ty Or_error.t =
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
  | EVariant (l, t) ->
    let%map ty = type_of ctx t in
    TyVariant [ l, ty ]
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
       (match%bind Or_error.all (List.map ~f:ty_of_case cases) with
        | [] -> error_s (Atom "case statement needs to have at least one branch")
        | hd :: tl -> Ok (List.fold_right tl ~f:join ~init:hd))
     | _ -> error_s [%message "expected match on variant" (ty : ty)])
  | ESeq (t, t') ->
    let%bind ty_t = type_of ctx t in
    if ty_t <: TyUnit
    then type_of ctx t'
    else error_s [%message "[ESeq (t, t')] expected t to be unit" (ty_t : ty)]
  | EIf (c, t, f) ->
    let%bind ty_c = type_of ctx c in
    if not (ty_c <: TyBool)
    then error_s [%message "[if] condition doesn't subsume to TyBool" (ty_c : ty)]
    else (
      let%bind ty_t = type_of ctx t in
      let%bind ty_f = type_of ctx f in
      Ok (join ty_t ty_f))
  | ELet (v, b, t) ->
    let%bind ty_b = type_of ctx b in
    let ctx = Map.set ctx ~key:v ~data:ty_b in
    type_of ctx t
  | EVar v ->
    (match Map.find ctx v with
     | Some ty -> Ok ty
     | None -> error_s [%message "var not in context" v (ctx : ty String.Map.t)])
  | EAbs (v, ty_v, t) ->
    let ctx = Map.set ctx ~key:v ~data:ty_v in
    let%map ty_t = type_of ctx t in
    TyArrow (ty_v, ty_t)
  | EApp (f, x) ->
    let%bind ty_f = type_of ctx f in
    (match ty_f with
     | TyArrow (ty_arg, ty_body) ->
       let%bind ty_x = type_of ctx x in
       if ty_x <: ty_arg
       then Ok ty_body
       else
         error_s
           [%message
             "arg's type does not subsume expected input type" (ty_f : ty) (ty_x : ty)]
     | _ -> error_s [%message "attempting to apply to non-arrow type" (ty_f : ty)])
  | EAs (t, ty_annotated) ->
    (* NOTE: Ascription does downcasting *)
    let%map _ = type_of ctx t in
    ty_annotated
  | EZero -> Ok TyNat
  | ESucc t ->
    let%bind ty_t = type_of ctx t in
    if ty_t <: TyNat
    then Ok TyNat
    else error_s [%message "expected succ to take term subsumed to nat" (ty_t : ty)]
  | EPred t ->
    let%bind ty_t = type_of ctx t in
    if ty_t <: TyNat
    then Ok TyNat
    else error_s [%message "expected pred to take term subsumed to nat" (ty_t : ty)]
  | EIsZero t ->
    let%bind ty_t = type_of ctx t in
    if ty_t <: TyNat
    then Ok TyBool
    else error_s [%message "expected iszero to take term subsumed to nat" (ty_t : ty)]
  | EFix t ->
    (match%bind type_of ctx t with
     | TyArrow (ty_l, ty_r) when ty_r <: ty_l -> Ok ty_r
     | ty_t -> error_s [%message "fix expects type a -> b where b <: a" (ty_t : ty)])
  | ERef t ->
    let%map ty = type_of ctx t in
    TyRef ty
  | EDeref t ->
    (match%bind type_of ctx t with
     | TyRef ty -> Ok ty
     | ty -> error_s [%message "deref expects ref" (ty : ty)])
  | EAssign (v, t) ->
    (match Map.find ctx v with
     | Some (TyRef ty_v) ->
       let%bind ty_t = type_of ctx t in
       if ty_t <: ty_v
       then Ok TyUnit
       else error_s [%message "assign to ref of unsubsumed type" (ty_v : ty) (ty_t : ty)]
     | Some ty -> error_s [%message "cannot assign to non-ref" (ty : ty)]
     | None -> error_s [%message "var not in context" v (ctx : ty String.Map.t)])
  | EError -> Ok TyBottom
;;

let typecheck = type_of String.Map.empty
