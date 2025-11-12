open Core
open Types

let assert_unique_fields fields =
  if Set.length (String.Set.of_list fields) = List.length fields
  then Ok ()
  else error_s [%message "duplicated labels in fields" (fields : string list)]
;;

let subst (v : string) (ty : ty) (term : ty) : ty =
  let rec aux term =
    match term with
    | TyUnit | TyBool | TyNat | TyBase _ -> term
    | TyVar v' -> if String.equal v v' then ty else term
    | TyRec (v', ty) -> if String.equal v v' then term else TyRec (v', aux ty)
    | TyArrow (ty, ty') -> TyArrow (aux ty, aux ty')
    | TyRef ty -> TyRef (aux ty)
    | TyTuple tys -> TyTuple (List.map ~f:aux tys)
    | TyVariant vs -> TyVariant (List.map vs ~f:(Tuple2.map_snd ~f:aux))
    | TyRecord vs -> TyRecord (List.map vs ~f:(Tuple2.map_snd ~f:aux))
  in
  aux term
;;

let unfold (ty : ty) : ty =
  match ty with
  | TyRec (v, ty') -> subst v ty ty'
  | _ -> ty
;;

let equal_ty (ty : ty) (ty' : ty) : bool =
  let for_all2 xs xs' ~f =
    match List.for_all2 xs xs' ~f with
    | Ok true -> true
    | Ok false | Unequal_lengths -> false
  in
  let rec aux (seen : (ty * ty) list) (ty : ty) (ty' : ty) : bool =
    if List.exists seen ~f:(fun (ty'', ty''') -> equal_ty ty ty'' && equal_ty ty' ty''')
    then true
    else (
      let aux = aux ((ty', ty) :: (ty, ty') :: seen) in
      (* TODO: I wonder if there's a way to use GADTs nicely to make this exhaustive *)
      match unfold ty, unfold ty' with
      | TyUnit, TyUnit | TyBool, TyBool | TyNat, TyNat -> true
      | TyVar v, TyVar v' -> String.equal v v'
      | TyBase c, TyBase c' -> Char.equal c c'
      | TyArrow (l, r), TyArrow (l', r') -> aux l l' && aux r r'
      | TyTuple tys, TyTuple tys' -> for_all2 tys tys' ~f:aux
      | TyVariant vs, TyVariant vs' | TyRecord vs, TyRecord vs' ->
        for_all2 vs vs' ~f:(fun (l, ty) (l', ty') -> String.equal l l' && aux ty ty')
      | TyRef ty, TyRef ty' -> aux ty ty'
      | _ -> false)
  in
  aux [] ty ty'
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
    let%bind ty = type_of ctx t in
    (match unfold ty with
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
    (match unfold ty with
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
    (match unfold ty with
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
     | None -> error_s [%message "var not in context" v (ctx : ty String.Map.t)])
  | EAbs (v, ty_v, t) ->
    let ctx = Map.set ctx ~key:v ~data:ty_v in
    let%map ty_t = type_of ctx t in
    TyArrow (ty_v, ty_t)
  | EApp (f, x) ->
    let%bind ty_f = type_of ctx f in
    (match unfold ty_f with
     | TyArrow (ty_arg, ty_body) ->
       let%bind ty_x = type_of ctx x in
       if equal_ty ty_arg ty_x
       then Ok ty_body
       else error_s [%message "arg can't be applied to func" (ty_f : ty) (ty_x : ty)]
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
    let%bind ty = type_of ctx t in
    (match unfold ty with
     | TyArrow (ty_l, ty_r) when equal_ty ty_l ty_r -> Ok ty_l
     | ty_t -> error_s [%message "fix expects function of 'a -> 'a" (ty_t : ty)])
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
       if equal_ty ty_v ty_t
       then Ok TyUnit
       else error_s [%message "assigning to ref of wrong type" (ty_v : ty) (ty_t : ty)]
     | Some ty -> error_s [%message "cannot assign to non-ref" (ty : ty)]
     | None -> error_s [%message "var not in context" v (ctx : ty String.Map.t)])
;;

let typecheck = type_of String.Map.empty
