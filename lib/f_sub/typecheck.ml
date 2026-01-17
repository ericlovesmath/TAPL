open Core
open Types

type ty_context = (string * ty_nameless) list [@@deriving sexp_of]

let find (ctx : ty_context) (v : string) : int Or_error.t =
  let rec find' acc = function
    | [] -> error_s [%message "failed to find" v (ctx : ty_context)]
    | (h, _) :: _ when String.equal h v -> Ok acc
    | _ :: t -> find' (acc + 1) t
  in
  find' 0 ctx
;;

let rec remove_names (ctx : ty_context) (ty : ty) : ty_nameless Or_error.t =
  let open Or_error.Let_syntax in
  match ty with
  | TyTop -> Ok UTyTop
  | TyUnit -> Ok UTyUnit
  | TyBool -> Ok UTyBool
  | TyNat -> Ok UTyNat
  | TyVar v ->
    let%map v = find ctx v in
    UTyVar v
  | TyTuple ts ->
    let%map ts = Or_error.all (List.map ~f:(remove_names ctx) ts) in
    UTyTuple ts
  | TyRecord r ->
    let%map r =
      Or_error.all
        (List.map r ~f:(fun (l, t) -> Or_error.both (Ok l) (remove_names ctx t)))
    in
    UTyRecord r
  | TyArrow (l, r) ->
    let%bind l = remove_names ctx l in
    let%bind r = remove_names ctx r in
    return (UTyArrow (l, r))
  | TyRef ty ->
    let%map ty = remove_names ctx ty in
    UTyRef ty
  | TyForall (v, ty_sub, ty) ->
    let%bind ty_sub = remove_names ctx ty_sub in
    let%bind ty = remove_names ((v, ty_sub) :: ctx) ty in
    return (UTyForall (ty_sub, ty))
  | TyExists (v, ty_sub, ty) ->
    let%bind ty_sub = remove_names ctx ty_sub in
    let%bind ty = remove_names ((v, ty_sub) :: ctx) ty in
    return (UTyExists (ty_sub, ty))
;;

let map_ty f ty =
  let rec walk c = function
    | (UTyTop | UTyUnit | UTyBool | UTyNat) as ty -> ty
    | UTyVar i -> f c i
    | UTyTuple ts -> UTyTuple (List.map ts ~f:(walk c))
    | UTyRecord r -> UTyRecord (List.map r ~f:(Tuple2.map_snd ~f:(walk c)))
    | UTyArrow (l, r) -> UTyArrow (walk c l, walk c r)
    | UTyRef ty -> UTyRef (walk c ty)
    | UTyForall (s, t) -> UTyForall (walk c s, walk (c + 1) t)
    | UTyExists (s, t) -> UTyExists (walk c s, walk (c + 1) t)
  in
  walk 0 ty
;;

let shift (d : int) = map_ty (fun c i -> UTyVar (if i >= c then i + d else i))

let subst (j : int) (s : ty_nameless) =
  map_ty (fun c i -> if i = j + c then shift c s else UTyVar i)
;;

let subst_top (b : ty_nameless) (t : ty_nameless) = shift (-1) (subst 0 (shift 1 b) t)

let is_free (ty : ty_nameless) : bool =
  let rec aux depth = function
    | UTyTop | UTyUnit | UTyBool | UTyNat -> false
    | UTyVar k -> Int.equal k depth
    | UTyTuple ts -> List.exists ts ~f:(aux depth)
    | UTyRecord r -> List.exists r ~f:(fun (_, t) -> aux depth t)
    | UTyArrow (l, r) -> aux depth l || aux depth r
    | UTyRef t -> aux depth t
    | UTyForall (t, t') | UTyExists (t, t') -> aux (depth + 1) t || aux (depth + 1) t'
  in
  aux 0 ty
;;

let assert_unique_fields fields =
  if Set.length (String.Set.of_list fields) = List.length fields
  then Ok ()
  else error_s [%message "duplicated labels in fields" (fields : string list)]
;;

let rec expose (ctx : ty_context) (ty : ty_nameless) : ty_nameless =
  match ty with
  | UTyVar i ->
    (match List.nth ctx i with
     | Some (_, bound) -> expose ctx (shift (i + 1) bound)
     | None -> ty)
  | _ -> ty
;;

let rec subtype (ctx : ty_context) (ty : ty_nameless) (ty' : ty_nameless) =
  match ty, ty' with
  | _ when equal_ty_nameless ty ty' -> true
  | _, UTyTop -> true
  | UTyVar i, _ ->
    let _, bound = List.nth_exn ctx i in
    subtype ctx (shift (i + 1) bound) ty'
  | UTyTuple ts, UTyTuple ts' ->
    (match List.for_all2 ts ts' ~f:(subtype ctx) with
     | Ok result -> result
     | Unequal_lengths -> false)
  | UTyRecord r, UTyRecord r' ->
    List.for_all r' ~f:(fun (l, ty_field') ->
      match List.Assoc.find r l ~equal:String.equal with
      | Some ty_field -> subtype ctx ty_field ty_field'
      | None -> false)
  | UTyArrow (a, b), UTyArrow (a', b') -> subtype ctx a' a && subtype ctx b b'
  | UTyRef t, UTyRef t' -> subtype ctx t t' && subtype ctx t' t
  | UTyForall (s1, t1), UTyForall (s2, t2) | UTyExists (s1, t1), UTyExists (s2, t2) ->
    equal_ty_nameless s1 s2 && subtype (("", s1) :: ctx) t1 t2
  | _ -> false
;;

let rec join (ctx : ty_context) (ty : ty_nameless) (ty' : ty_nameless) =
  match ty, ty' with
  | _ when subtype ctx ty ty' -> ty'
  | _ when subtype ctx ty' ty -> ty
  (* NOTE: We don't call [expose] as we want to unwrap one layer at a time up *)
  | UTyVar i, _ ->
    let _, bound = List.nth_exn ctx i in
    join ctx (shift (i + 1) bound) ty'
  | _, UTyVar i ->
    let _, bound = List.nth_exn ctx i in
    join ctx ty (shift (i + 1) bound)
  | UTyRecord r, UTyRecord r' ->
    let r'' =
      List.filter_map r' ~f:(fun (l, ty) ->
        let%map.Option ty' = List.Assoc.find r l ~equal:String.equal in
        l, join ctx ty ty')
    in
    if List.is_empty r'' then UTyTop else UTyRecord r''
  | UTyTuple ts, UTyTuple ts' ->
    (match List.map2 ts ts' ~f:(join ctx) with
     | Ok ts'' -> UTyTuple ts''
     | Unequal_lengths -> UTyTop)
  | UTyArrow (a, b), UTyArrow (a', b') ->
    (* NOTE: Contravariant on input, only join if inputs are compatible *)
    if equal_ty_nameless a a' then UTyArrow (a, join ctx b b') else UTyTop
  | UTyRef t, UTyRef t' -> if equal_ty_nameless t t' then UTyRef t else UTyTop
  | UTyForall (s1, t1), UTyForall (s2, t2) ->
    if equal_ty_nameless s1 s2 then UTyForall (s1, join ctx t1 t2) else UTyTop
  | UTyExists (s1, t1), UTyExists (s2, t2) ->
    if equal_ty_nameless s1 s2 then UTyExists (s1, join ctx t1 t2) else UTyTop
  | _ -> UTyTop
;;

let rec type_of (ctx : ty_nameless String.Map.t) (ty_ctx : ty_context) (t : t)
  : ty_nameless Or_error.t
  =
  let open Or_error.Let_syntax in
  let ( <: ) = subtype ty_ctx in
  let join = join ty_ctx in
  let expose = expose ty_ctx in
  match t with
  | EUnit -> Ok UTyUnit
  | ETrue | EFalse -> Ok UTyBool
  | ETuple ts ->
    let%map tys = Or_error.all (List.map ~f:(type_of ctx ty_ctx) ts) in
    UTyTuple tys
  | ERecord record ->
    let type_of_field (l, t) =
      let%map ty = type_of ctx ty_ctx t in
      l, ty
    in
    let%bind fields = Or_error.all (List.map ~f:type_of_field record) in
    let%bind () = assert_unique_fields (List.map ~f:fst fields) in
    Ok (UTyRecord fields)
  | EProjTuple (t, i) ->
    let%bind ty = type_of ctx ty_ctx t in
    (match expose ty with
     | UTyTuple tys ->
       (match List.nth tys i with
        | Some ty -> Ok ty
        | None ->
          error_s
            [%message
              "tuple projection on invalid index" (tys : ty_nameless list) (i : int)])
     | _ -> error_s [%message "expected tuple to project from" (t : t)])
  | EProjRecord (t, l) ->
    let%bind ty = type_of ctx ty_ctx t in
    (match expose ty with
     | UTyRecord tys ->
       let%bind () = assert_unique_fields (List.map ~f:fst tys) in
       (match List.Assoc.find tys l ~equal:String.equal with
        | Some ty -> Ok ty
        | None ->
          error_s [%message "record missing field" (tys : ty_context) (l : string)])
     | ty_t ->
       error_s [%message "expected record to project from" (t : t) (ty_t : ty_nameless)])
  | ESeq (t, t') ->
    let%bind ty_t = type_of ctx ty_ctx t in
    if ty_t <: UTyUnit
    then type_of ctx ty_ctx t'
    else error_s [%message "[ESeq (t, t')] expected t to be unit" (ty_t : ty_nameless)]
  | EIf (c, t, f) ->
    let%bind ty_c = type_of ctx ty_ctx c in
    if not (ty_c <: UTyBool)
    then error_s [%message "[if] cond doesn't subsume to TyBool" (ty_c : ty_nameless)]
    else (
      let%bind ty_t = type_of ctx ty_ctx t in
      let%bind ty_f = type_of ctx ty_ctx f in
      Ok (join ty_t ty_f))
  | ELet (v, b, t) ->
    let%bind ty_b = type_of ctx ty_ctx b in
    let ctx = Map.set ctx ~key:v ~data:ty_b in
    type_of ctx ty_ctx t
  | EVar v ->
    (match Map.find ctx v with
     | Some ty -> Ok ty
     | None -> error_s [%message "var not in context" v (ctx : ty_nameless String.Map.t)])
  | EAbs (v, ty_v, t) ->
    let%bind ty_v = remove_names ty_ctx ty_v in
    let ctx = Map.set ctx ~key:v ~data:ty_v in
    let%bind ty_t = type_of ctx ty_ctx t in
    return (UTyArrow (ty_v, ty_t))
  | EApp (f, x) ->
    let%bind ty_f = type_of ctx ty_ctx f in
    (match expose ty_f with
     | UTyArrow (ty_arg, ty_body) ->
       let%bind ty_x = type_of ctx ty_ctx x in
       if ty_x <: ty_arg
       then Ok ty_body
       else
         error_s
           [%message
             "arg can't be applied to func" (ty_f : ty_nameless) (ty_x : ty_nameless)]
     | _ -> error_s [%message "can't to apply to non-arrow type" (ty_f : ty_nameless)])
  | EZero -> Ok UTyNat
  | ESucc t ->
    let%bind ty_t = type_of ctx ty_ctx t in
    if ty_t <: UTyNat
    then Ok UTyNat
    else error_s [%message "expected succ to take nat" (ty_t : ty_nameless)]
  | EPred t ->
    let%bind ty_t = type_of ctx ty_ctx t in
    if ty_t <: UTyNat
    then Ok UTyNat
    else error_s [%message "expected pred to take nat" (ty_t : ty_nameless)]
  | EIsZero t ->
    let%bind ty_t = type_of ctx ty_ctx t in
    if ty_t <: UTyNat
    then Ok UTyNat
    else error_s [%message "expected iszero to take nat" (ty_t : ty_nameless)]
  | ERef t ->
    let%map ty = type_of ctx ty_ctx t in
    UTyRef ty
  | EDeref t ->
    (match%bind type_of ctx ty_ctx t with
     | UTyRef ty -> Ok ty
     | ty -> error_s [%message "deref expects ref" (ty : ty_nameless)])
  | EAssign (v, t) ->
    (match Map.find ctx v with
     | Some (UTyRef ty_v) ->
       let%bind ty_t = type_of ctx ty_ctx t in
       if ty_t <: ty_v
       then Ok UTyUnit
       else
         error_s
           [%message
             "assigning to ref of wrong type" (ty_v : ty_nameless) (ty_t : ty_nameless)]
     | Some ty -> error_s [%message "cannot assign to non-ref" (ty : ty_nameless)]
     | None -> error_s [%message "var not in context" v (ctx : ty_nameless String.Map.t)])
  | ETyAbs (ty_var, ty_sub, t) ->
    let%bind ty_sub = remove_names ty_ctx ty_sub in
    let ctx' = Map.map ctx ~f:(shift 1) in
    let ty_ctx' = (ty_var, ty_sub) :: ty_ctx in
    let%bind ty_t = type_of ctx' ty_ctx' t in
    return (UTyForall (ty_sub, ty_t))
  | ETyApp (t, ty_arg) ->
    let%bind ty_t = type_of ctx ty_ctx t in
    let%bind ty_arg = remove_names ty_ctx ty_arg in
    (match expose ty_t with
     | UTyForall (ty_sub, ty_body) ->
       if subtype ty_ctx ty_arg ty_sub
       then Ok (subst_top ty_arg ty_body)
       else
         error_s
           [%message
             "type argument does not satisfy bound"
               (ty_arg : ty_nameless)
               (ty_sub : ty_nameless)]
     | _ -> error_s [%message "expected universal type" (ty_t : ty_nameless)])
  | EPack (ty_real, t, ty_package) ->
    let%bind ty_real = remove_names ty_ctx ty_real in
    let%bind ty_package = remove_names ty_ctx ty_package in
    (match expose ty_package with
     | UTyExists (ty_bound, ty_body) ->
       if not (subtype ty_ctx ty_real ty_bound)
       then
         error_s
           [%message
             "witness type does not satisfy bound"
               (ty_real : ty_nameless)
               (ty_bound : ty_nameless)]
       else (
         let%bind ty_t = type_of ctx ty_ctx t in
         let expected_ty = subst_top ty_real ty_body in
         if subtype ty_ctx ty_t expected_ty
         then Ok ty_package
         else
           error_s
             [%message
               "pack term does not match declared existential type"
                 (ty_t : ty_nameless)
                 (expected_ty : ty_nameless)])
     | _ ->
       error_s
         [%message
           "pack annotation must be an existential type" (ty_package : ty_nameless)])
  | EUnpack (ty_v, t_v, t_package, t_body) ->
    let%bind ty_pkg = type_of ctx ty_ctx t_package in
    (match expose ty_pkg with
     | UTyExists (ty_bound, ty_body) ->
       let ctx_shifted = Map.map ctx ~f:(shift 1) in
       let ctx = Map.set ctx_shifted ~key:t_v ~data:ty_body in
       let ty_ctx = (ty_v, ty_bound) :: ty_ctx in
       let%bind ty_res = type_of ctx ty_ctx t_body in
       if is_free ty_res
       then
         error_s
           [%message "existential type variable escapes scope" (ty_res : ty_nameless)]
       else Ok (shift (-1) ty_res)
     | _ -> error_s [%message "unpack expects existential type" (ty_pkg : ty_nameless)])
;;

let rename_tyvars (ty : ty_nameless) : ty =
  let counter = ref 0 in
  let gensym () =
    let i = !counter in
    incr counter;
    let char_code = Char.to_int 'A' + (i mod 26) in
    let num_suffix = i / 26 in
    let base_name = String.of_char (Char.of_int_exn char_code) in
    if num_suffix = 0 then base_name else base_name ^ Int.to_string num_suffix
  in
  let rec aux (env : string list) (t : ty_nameless) : ty =
    match t with
    | UTyTop -> TyTop
    | UTyUnit -> TyUnit
    | UTyBool -> TyBool
    | UTyNat -> TyNat
    | UTyVar i -> TyVar (List.nth_exn env i)
    | UTyTuple ts -> TyTuple (List.map ts ~f:(aux env))
    | UTyRecord fs -> TyRecord (List.map fs ~f:(fun (l, ty) -> l, aux env ty))
    | UTyArrow (l, r) -> TyArrow (aux env l, aux env r)
    | UTyRef ty -> TyRef (aux env ty)
    | UTyForall (ty_sub, ty) ->
      let v = gensym () in
      TyForall (v, aux env ty_sub, aux (v :: env) ty)
    | UTyExists (ty_sub, ty) ->
      let v = gensym () in
      TyExists (v, aux env ty_sub, aux (v :: env) ty)
  in
  aux [] ty
;;

let typecheck t = Or_error.map ~f:rename_tyvars (type_of String.Map.empty [] t)
