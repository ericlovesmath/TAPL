open Core
open Types

let find (ctx : string list) (v : string) : int Or_error.t =
  let rec find' acc = function
    | [] -> error_s [%message "failed to find variable" v (ctx : string list)]
    | h :: _ when String.equal h v -> Ok acc
    | _ :: t -> find' (acc + 1) t
  in
  find' 0 ctx
;;

let rec remove_names (ctx : string list) (ty : ty) : ty_nameless Or_error.t =
  let open Or_error.Let_syntax in
  match ty with
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
  | TyForall (v, ty) ->
    let%map ty = remove_names (v :: ctx) ty in
    UTyForall ty
  | TyExists (v, ty) ->
    let%map ty = remove_names (v :: ctx) ty in
    UTyExists ty
;;

let shift (d : int) (ty : ty_nameless) =
  let rec walk (c : int) = function
    | UTyUnit -> UTyUnit
    | UTyBool -> UTyBool
    | UTyNat -> UTyNat
    | UTyVar i -> UTyVar (if i >= c then i + d else i)
    | UTyTuple tys -> UTyTuple (List.map ~f:(walk c) tys)
    | UTyRecord r -> UTyRecord (List.map ~f:(Tuple2.map_snd ~f:(walk c)) r)
    | UTyArrow (l, r) -> UTyArrow (walk c l, walk c r)
    | UTyRef ty -> UTyRef (walk c ty)
    | UTyForall ty -> UTyForall (walk (c + 1) ty)
    | UTyExists ty -> UTyExists (walk (c + 1) ty)
  in
  walk 0 ty
;;

let subst (j : int) (s : ty_nameless) (ty : ty_nameless) =
  let rec walk (c : int) = function
    | UTyUnit -> UTyUnit
    | UTyBool -> UTyBool
    | UTyNat -> UTyNat
    | UTyVar i -> if i = j + c then shift c s else UTyVar i
    | UTyTuple tys -> UTyTuple (List.map ~f:(walk c) tys)
    | UTyRecord r -> UTyRecord (List.map ~f:(Tuple2.map_snd ~f:(walk c)) r)
    | UTyArrow (l, r) -> UTyArrow (walk c l, walk c r)
    | UTyRef ty -> UTyRef (walk c ty)
    | UTyForall ty -> UTyForall (walk (c + 1) ty)
    | UTyExists ty -> UTyExists (walk (c + 1) ty)
  in
  walk 0 ty
;;

let subst_top (b : ty_nameless) (t : ty_nameless) = shift (-1) (subst 0 (shift 1 b) t)

let is_free (ty : ty_nameless) : bool =
  let rec aux depth = function
    | UTyUnit | UTyBool | UTyNat -> false
    | UTyVar k -> Int.equal k depth
    | UTyTuple ts -> List.exists ts ~f:(aux depth)
    | UTyRecord r -> List.exists r ~f:(fun (_, t) -> aux depth t)
    | UTyArrow (l, r) -> aux depth l || aux depth r
    | UTyRef t -> aux depth t
    | UTyForall t | UTyExists t -> aux (depth + 1) t
  in
  aux 0 ty
;;

let assert_unique_fields fields =
  if Set.length (String.Set.of_list fields) = List.length fields
  then Ok ()
  else error_s [%message "duplicated labels in fields" (fields : string list)]
;;

let rec type_of (ctx : ty_nameless String.Map.t) (ty_ctx : string list) (t : t)
  : ty_nameless Or_error.t
  =
  let open Or_error.Let_syntax in
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
    (match%bind type_of ctx ty_ctx t with
     | UTyTuple tys ->
       (match List.nth tys i with
        | Some ty -> Ok ty
        | None ->
          error_s
            [%message
              "tuple projection on invalid index" (tys : ty_nameless list) (i : int)])
     | _ -> error_s [%message "expected tuple to project from" (t : t)])
  | EProjRecord (t, l) ->
    (match%bind type_of ctx ty_ctx t with
     | UTyRecord tys ->
       let%bind () = assert_unique_fields (List.map ~f:fst tys) in
       (match List.Assoc.find tys l ~equal:String.equal with
        | Some ty -> Ok ty
        | None ->
          error_s
            [%message
              "record missing field" (tys : (string * ty_nameless) list) (l : string)])
     | _ -> error_s [%message "expected record to project from" (t : t)])
  | ESeq (t, t') ->
    let%bind ty_t = type_of ctx ty_ctx t in
    if equal_ty_nameless ty_t UTyUnit
    then type_of ctx ty_ctx t'
    else error_s [%message "[ESeq (t, t')] expected t to be unit" (ty_t : ty_nameless)]
  | EIf (c, t, f) ->
    let%bind ty_c = type_of ctx ty_ctx c in
    if not (equal_ty_nameless ty_c UTyBool)
    then error_s [%message "[if] condition is not TyBool" (ty_c : ty_nameless)]
    else (
      let%bind ty_t = type_of ctx ty_ctx t in
      let%bind ty_f = type_of ctx ty_ctx f in
      if not (equal_ty_nameless ty_t ty_f)
      then
        error_s
          [%message
            "[if] branches have unequal types" (ty_t : ty_nameless) (ty_f : ty_nameless)]
      else Ok ty_t)
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
    (match ty_f with
     | UTyArrow (ty_arg, ty_body) ->
       let%bind ty_x = type_of ctx ty_ctx x in
       if equal_ty_nameless ty_arg ty_x
       then Ok ty_body
       else
         error_s
           [%message
             "arg can't be applied to func" (ty_f : ty_nameless) (ty_x : ty_nameless)]
     | _ ->
       error_s [%message "attempting to apply to non-arrow type" (ty_f : ty_nameless)])
  | EZero -> Ok UTyNat
  | ESucc t ->
    (match%bind type_of ctx ty_ctx t with
     | UTyNat -> Ok UTyNat
     | ty_t -> error_s [%message "expected succ to take nat" (ty_t : ty_nameless)])
  | EPred t ->
    (match%bind type_of ctx ty_ctx t with
     | UTyNat -> Ok UTyNat
     | ty_t -> error_s [%message "expected pred to take nat" (ty_t : ty_nameless)])
  | EIsZero t ->
    (match%bind type_of ctx ty_ctx t with
     | UTyNat -> Ok UTyBool
     | ty_t -> error_s [%message "expected iszero to take nat" (ty_t : ty_nameless)])
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
       if equal_ty_nameless ty_v ty_t
       then Ok UTyUnit
       else
         error_s
           [%message
             "assigning to ref of wrong type" (ty_v : ty_nameless) (ty_t : ty_nameless)]
     | Some ty -> error_s [%message "cannot assign to non-ref" (ty : ty_nameless)]
     | None -> error_s [%message "var not in context" v (ctx : ty_nameless String.Map.t)])
  | ETyAbs (ty_var, t) ->
    let ctx' = Map.map ctx ~f:(shift 1) in
    let ty_ctx' = ty_var :: ty_ctx in
    let%map ty_t = type_of ctx' ty_ctx' t in
    UTyForall ty_t
  | ETyApp (t, ty_arg) ->
    let%bind ty_t = type_of ctx ty_ctx t in
    let%bind ty_arg = remove_names ty_ctx ty_arg in
    (match ty_t with
     | UTyForall ty_body -> Ok (subst_top ty_arg ty_body)
     | _ -> error_s [%message "expected universal type" (ty_t : ty_nameless)])
  | EPack (ty_real, t, ty_package) ->
    let%bind ty_real = remove_names ty_ctx ty_real in
    let%bind ty_package = remove_names ty_ctx ty_package in
    (match ty_package with
     | UTyExists ty_body ->
       let%bind ty_t = type_of ctx ty_ctx t in
       let expected_ty = subst_top ty_real ty_body in
       if equal_ty_nameless ty_t expected_ty
       then Ok ty_package
       else
         error_s
           [%message
             "pack term does not match declared existential type"
               (ty_t : ty_nameless)
               (expected_ty : ty_nameless)]
     | _ ->
       error_s
         [%message
           "pack annotation must be an existential type" (ty_package : ty_nameless)])
  | EUnpack (ty_v, t_v, t_package, t_body) ->
    let%bind ty_pkg = type_of ctx ty_ctx t_package in
    (match ty_pkg with
     | UTyExists ex_body ->
       let ctx_shifted = Map.map ctx ~f:(shift 1) in
       let ctx' = Map.set ctx_shifted ~key:t_v ~data:ex_body in
       let ty_ctx' = ty_v :: ty_ctx in
       let%bind result_ty = type_of ctx' ty_ctx' t_body in
       if is_free result_ty
       then
         error_s
           [%message "existential type variable escapes scope" (result_ty : ty_nameless)]
       else Ok result_ty
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
    | UTyUnit -> TyUnit
    | UTyBool -> TyBool
    | UTyNat -> TyNat
    | UTyVar i -> TyVar (List.nth_exn env i)
    | UTyTuple ts -> TyTuple (List.map ts ~f:(aux env))
    | UTyRecord fs -> TyRecord (List.map fs ~f:(fun (l, ty) -> l, aux env ty))
    | UTyArrow (l, r) -> TyArrow (aux env l, aux env r)
    | UTyRef ty -> TyRef (aux env ty)
    | UTyForall body ->
      let v = gensym () in
      TyForall (v, aux (v :: env) body)
    | UTyExists body ->
      let v = gensym () in
      TyExists (v, aux (v :: env) body)
  in
  aux [] ty
;;

let typecheck t = Or_error.map ~f:rename_tyvars (type_of String.Map.empty [] t)
