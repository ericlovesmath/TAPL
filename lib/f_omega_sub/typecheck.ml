open Core
open Types

type ty_context_entry = string * ty_nameless * kind [@@deriving sexp_of]
type ty_context = ty_context_entry list [@@deriving sexp_of]

let find_kind (ctx : ty_context) (i : int) : kind Or_error.t =
  match List.nth ctx i with
  | Some (_, _, k) -> Ok k
  | None ->
    Or_error.error_s
      [%message "failed to find type variable in context" (i : int) (ctx : ty_context)]
;;

let rec remove_names (ctx : ty_context) (ty : ty) : ty_nameless Or_error.t =
  let open Or_error.Let_syntax in
  match ty with
  | TyTop k -> Ok (UTyTop k)
  | TyUnit -> Ok UTyUnit
  | TyBool -> Ok UTyBool
  | TyNat -> Ok UTyNat
  | TyVar v ->
    let rec find' acc = function
      | [] -> error_s [%message "failed to find type variable" v (ctx : ty_context)]
      | (h, _, _) :: _ when String.equal h v -> Ok acc
      | _ :: t -> find' (acc + 1) t
    in
    let%map v = find' 0 ctx in
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
  | TyForall (v, bound, k, ty) ->
    let%bind bound = remove_names ctx bound in
    let%map ty = remove_names ((v, bound, k) :: ctx) ty in
    UTyForall (bound, k, ty)
  | TyExists (v, bound, k, ty) ->
    let%bind bound = remove_names ctx bound in
    let%map ty = remove_names ((v, bound, k) :: ctx) ty in
    UTyExists (bound, k, ty)
  | TyAbs (v, k, ty) ->
    (* When we abstract a type, we assume it's upper bounded by Top of that kind *)
    let%map ty = remove_names ((v, UTyTop k, k) :: ctx) ty in
    UTyAbs (k, ty)
  | TyApp (t1, t2) ->
    let%bind t1 = remove_names ctx t1 in
    let%bind t2 = remove_names ctx t2 in
    return (UTyApp (t1, t2))
;;

let map_ty f ty =
  let rec walk c = function
    | UTyTop k -> UTyTop k
    | UTyUnit -> UTyUnit
    | UTyBool -> UTyBool
    | UTyNat -> UTyNat
    | UTyVar i -> f c i
    | UTyTuple ts -> UTyTuple (List.map ts ~f:(walk c))
    | UTyRecord r -> UTyRecord (List.map r ~f:(Tuple2.map_snd ~f:(walk c)))
    | UTyArrow (l, r) -> UTyArrow (walk c l, walk c r)
    | UTyRef ty -> UTyRef (walk c ty)
    | UTyForall (bound, k, ty) -> UTyForall (walk c bound, k, walk (c + 1) ty)
    | UTyExists (bound, k, ty) -> UTyExists (walk c bound, k, walk (c + 1) ty)
    | UTyAbs (k, ty) -> UTyAbs (k, walk (c + 1) ty)
    | UTyApp (t1, t2) -> UTyApp (walk c t1, walk c t2)
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
    | UTyTop _ | UTyUnit | UTyBool | UTyNat -> false
    | UTyVar k -> Int.equal k depth
    | UTyTuple ts -> List.exists ts ~f:(aux depth)
    | UTyRecord r -> List.exists r ~f:(fun (_, t) -> aux depth t)
    | UTyArrow (l, r) -> aux depth l || aux depth r
    | UTyRef t -> aux depth t
    | UTyForall (bound, _, t) | UTyExists (bound, _, t) ->
      aux depth bound || aux (depth + 1) t
    | UTyAbs (_, t) -> aux (depth + 1) t
    | UTyApp (t1, t2) -> aux depth t1 || aux depth t2
  in
  aux 0 ty
;;

let rec kind_of (ctx : ty_context) (ty : ty_nameless) : kind Or_error.t =
  let open Or_error.Let_syntax in
  match ty with
  | UTyTop k -> Ok k
  | UTyUnit | UTyBool | UTyNat -> Ok KiStar
  | UTyVar i -> find_kind ctx i
  | UTyTuple ts ->
    let%bind () =
      Or_error.all_unit
        (List.map ts ~f:(fun t ->
           let%bind k = kind_of ctx t in
           if equal_kind k KiStar
           then Ok ()
           else
             error_s
               [%message "tuple element must have kind *" (t : ty_nameless) (k : kind)]))
    in
    Ok KiStar
  | UTyRecord r ->
    let%bind () =
      Or_error.all_unit
        (List.map r ~f:(fun (_, t) ->
           let%bind k = kind_of ctx t in
           if equal_kind k KiStar
           then Ok ()
           else
             error_s
               [%message "record field must have kind *" (t : ty_nameless) (k : kind)]))
    in
    Ok KiStar
  | UTyArrow (l, r) ->
    let%bind kl = kind_of ctx l in
    let%bind kr = kind_of ctx r in
    if equal_kind kl KiStar && equal_kind kr KiStar
    then Ok KiStar
    else
      error_s
        [%message
          "arrow types must connect types of kind *"
            (l : ty_nameless)
            (kl : kind)
            (r : ty_nameless)
            (kr : kind)]
  | UTyRef t ->
    let%bind k = kind_of ctx t in
    if equal_kind k KiStar
    then Ok KiStar
    else error_s [%message "ref must contain type of kind *" (t : ty_nameless) (k : kind)]
  | UTyForall (bound, k, t) ->
    let%bind k_bound = kind_of ctx bound in
    if not (equal_kind k_bound k)
    then error_s [%message "bound kind mismatch" (k_bound : kind) (k : kind)]
    else (
      let%bind kt = kind_of (("", bound, k) :: ctx) t in
      if equal_kind kt KiStar
      then Ok KiStar
      else error_s [%message "forall body must have kind *" (t : ty_nameless) (kt : kind)])
  | UTyExists (bound, k, t) ->
    let%bind k_bound = kind_of ctx bound in
    if not (equal_kind k_bound k)
    then error_s [%message "bound kind mismatch" (k_bound : kind) (k : kind)]
    else (
      let%bind kt = kind_of (("", bound, k) :: ctx) t in
      if equal_kind kt KiStar
      then Ok KiStar
      else error_s [%message "exists body must have kind *" (t : ty_nameless) (kt : kind)])
  | UTyAbs (k, t) ->
    let%map kt = kind_of (("", UTyTop k, k) :: ctx) t in
    KiArrow (k, kt)
  | UTyApp (t1, t2) ->
    let%bind k1 = kind_of ctx t1 in
    let%bind k2 = kind_of ctx t2 in
    (match k1 with
     | KiArrow (k11, k12) ->
       if equal_kind k11 k2
       then Ok k12
       else
         error_s
           [%message
             "kind mismatch in type application"
               (t1 : ty_nameless)
               (k1 : kind)
               (t2 : ty_nameless)
               (k2 : kind)]
     | KiStar ->
       error_s
         [%message
           "attempting to apply a type of kind * as a type operator" (t1 : ty_nameless)])
;;

let rec simplify (ty : ty_nameless) : ty_nameless =
  match ty with
  | UTyApp (t1, t2) ->
    let t1 = simplify t1 in
    let t2 = simplify t2 in
    (match t1 with
     | UTyAbs (_, t12) -> simplify (subst_top t2 t12)
     | _ -> UTyApp (t1, t2))
  | UTyTuple ts -> UTyTuple (List.map ts ~f:simplify)
  | UTyRecord r -> UTyRecord (List.map r ~f:(fun (l, t) -> l, simplify t))
  | UTyArrow (l, r) -> UTyArrow (simplify l, simplify r)
  | UTyRef t -> UTyRef (simplify t)
  | UTyForall (bound, k, t) -> UTyForall (simplify bound, k, simplify t)
  | UTyExists (bound, k, t) -> UTyExists (simplify bound, k, simplify t)
  | UTyAbs (k, t) -> UTyAbs (k, simplify t)
  | _ -> ty
;;

let assert_unique_fields fields =
  if Set.length (String.Set.of_list fields) = List.length fields
  then Ok ()
  else error_s [%message "duplicated labels in fields" (fields : string list)]
;;

let rec expose (ctx : ty_context) (ty : ty_nameless) : ty_nameless =
  match simplify ty with
  | UTyVar i ->
    (match List.nth ctx i with
     | Some (_, bound, _) -> expose ctx (shift (i + 1) bound)
     | None -> ty)
  | UTyApp (t1, t2) -> simplify (UTyApp (expose ctx t1, t2))
  | ty -> ty
;;

let rec subtype (ctx : ty_context) (ty : ty_nameless) (ty' : ty_nameless) =
  let ty = simplify ty in
  let ty' = simplify ty' in
  if equal_ty_nameless ty ty'
  then true
  else (
    match ty, ty' with
    | _, UTyTop k' ->
      (match kind_of ctx ty with
       | Ok k -> equal_kind k k'
       | Error _ -> false)
    | UTyVar i, _ ->
      (match List.nth ctx i with
       | Some (_, bound, _) -> subtype ctx (shift (i + 1) bound) ty'
       | None -> false)
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
    | UTyRef t, UTyRef t' -> equal_ty_nameless t t' (* Invariant *)
    | UTyForall (bound1, k1, t1), UTyForall (bound2, k2, t2) ->
      equal_kind k1 k2
      && subtype ctx bound2 bound1
      && subtype (("", bound2, k2) :: ctx) t1 t2
    | UTyAbs (k, _), UTyAbs (k', _) ->
      equal_kind k k'
      &&
      (* Pointwise subtyping for type operators?
            TAPL 31.3 says S <: T iff S T1 .. Tn <: T T1 .. Tn
            But here we check if bodies are subtypes under assumption?
            Actually, pure F_omega_sub usually only subtyping at kind *?
            TAPL 31.4 Higher-order subtyping.
            S :: K, T :: K.
            If K = K1 -> K2, then S <: T iff forall U :: K1, S U <: T U.
            We can check this by generating a fresh variable X :: K1 and checking S X <: T X.
            *)
      let ctx' = ("", UTyTop k, k) :: ctx in
      let t_app = simplify (UTyApp (shift 1 ty, UTyVar 0)) in
      let t'_app = simplify (UTyApp (shift 1 ty', UTyVar 0)) in
      subtype ctx' t_app t'_app
    | _ -> false)
;;

let rec join (ctx : ty_context) (ty : ty_nameless) (ty' : ty_nameless) : ty_nameless =
  let ty = simplify ty in
  let ty' = simplify ty' in
  if subtype ctx ty ty'
  then ty'
  else if subtype ctx ty' ty
  then ty
  else (
    match ty, ty' with
    | UTyVar i, _ ->
      (match List.nth ctx i with
       | Some (_, bound, _) -> join ctx (shift (i + 1) bound) ty'
       | None -> UTyTop KiStar)
    | _, UTyVar i ->
      (match List.nth ctx i with
       | Some (_, bound, _) -> join ctx ty (shift (i + 1) bound)
       | None -> UTyTop KiStar)
    | UTyRecord r, UTyRecord r' ->
      let r'' =
        List.filter_map r' ~f:(fun (l, ty) ->
          let%map.Option ty' = List.Assoc.find r l ~equal:String.equal in
          l, join ctx ty ty')
      in
      if List.is_empty r'' then UTyTop KiStar else UTyRecord r''
    | UTyTuple ts, UTyTuple ts' ->
      (match List.map2 ts ts' ~f:(join ctx) with
       | Ok ts'' -> UTyTuple ts''
       | Unequal_lengths -> UTyTop KiStar)
    | UTyArrow (a, b), UTyArrow (a', b') ->
      if equal_ty_nameless a a' then UTyArrow (a, join ctx b b') else UTyTop KiStar
    | UTyRef t, UTyRef t' -> if equal_ty_nameless t t' then UTyRef t else UTyTop KiStar
    | UTyForall (bound1, k1, t1), UTyForall (bound2, k2, t2) ->
      if equal_kind k1 k2 && equal_ty_nameless bound1 bound2
      then UTyForall (bound1, k1, join (("", bound1, k1) :: ctx) t1 t2)
      else UTyTop KiStar
    | _ -> UTyTop KiStar (* Simplified join *))
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
          error_s
            [%message
              "record missing field" (tys : (string * ty_nameless) list) (l : string)])
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
    let%bind k = kind_of ty_ctx ty_v in
    if not (equal_kind k KiStar)
    then
      error_s
        [%message
          "abstraction parameter must have kind *" (ty_v : ty_nameless) (k : kind)]
    else (
      let ctx = Map.set ctx ~key:v ~data:ty_v in
      let%bind ty_t = type_of ctx ty_ctx t in
      return (UTyArrow (ty_v, ty_t)))
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
     | _ -> error_s [%message "can't apply to non-arrow type" (ty_f : ty_nameless)])
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
  | ETyAbs (ty_var, bound, k, t) ->
    let%bind bound = remove_names ty_ctx bound in
    let%bind k_bound = kind_of ty_ctx bound in
    if not (equal_kind k k_bound)
    then error_s [%message "bound kind mismatch" (k : kind) (k_bound : kind)]
    else (
      let ctx' = Map.map ctx ~f:(shift 1) in
      let ty_ctx' = (ty_var, bound, k) :: ty_ctx in
      let%map ty_t = type_of ctx' ty_ctx' t in
      UTyForall (bound, k, ty_t))
  | ETyApp (t, ty_arg) ->
    let%bind ty_t = type_of ctx ty_ctx t in
    let%bind ty_arg = remove_names ty_ctx ty_arg in
    (match expose ty_t with
     | UTyForall (ty_sub, k_bound, ty_body) ->
       let%bind k_arg = kind_of ty_ctx ty_arg in
       if not (equal_kind k_bound k_arg)
       then
         error_s
           [%message "kind mismatch in type application" (k_bound : kind) (k_arg : kind)]
       else if subtype ty_ctx ty_arg ty_sub
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
     | UTyExists (ty_bound, k_bound, ty_body) ->
       let%bind k_real = kind_of ty_ctx ty_real in
       if not (equal_kind k_bound k_real)
       then error_s [%message "kind mismatch in pack" (k_bound : kind) (k_real : kind)]
       else if not (subtype ty_ctx ty_real ty_bound)
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
     | UTyExists (ty_bound, k_bound, ty_body) ->
       let ctx_shifted = Map.map ctx ~f:(shift 1) in
       let ctx = Map.set ctx_shifted ~key:t_v ~data:ty_body in
       let ty_ctx = (ty_v, ty_bound, k_bound) :: ty_ctx in
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
    | UTyTop k -> TyTop k
    | UTyUnit -> TyUnit
    | UTyBool -> TyBool
    | UTyNat -> TyNat
    | UTyVar i -> TyVar (List.nth_exn env i)
    | UTyTuple ts -> TyTuple (List.map ts ~f:(aux env))
    | UTyRecord fs -> TyRecord (List.map fs ~f:(fun (l, ty) -> l, aux env ty))
    | UTyArrow (l, r) -> TyArrow (aux env l, aux env r)
    | UTyRef ty -> TyRef (aux env ty)
    | UTyForall (bound, k, body) ->
      let v = gensym () in
      TyForall (v, aux env bound, k, aux (v :: env) body)
    | UTyExists (bound, k, body) ->
      let v = gensym () in
      TyExists (v, aux env bound, k, aux (v :: env) body)
    | UTyAbs (k, body) ->
      let v = gensym () in
      TyAbs (v, k, aux (v :: env) body)
    | UTyApp (t, t') -> TyApp (aux env t, aux env t')
  in
  aux [] ty
;;

let typecheck t =
  t
  |> type_of String.Map.empty []
  |> Or_error.map ~f:simplify
  |> Or_error.map ~f:rename_tyvars
;;
