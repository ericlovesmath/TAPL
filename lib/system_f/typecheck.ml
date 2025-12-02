open Core
open Types

let rec ty_subst (v : string) (src : ty) (dst : ty) : ty =
  let aux = ty_subst v src in
  match dst with
  | TyVar v' -> if String.equal v v' then src else dst
  | TyUnit | TyBool | TyNat -> dst
  | TyArrow (f, x) -> TyArrow (aux f, aux x)
  | TyForall (v', t) ->
    if String.equal v v' then TyForall (v', t) else TyForall (v', aux t)
  | TyExists (v', t) ->
    if String.equal v v' then TyExists (v', t) else TyExists (v', aux t)
;;

let rec is_free (v : string) (t : ty) : bool =
  match t with
  | TyVar v' -> String.equal v v'
  | TyUnit | TyBool | TyNat -> false
  | TyArrow (ty, ty') -> is_free v ty || is_free v ty'
  | TyForall (v', ty) | TyExists (v', ty) ->
    if String.equal v v' then false else is_free v ty
;;

let rec type_of (ctx : ty String.Map.t) (t : t) : ty Or_error.t =
  let open Or_error.Let_syntax in
  match t with
  | EUnit -> Ok TyUnit
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
       if equal_ty ty_arg ty_x
       then Ok ty_body
       else error_s [%message "arg can't be applied to func" (ty_f : ty) (ty_arg : ty)]
     | _ -> error_s [%message "attempting to apply to non-arrow type" (ty_f : ty)])
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
  | ETyAbs (ty_var, t) ->
    let%map ty_t = type_of ctx t in
    TyForall (ty_var, ty_t)
  | ETyApp (t, ty_arg) ->
    let%bind ty_t = type_of ctx t in
    (match ty_t with
     | TyForall (ty_var, ty_body) -> Ok (ty_subst ty_var ty_arg ty_body)
     | _ -> error_s [%message "expected universal type" (ty_t : ty)])
  | EPack (ty_real, t, ty_package) ->
    (match ty_package with
     | TyExists (ty_var, ty_body) ->
       let%bind ty_t = type_of ctx t in
       let expected_ty = ty_subst ty_var ty_real ty_body in
       if equal_ty ty_t expected_ty
       then Ok ty_package
       else
         error_s
           [%message
             "pack term does not match declared existential type"
               (ty_t : ty)
               (expected_ty : ty)]
     | _ ->
       error_s [%message "pack annotation must be an existential type" (ty_package : ty)])
  | EUnpack (ty_v, t_v, t_package, t_body) ->
    let%bind ty_pkg = type_of ctx t_package in
    (match ty_pkg with
     | TyExists (ex_v, ex_b) ->
       let concrete_body_ty = ty_subst ex_v (TyVar ty_v) ex_b in
       let ctx = Map.set ctx ~key:t_v ~data:concrete_body_ty in
       let%bind result_ty = type_of ctx t_body in
       if is_free ty_v result_ty
       then
         error_s
           [%message
             "existential type variable escapes scope" (ty_v : string) (result_ty : ty)]
       else Ok result_ty
     | _ -> error_s [%message "unpack expects existential type" (ty_pkg : ty)])
;;

let typecheck = type_of String.Map.empty
