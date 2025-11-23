open Core
open Types

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
  | EAbs (_, _) -> failwith "TODO"
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
;;

let typecheck = type_of String.Map.empty
