open Core
open Types
open Or_error.Let_syntax
module Unique_id = Unique_id.Int ()

type substitution = (string * ty) list [@@deriving sexp_of]
type constraints = (ty * ty) list [@@deriving sexp_of]

let gensym () = "v" ^ Unique_id.(to_string (create ()))

(* TODO: More careful consideration of free type variables *)

let rec constraints (ctx : ty String.Map.t) (t : t) : (ty * constraints) Or_error.t =
  match t with
  | EUnit -> return (TyUnit, [])
  | ETrue | EFalse -> return (TyBool, [])
  | EIf (c, t, f) ->
    let%bind ty_c, con_c = constraints ctx c in
    let%bind ty_t, con_t = constraints ctx t in
    let%bind ty_f, con_f = constraints ctx f in
    let con = ((ty_c, TyBool) :: (ty_t, ty_f) :: con_c) @ con_t @ con_f in
    return (ty_t, con)
  | EVar v ->
    let%map ty = Map.find_or_error ctx v in
    ty, []
  | EAbs (v, t) ->
    let ty_var = TyVar (gensym ()) in
    let ctx = Map.set ctx ~key:v ~data:ty_var in
    let%map ty_t, con = constraints ctx t in
    TyArrow (ty_var, ty_t), con
  | EApp (f, x) ->
    let%bind ty_f, con_f = constraints ctx f in
    let%bind ty_x, con_x = constraints ctx x in
    let v = TyVar (gensym ()) in
    return (v, ((ty_f, TyArrow (ty_x, v)) :: con_f) @ con_x)
  | EZero -> return (TyNat, [])
  | ESucc t | EPred t ->
    let%map ty, con = constraints ctx t in
    TyNat, (ty, TyNat) :: con
  | EIsZero t ->
    let%map ty, con = constraints ctx t in
    TyBool, (ty, TyNat) :: con
  | ELet (_, _, _) -> failwith "TODO Let Constraints"
;;

let apply_to_ty (sub : substitution) (ty : ty) : ty =
  let rec aux (ty : ty) =
    match ty with
    | TyVar v -> List.Assoc.find ~equal:String.equal sub v |> Option.value ~default:ty
    | TyUnit | TyBool | TyNat -> ty
    | TyArrow (f, x) -> TyArrow (aux f, aux x)
  in
  aux ty
;;

let apply_to_constraints (sub : substitution) (con : constraints) : constraints =
  List.map con ~f:(fun (l, r) -> apply_to_ty sub l, apply_to_ty sub r)
;;

let rec unify (con : constraints) : substitution Or_error.t =
  match con with
  | [] -> return []
  | (TyVar v, ty) :: con | (ty, TyVar v) :: con ->
    if equal_ty (TyVar v) ty
    then unify con
    else (
      let%map sub = unify (apply_to_constraints [ v, ty ] con) in
      (v, ty) :: sub)
  | (((TyNat | TyUnit | TyBool) as ty), ty') :: con
  | (ty, ((TyNat | TyUnit | TyBool) as ty')) :: con ->
    if equal_ty ty ty'
    then unify con
    else error_s [%message "unify: invalid equality constraint" (ty : ty) (ty' : ty)]
  | (TyArrow (f, x), TyArrow (f', x')) :: con -> unify ((f, f') :: (x, x') :: con)
;;

(** Renames type variables to OCaml-like naming, assumes no free tyvars.
    Should never fail on all inputs. *)
let rename_tyvars (ty : ty) : ty =
  let counter = ref 0 in
  let gensym () =
    let i = !counter in
    incr counter;
    let char_code = Char.to_int 'a' + (i mod 26) in
    let num_suffix = i / 26 in
    let base_name = String.of_char (Char.of_int_exn char_code) in
    if num_suffix = 0 then base_name else base_name ^ Int.to_string num_suffix
  in
  let rec collect_vars set = function
    | TyVar v -> Set.add set v
    | TyUnit | TyBool | TyNat -> set
    | TyArrow (ty, ty') -> collect_vars (collect_vars set ty) ty'
  in
  let substitution_map =
    ty
    |> collect_vars String.Set.empty
    |> Set.to_list
    |> List.map ~f:(fun v -> v, gensym ())
    |> String.Map.of_alist_exn
  in
  let rec rename = function
    | TyVar v -> TyVar (Map.find_exn substitution_map v)
    | TyArrow (ty, ty') -> TyArrow (rename ty, rename ty')
    | (TyUnit | TyBool | TyNat) as ty -> ty
  in
  rename ty
;;

let typecheck t =
  let%bind ty_s, con = constraints String.Map.empty t in
  let%bind sub = unify con in
  let ty = apply_to_ty sub ty_s in
  Or_error.try_with (fun () -> rename_tyvars ty)
;;
