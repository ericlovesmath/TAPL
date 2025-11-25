open Core
open Types
open Or_error.Let_syntax
module Unique_id = Unique_id.Int ()

type substitution = (string * ty) list [@@deriving sexp_of]

(** List of equality constraints on types *)
type constraints = (ty * ty) list [@@deriving sexp_of]

(** forall x1 x2 ... . t1 *)
type type_scheme = string list * ty [@@deriving sexp_of]

type context = type_scheme String.Map.t

let gensym () = "v" ^ Unique_id.(to_string (create ()))

let apply_to_ty (sub : substitution) (ty : ty) : ty =
  let rec aux (ty : ty) =
    match ty with
    | TyVar v -> List.Assoc.find ~equal:String.equal sub v |> Option.value ~default:ty
    | TyUnit | TyBool | TyNat -> ty
    | TyArrow (f, x) -> TyArrow (aux f, aux x)
    | TyRef ty -> TyRef (aux ty)
  in
  aux ty
;;

let apply_to_context (sub : substitution) (ctx : context) : type_scheme String.Map.t =
  Map.map ctx ~f:(fun (vars, ty) ->
    let sub' = List.filter sub ~f:(fun (v, _) -> List.mem vars v ~equal:String.( <> )) in
    vars, apply_to_ty sub' ty)
;;

let apply_to_constraints (sub : substitution) (con : constraints) : constraints =
  List.map con ~f:(fun (l, r) -> apply_to_ty sub l, apply_to_ty sub r)
;;

(** Generalize a type by quantifying over all type variables not free in the context *)
let generalize (ctx : context) (ty : ty) : type_scheme =
  let rec ftv_of_ty = function
    | TyVar v -> String.Set.singleton v
    | TyUnit | TyBool | TyNat -> String.Set.empty
    | TyArrow (t1, t2) -> Set.union (ftv_of_ty t1) (ftv_of_ty t2)
    | TyRef ty -> ftv_of_ty ty
  in
  let ftv_of_scheme scheme =
    let vars, ty = scheme in
    Set.diff (ftv_of_ty ty) (String.Set.of_list vars)
  in
  let ftv_of_context (ctx : context) : String.Set.t =
    Map.data ctx |> List.map ~f:ftv_of_scheme |> String.Set.union_list
  in
  Set.to_list (Set.diff (ftv_of_ty ty) (ftv_of_context ctx)), ty
;;

let rec unify (con : constraints) : substitution Or_error.t =
  match con with
  | [] -> return []
  | (TyVar v, ty) :: con | (ty, TyVar v) :: con ->
    let rec has_infinite_type = function
      | TyVar v' -> String.equal v v'
      | TyUnit | TyBool | TyNat -> false
      | TyArrow (ty, ty') -> has_infinite_type ty || has_infinite_type ty'
      | TyRef ty -> has_infinite_type ty
    in
    if has_infinite_type ty
    then error_s [%message "unify: recursive unification" (v : string) (ty : ty)]
    else if equal_ty (TyVar v) ty
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
  | ((TyArrow _ as ty), ty') :: _ | (ty', (TyArrow _ as ty)) :: _ ->
    error_s [%message "unify: -> type expects another ->" (ty : ty) (ty' : ty)]
  | (TyRef ty, TyRef ty') :: con -> unify ((ty, ty') :: con)
;;

(** Determines if an expression is a syntactic value (non-expansive). *)
let rec is_syntactic_value (t : t) : bool =
  match t with
  | EUnit | ETrue | EFalse | EZero | EAbs _ | EVar _ -> true
  | EIf _ | EApp _ | EPred _ | EIsZero _ | ELet _ | ERef _ | EDeref _ | EAssign _ | EFix _
    -> false
  | ESucc t -> is_syntactic_value t
;;

let rec constraints (ctx : context) (t : t) : (ty * constraints) Or_error.t =
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
    let%map vars, ty = Map.find_or_error ctx v in
    (* instantiating new type variables *)
    let sub = List.map vars ~f:(fun v -> v, TyVar (gensym ())) in
    apply_to_ty sub ty, []
  | EAbs (v, t) ->
    let ty_var = TyVar (gensym ()) in
    let ctx = Map.set ctx ~key:v ~data:([], ty_var) in
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
  | ELet (v, b, t) ->
    let%bind ty_bind, con_bind = constraints ctx b in
    let%bind sub_bind = unify con_bind in
    let ty_bind = apply_to_ty sub_bind ty_bind in
    let ctx = apply_to_context sub_bind ctx in
    (* NOTE: Value Restriction, only generalize if syntactic *)
    let scheme = if is_syntactic_value b then generalize ctx ty_bind else [], ty_bind in
    let ctx_for_body = Map.set ctx ~key:v ~data:scheme in
    let%bind ty_body, con_body = constraints ctx_for_body t in
    return (ty_body, con_bind @ con_body)
  | ERef e ->
    let%map ty_e, con_e = constraints ctx e in
    TyRef ty_e, con_e
  | EDeref e ->
    let%bind ty_e, con_e = constraints ctx e in
    let v = TyVar (gensym ()) in
    return (v, (ty_e, TyRef v) :: con_e)
  | EAssign (v, t) ->
    let%bind vars, ty_scheme = Map.find_or_error ctx v in
    let sub = List.map vars ~f:(fun v -> v, TyVar (gensym ())) in
    let ty_v = apply_to_ty sub ty_scheme in
    let%bind ty_t, con = constraints ctx t in
    return (TyUnit, (ty_v, TyRef ty_t) :: con)
  | EFix t ->
    let%bind ty, con = constraints ctx t in
    let v = TyVar (gensym ()) in
    return (v, (ty, TyArrow (v, v)) :: con)
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
    | TyRef ty -> collect_vars set ty
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
    | TyRef ty -> TyRef (rename ty)
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
