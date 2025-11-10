open Core
open Types
open Or_error.Let_syntax

(* TODO: Better Error Messages *)
(* TODO: Check for cycles *)

(** Look up type of method in class, [fields -> term]  (Fig 19.2) *)
let rec method_type (tbl : class_decl list) (m : method_name) (c : class_name) =
  let%bind cl = Utils.find_class tbl c in
  let md_opt = List.find cl.methods ~f:(fun md -> equal_method_name md.method_name m) in
  match md_opt with
  | Some md -> return (List.map ~f:fst md.fields, fst md.term)
  | None -> method_type tbl m cl.superclass_name
;;

let rec type_of (tbl : class_decl list) (ctx : class_name String.Map.t) = function
  | Var v ->
    (* T-Var *)
    Map.find_or_error ctx v
    |> Or_error.tag_s ~tag:[%message "no var in ctx" v (ctx : class_name String.Map.t)]
  | FieldAccess (t, field) ->
    (* T-Field *)
    let%bind class_name = type_of tbl ctx t in
    let%bind fields = Utils.fields tbl class_name in
    fields
    |> List.find ~f:(Fn.compose (equal_field_name field) snd)
    |> Option.map ~f:fst
    |> Or_error.of_option ~error:(Error.of_string "missing field")
  | InvokeMethod (t, mn, args) ->
    (* T-Invk *)
    let%bind cn = type_of tbl ctx t in
    let%bind arg_cns = Or_error.all (List.map ~f:(type_of tbl ctx) args) in
    let%bind field_cns, ret = method_type tbl mn cn in
    (match List.for_all2 arg_cns field_cns ~f:(Utils.is_subtype tbl) with
     | Ok true -> return ret
     | Ok false -> Or_error.error_string "args and fields not subtyped"
     | Unequal_lengths -> Or_error.error_string "args and fields diff length")
  | CreateObject (class_name, args) ->
    (* T-New *)
    let%bind arg_cns = Or_error.all (List.map ~f:(type_of tbl ctx) args) in
    let%bind fields = Utils.fields tbl class_name in
    let field_cns = List.map ~f:fst fields in
    (match List.for_all2 arg_cns field_cns ~f:(Utils.is_subtype tbl) with
     | Ok true -> return class_name
     | Ok false -> Or_error.error_string "args and fields not subtyped"
     | Unequal_lengths -> Or_error.error_string "args and fields diff length")
  | Cast (cn, t) ->
    let%bind cn' = type_of tbl ctx t in
    if Utils.is_subtype tbl cn' cn || Utils.is_subtype tbl cn cn'
    then return cn (* T-UCast / T-DCast*)
    else (
      (* T-SCast *)
      print_endline "warning: stupid cast";
      return cn)
;;

(** Check that method performs a valid override if it does (Fig 19.2) *)
let override
      (tbl : class_decl list)
      (md : method_decl)
      (cl : class_decl)
      (t_cn : class_name)
      (fields : (class_name * field_name) list)
  : bool
  =
  let rec find_super_method (cn : class_name) =
    if Utils.is_object cn
    then None
    else (
      (* NOTE: If we can't find the class, we would have failed [typecheck_method]
         before we even get to this point *)
      match Utils.find_class tbl cn with
      | Error _ -> None
      | Ok cl ->
        let is_method m = equal_method_name m.method_name md.method_name in
        (match List.find cl.methods ~f:is_method with
         | Some md -> Some (List.map ~f:fst md.fields, fst md.term)
         | None -> find_super_method cl.superclass_name))
  in
  match find_super_method cl.superclass_name with
  | None -> true
  | Some (fields', t_cn') ->
    equal_class_name t_cn t_cn'
    && List.equal equal_class_name (List.map ~f:fst fields) fields'
;;

(** M OK in C *)
let typecheck_method (tbl : class_decl list) (cl : class_decl) (md : method_decl)
  : unit Or_error.t
  =
  let { method_name = _; fields; term = t_cn, t } = md in
  let%bind ctx =
    fields
    |> List.map ~f:(fun (cn, field) -> string_of_field_name field, cn)
    |> List.cons ("this", cl.class_name)
    |> String.Map.of_alist_or_error
  in
  let%bind t_cn' = type_of tbl ctx t in
  if Utils.is_subtype tbl t_cn' t_cn
  then
    if override tbl md cl t_cn fields
    then return ()
    else Or_error.error_string "invalid override"
  else Or_error.error_string "expr is not expected return type of method"
;;

(** C OK *)
let typecheck_class (tbl : class_decl list) (cl : class_decl) : unit Or_error.t =
  Or_error.all_unit (List.map cl.methods ~f:(typecheck_method tbl cl))
;;

let typecheck (Program (tbl, t)) =
  let%bind () = Or_error.all_unit (List.map ~f:(typecheck_class tbl) tbl) in
  type_of tbl String.Map.empty t
;;
