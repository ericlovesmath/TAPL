open Core
open Types

let is_object = equal_class_name (class_name_of_string "Object")

let find_class (tbl : class_decl list) (c : class_name) =
  List.find tbl ~f:(fun cl -> equal_class_name cl.class_name c)
  |> Or_error.of_option ~error:(Error.of_string "List.find failed")
;;

let find_method (cl : class_decl) (m : method_name) =
  List.find cl.methods ~f:(fun md -> equal_method_name md.method_name m)
  |> Or_error.of_option ~error:(Error.of_string "List.find failed")
;;

let rec is_subtype (tbl : class_decl list) (c : class_name) (c' : class_name) =
  if is_object c' || equal_class_name c c'
  then true
  else if is_object c
  then false
  else (
    match find_class tbl c with
    | Ok cl when equal_class_name cl.superclass_name c' -> true
    | Ok cl -> is_subtype tbl cl.superclass_name c'
    | Error _ -> false)
;;

let rec fields (tbl : class_decl list) (c : class_name) =
  let open Or_error.Let_syntax in
  if is_object c
  then return []
  else (
    let%bind cl = find_class tbl c in
    let%bind params = fields tbl cl.superclass_name in
    return (cl.params @ params))
;;
