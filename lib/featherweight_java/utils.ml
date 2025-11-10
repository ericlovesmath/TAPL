open Core
open Types

let is_object = equal_class_name (class_name_of_string "Object")

let find_class (tbl : class_decl list) (c : class_name) =
  List.find_exn tbl ~f:(fun cl -> equal_class_name cl.class_name c)
;;

let find_method (cl : class_decl) (m : method_name) =
  List.find_exn cl.methods ~f:(fun md -> equal_method_name md.method_name m)
;;

let rec is_subtype (tbl : class_decl list) (c : class_name) (c' : class_name) =
  if is_object c' || equal_class_name c c'
  then true
  else if is_object c
  then false
  else (
    let cl = find_class tbl c in
    if equal_class_name cl.superclass_name c'
    then true
    else is_subtype tbl cl.superclass_name c')
;;

let rec fields (tbl : class_decl list) (c : class_name) =
  if is_object c
  then []
  else (
    let cl = find_class tbl c in
    cl.params @ fields tbl cl.superclass_name)
;;
