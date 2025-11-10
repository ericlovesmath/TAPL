open Types

val is_object : class_name -> bool

(** Lookup class by classname *)
val find_class : class_decl list -> class_name -> class_decl

(** Lookup method in class by name *)
val find_method : class_decl -> method_name -> method_decl

(** Subtype of class *)
val is_subtype : class_decl list -> class_name -> class_name -> bool

(** Look up fields in class, including inherited fields (Fig 19.2) *)
val fields : class_decl list -> class_name -> (class_name * field_name) list
