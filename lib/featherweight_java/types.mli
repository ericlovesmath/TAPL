type class_name [@@deriving string, sexp_of, equal]
type field_name [@@deriving string, sexp_of, equal]
type method_name [@@deriving string, sexp_of, equal]

type t =
  | Var of string
  | FieldAccess of t * field_name
  | InvokeMethod of t * method_name * t list
  | CreateObject of class_name * t list
  | Cast of class_name * t
[@@deriving to_string, sexp_of]

type method_decl =
  { method_name : method_name
  ; fields : (class_name * field_name) list
  ; term : class_name * t
  }
[@@deriving sexp_of]

type class_decl =
  { class_name : class_name
  ; superclass_name : class_name
  ; params : (class_name * field_name) list
  ; fields_to_super : field_name list
  ; methods : method_decl list
  }
[@@deriving sexp_of]

type program = Program of class_decl list * t [@@deriving sexp_of]
