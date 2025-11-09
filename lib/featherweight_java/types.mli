type class_name [@@deriving string]
type field [@@deriving string]
type method_name [@@deriving string]

type t =
  | Var of string
  | FieldAccesss of t * field
  | InvokeMethod of t * method_name * t list
  | CreateObject of class_name * t list
  | Cast of class_name * t
[@@deriving to_string, sexp_of]

type method_decl =
  { method_name : string
  ; fields : (class_name * field) list
  ; return : t
  }

type class_decl =
  { class_name : class_name
  ; superclass_name : class_name
  ; params : (class_name * field) list
  ; fields_to_super : field list
  ; methods : method_decl list
  }

type program = Program of class_decl list * t
