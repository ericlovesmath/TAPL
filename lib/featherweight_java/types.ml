open Core
open Sexplib.Sexp

type class_name = string [@@deriving string, sexp_of, equal]
type field_name = string [@@deriving string, sexp_of, equal]
type method_name = string [@@deriving string, sexp_of, equal]

type t =
  | Var of string
  | FieldAccess of t * field_name
  | InvokeMethod of t * method_name * t list
  | CreateObject of class_name * t list
  | Cast of class_name * t

let rec to_string = function
  | Var v -> v
  | FieldAccess (t, v) ->
    let t = to_string t in
    [%string "%{t}.%{v}"]
  | InvokeMethod (t, m, ts) ->
    let t = to_string t in
    let ts = String.concat ~sep:", " (List.map ~f:to_string ts) in
    [%string "%{t}.%{m}(%{ts})"]
  | CreateObject (c, ts) ->
    let ts = String.concat ~sep:", " (List.map ~f:to_string ts) in
    [%string "new %{c}(%{ts})"]
  | Cast (c, t) ->
    let t = to_string t in
    [%string "(%{c}) %{t}"]
;;

let sexp_of_t t = Atom (to_string t)

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
