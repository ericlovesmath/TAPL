open Core
open Sexplib.Sexp

type class_name = string [@@deriving string]
type field = string [@@deriving string]
type method_name = string [@@deriving string]

type t =
  | Var of string
  | FieldAccesss of t * field
  | InvokeMethod of t * method_name * t list
  | CreateObject of class_name * t list
  | Cast of class_name * t

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

let rec to_string = function
  | Var v -> v
  | FieldAccesss (t, v) ->
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
