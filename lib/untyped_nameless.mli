type t =
  | Var of int
  | Abs of t
  | App of t * t
[@@deriving sexp]

type context = string list

(** [Exercise 6.1.5] Converts to DeBruijn indices *)
val remove_names : context -> Untyped_lambda_calculus.t -> t option

(** [Exercise 6.1.5] Converts from DeBruijn indices *)
val restore_names : context -> t -> Untyped_lambda_calculus.t option

(** Use [open Syntax] to write [t] terms with shorter syntax *)
module Syntax : sig
  (** Shorthand for [Var v] *)
  val v : int -> t

  (** Shorthand for [Abs t] *)
  val h : t -> t

  (** Shorthand for [App (f, x)] *)
  val ( $ ) : t -> t -> t
end
