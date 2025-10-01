(** Simplest version of beta reduction on an untyped lambda calculus *)
type t = Var of string | Lam of string * t | App of t * t [@@deriving sexp]

(** Call-by-name beta reduction *)
val beta : t -> t

(** Use [open Syntax] to write [t] terms with shorter syntax *)
module Syntax : sig
  val v : string -> t
  (** Shorthand for [Var v] *)

  val ( > ) : string -> t -> t
  (** Shorthand for [Lam (v, t)] *)

  val ( $ ) : t -> t -> t
  (** Shorthand for [App (f, x)] *)
end
