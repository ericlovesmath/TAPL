open Core
include module type of Types

(* Evaluates type of [t] *)
val type_of : context -> t -> ty Or_error.t

(* Evaluates normalized [t], should not fail on typechecked [t]
   that does not contain closures *)
val eval : t String.Map.t -> t -> t
