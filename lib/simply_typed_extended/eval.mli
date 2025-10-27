open Types

(* NOTE: Assumes [t] has already been typechecked, will fail on unbound variables *)
val remove_names : t -> nameless

val eval : nameless -> nameless
