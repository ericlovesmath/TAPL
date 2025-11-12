open Simply_typed_extended.Types

(* NOTE: Assumes [t] has already been typechecked, will fail on unbound variables *)
val remove_names : Types.t -> nameless
val eval : nameless -> nameless
