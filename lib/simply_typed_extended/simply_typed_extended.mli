open Core
include module type of Types

val type_of : context -> t -> ty Or_error.t
