open Core
open Types
module Unique_id : Unique_id.Id

val typecheck : t -> ty Or_error.t
