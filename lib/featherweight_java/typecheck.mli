open Core
open Types

val typecheck : program -> class_name Or_error.t
