module Eval = Eval
module Typecheck = Typecheck
include module type of Types

(* Reads and parses [string] input, prints result *)
val repl : string -> unit
