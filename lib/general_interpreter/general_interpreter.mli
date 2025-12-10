type t =
  | UUnit
  | UTrue
  | UFalse
  | UTuple of t list
  | UProjTuple of t * int
  | URecord of (string * t) list
  | UProjRecord of t * string
  | UVariant of string * t
  | UMatch of t * (string * t) list
  | USeq of t * t
  | UIf of t * t * t
  | UVar of int
  | UAbs of t
  | UApp of t * t
  | UZero
  | USucc of t
  | UPred of t
  | UIsZero of t
  | UFix of t
  | URef of t
  | ULoc of int
  | UDeref of t
  | UAssign of int * t
  | UError
[@@deriving equal, sexp_of]

val eval : t -> t
