type t
type pos [@@deriving sexp]

type token =
  | UNIT
  | TRUE
  | FALSE
  | ZERO
  | PRED
  | SUCC
  | ISZERO
  | FIX
  | LETREC
  | EQ
  | ARROW
  | LPAREN
  | RPAREN
  | DOT
  | LANGLE
  | RANGLE
  | SEMI
  | COLON
  | COMMA
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | FUN
  | AS
  | BAR
  | MATCH
  | WITH
  | LCURLY
  | RCURLY
  | BOOL
  | UNITTY
  | NAT
  | INT of int
  | BASE of char
  | ID of string
[@@deriving sexp, equal]

val of_string : string -> t
val lex : t -> (token * pos) list
