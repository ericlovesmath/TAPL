type t
type pos [@@deriving sexp_of]

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
  | LBRACKET
  | RBRACKET
  | SEMI
  | COLON
  | DCOLON
  | COMMA
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | FUN
  | AS
  | BAR
  | DARROW
  | MATCH
  | WITH
  | LCURLY
  | RCURLY
  | BOOL
  | UNITTY
  | NAT
  | BANG
  | REF
  | ASSIGN
  | TOP
  | BOT
  | ERROR
  | CLASS
  | EXTENDS
  | SUPER
  | THIS
  | NEW
  | RETURN
  | REC
  | TICK
  | FORALL
  | STAR
  | EXISTS
  | SUBTYPE
  | INT of int
  | BASE of char
  | ID of string
[@@deriving sexp, equal]

val of_string : string -> t
val lex : t -> (token * pos) list
val initial_pos : pos
