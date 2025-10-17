open Core

type stream
type 'a t = stream -> ('a * stream) Or_error.t

(** Run parser [p] on string [s], ensuring the entire input is consumed.
    Error messages state line/col of error *)
val run : 'a t -> string -> 'a Or_error.t

include Applicative.S with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

module Infix_syntax : sig
  include Applicative.Applicative_infix with type 'a t := 'a t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  (* TODO: I don't think the Functor infix is correct *)
  val ( <$ ) : 'a -> 'b t -> 'a t
  val ( $> ) : 'a t -> 'a -> 'a t

  (* NOTE: [Applicative_infix] requires usage of [ignore_m], which is cumbersome.
    This replaces [*>] and [<*] to use Haskell-like syntax, implicitly ignoring *)
  val ( *> ) : 'ignore t -> 'a t -> 'a t
  val ( <* ) : 'a t -> 'ignore t -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( <*>| ) : ('a -> 'b) t -> 'a t Lazy.t -> 'b t
end

val satisfy : (char -> bool) -> char t
val many1 : 'a t -> 'a list t
val many : 'a t -> 'a list t
val sep_by1 : 'a t -> 'b t -> 'b list t
val sep_by : 'a t -> 'b t -> 'b list t
val char_p : char -> char t
val string_p : string -> char list t
val alpha_p : char list t
val numeric_p : char list t
val empty_p : char t
val strip : 'a t -> 'a t
val spaces_p : char list t
val between : [ `Angle | `Bracket | `Curly | `Paren ] -> 'a t -> 'a t
