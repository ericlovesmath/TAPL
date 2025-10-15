open Core

type 'a t

(** Run parser [p] on string [s], ensuring the entire input is consumed.
    Error messages state line/col of error *)
val run : 'a t -> string -> 'a Or_error.t

include Applicative.S with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

module Infix_syntax : sig
  include Applicative.Applicative_infix with type 'a t := 'a t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <$ ) : 'a -> 'b t -> 'a t
  val ( $> ) : 'a t -> 'a -> 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( <*>| ) : ('a -> 'b) t -> 'a t Lazy.t -> 'b t
end

val satisfy : (char -> bool) -> char t
val many1 : 'a t -> 'a list t
val many : 'a t -> 'a list t
val sep_by1 : unit t -> 'a t -> 'a list t
val sep_by : unit t -> 'a t -> 'a list t
val char_p : char -> char t
val string_p : string -> char list t
val alpha_p : char list t
val numeric_p : char list t
val empty_p : char t
val strip : 'a t -> 'a t
val spaces_p : char list t
