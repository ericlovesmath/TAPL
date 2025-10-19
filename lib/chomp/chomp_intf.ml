open Core

module type Parsable = sig
  type pos [@@deriving sexp]
  type token [@@deriving sexp, equal]
end

module type Parser = sig
  include Parsable

  type stream
  type 'a t = stream -> ('a * stream) Or_error.t

  (** Run parser [p] on string [s], ensuring the entire input is consumed.
    Error messages state line/col of error *)
  val run : 'a t -> (token * pos) list -> 'a Or_error.t

  include Applicative.S with type 'a t := 'a t
  include Monad.S with type 'a t := 'a t

  module Infix_syntax : sig
    include Applicative.Applicative_infix with type 'a t := 'a t

    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val ( <$ ) : 'a -> 'b t -> 'a t
    val ( $> ) : 'a t -> 'a -> 'a t

    (* NOTE: [Applicative_infix] requires usage of [ignore_m], which is cumbersome.
    This replaces [*>] and [<*] to use Haskell-like syntax, implicitly ignoring *)
    val ( *> ) : 'ignore t -> 'a t -> 'a t
    val ( <* ) : 'a t -> 'ignore t -> 'a t
    val ( <|> ) : 'a t -> 'a t -> 'a t
    val ( <*>| ) : ('a -> 'b) t -> 'a t Lazy.t -> 'b t
  end

  val satisfy : (token -> bool) -> token t
  val satisfy_map : (token -> 'a option) -> 'a t
  val peek : token t
  val many1 : 'a t -> 'a list t
  val many : 'a t -> 'a list t
  val sep_by1 : 'a t -> 'b t -> 'b list t
  val sep_by : 'a t -> 'b t -> 'b list t
  val tok : token -> token t
  val fail : string -> 'a t
end

module type Make = functor (M : Parsable) ->
  Parser with type token = M.token and type pos = M.pos

module type Intf = sig
  module type Parser = Parser

  module Make : Make
end
