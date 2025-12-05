open Core
include Chomp_intf
module Lexer = Lexer

(* TODO: Fail and Fatal should track location separately *)

module Maybe = struct
  type 'a t =
    | Success of 'a
    | Fail of Error.t
    | Fatal of Error.t

  let to_or_error = function
    | Fail e | Fatal e -> Error e
    | Success x -> Ok x
  ;;

  include Applicative.Make (struct
      type nonrec 'a t = 'a t

      let apply mf mx =
        match mf with
        | Fail e -> Fail e
        | Fatal e -> Fatal e
        | Success f ->
          (match mx with
           | Fail e -> Fail e
           | Fatal e -> Fatal e
           | Success x -> Success (f x))
      ;;

      let return x = Success x
      let map = `Define_using_apply
    end)

  include Monad.Make (struct
      type nonrec 'a t = 'a t

      let bind m ~f =
        match m with
        | Success x -> f x
        | Fail e -> Fail e
        | Fatal e -> Fatal e
      ;;

      let return = return
      let map = `Define_using_bind
    end)
end

module Make (M : Parsable) : Parser with type token = M.token and type pos = M.pos =
struct
  include M
  module Maybe = Maybe
  open Maybe

  type stream = (token * pos) Sequence.t
  type 'a t = stream -> ('a * stream) Maybe.t

  include Applicative.Make (struct
      type nonrec 'a t = 'a t

      let apply pf px st =
        let open Maybe.Let_syntax in
        let%bind f, st' = pf st in
        let%bind x, st'' = px st' in
        Success (f x, st'')
      ;;

      let return x st = Success (x, st)
      let map = `Define_using_apply
    end)

  include Monad.Make (struct
      type nonrec 'a t = 'a t

      let bind p ~f =
        fun st ->
        let%bind.Maybe a, st' = p st in
        f a st'
      ;;

      let return = return
      let map = `Define_using_bind
    end)

  module Infix_syntax = struct
    include Applicative_infix

    let ( <$> ) f x = map ~f x
    let ( <$ ) f p = Fun.const f <$> p
    let ( $> ) p f = f <$ p
    let ( *> ) i p = ignore_m i *> p
    let ( <* ) p i = p <* ignore_m i

    let ( <*>| ) pf px =
      let open Maybe.Let_syntax in
      fun st ->
        let%bind f, st' = pf st in
        let%bind x, st'' = (Lazy.force px) st' in
        Success (f x, st'')
    ;;

    let ( <|> ) p p' =
      fun st ->
      match p st with
      | Success res -> Success res
      | Fatal e -> Fatal e
      | Fail _ ->
        (match p' st with
         | Success res -> Success res
         | Fatal e' -> Fatal e'
         | Fail e' -> Fail e')
    ;;

    let ( <?> ) p tag =
      fun st ->
      match p st with
      | Success res -> Success res
      | Fatal _ -> Fatal (Error.of_string tag)
      | Fail _ -> Fail (Error.of_string tag)
    ;;

    let ( <??> ) p tag =
      fun st ->
      match p st with
      | Success res -> Success res
      | Fatal e -> Fatal (Error.tag e ~tag)
      | Fail e -> Fail (Error.tag e ~tag)
    ;;
  end

  open Let_syntax
  open Infix_syntax

  (* TODO: Add back pos *)
  let fail ~pos msg =
    ignore pos;
    Fail (Error.of_lazy_sexp (lazy [%message msg]))
  ;;

  let commit p =
    fun st ->
    match p st with
    | Success v -> Success v
    | Fail e | Fatal e -> Fatal e
  ;;

  (* TODO: Rewrite below with <?> and <??> syntax *)

  let satisfy (pred : token -> bool) : token t =
    fun st ->
    match Sequence.next st with
    | Some ((c, pos), st') -> if pred c then Success (c, st') else fail ~pos "satisfy"
    | None -> Fail (Error.of_string "satisfy: EOF")
  ;;

  let satisfy_map (pred : token -> 'a option) : 'a t =
    fun st ->
    match Sequence.next st with
    | None -> Fail (Error.of_string "satisfy_map: EOF")
    | Some ((c, pos), st') ->
      (match pred c with
       | Some c -> Success (c, st')
       | None -> fail ~pos "satisfy_map")
  ;;

  let peek : token t =
    fun st ->
    match Sequence.next st with
    | None -> Fail (Error.of_string "peek: EOF")
    | Some ((c, _), _) -> Success (c, st)
  ;;

  let rec many1 p = List.cons <$> p <*>| lazy (many p)
  and many p = many1 p <|> return []

  let sep_by1 sep p = List.cons <$> p <*> many (sep *> p)
  let sep_by sep p = sep_by1 sep p <|> return []

  let run p s =
    Maybe.to_or_error
      (let%bind.Maybe x, st = p (Sequence.of_list s) in
       match Sequence.next st with
       | Some ((_, pos), _) -> fail ~pos "run: stream not fully consumed"
       | None -> Success x)
  ;;

  let tok t = satisfy (equal_token t)
  let fail msg = Fn.const (Fail (Error.of_string msg))
  let fatal msg = Fn.const (Fatal (Error.of_string msg))
end
