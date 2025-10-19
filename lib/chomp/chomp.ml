open Core
include Chomp_intf

module Make (M : Parsable) : Parser with type token = M.token and type pos = M.pos =
struct
  include M

  type stream = (token * pos) Sequence.t
  type 'a t = stream -> ('a * stream) Or_error.t

  include Applicative.Make (struct
      type nonrec 'a t = 'a t

      let apply pf px st =
        let open Or_error.Let_syntax in
        let%bind f, st' = pf st in
        let%bind x, st'' = px st' in
        Ok (f x, st'')
      ;;

      let return x st = Ok (x, st)
      let map = `Define_using_apply
    end)

  include Monad.Make (struct
      type nonrec 'a t = 'a t

      let bind p ~f =
        fun st ->
        let%bind.Or_error a, st' = p st in
        f a st'
      ;;

      let return = return
      let map = `Define_using_bind
    end)

  module Infix_syntax = struct
    include Applicative_infix

    let ( <$> ) f x = map ~f x
    let ( <$ ) f p = Fun.const <$> return f <*> p
    let ( $> ) p f = f <$ p
    let ( *> ) i p = ignore_m i *> p
    let ( <* ) p i = p <* ignore_m i

    let ( <*>| ) pf px =
      let open Or_error.Let_syntax in
      fun st ->
        let%bind f, st' = pf st in
        let%bind x, st'' = (Lazy.force px) st' in
        Ok (f x, st'')
    ;;

    let ( <|> ) p p' =
      fun st ->
      match p st with
      | Error _ -> p' st
      | Ok res -> Ok res
    ;;
  end

  open Let_syntax
  open Infix_syntax

  let fail ~pos msg = error_s [%message (pos : pos) msg]

  let satisfy (pred : token -> bool) : token t =
    fun st ->
    match Sequence.next st with
    | Some ((c, pos), st') ->
      if pred c then Ok (c, st') else fail ~pos "satisfy: pred not satisfied"
    | None -> Or_error.error_string "satisfy: EOF"
  ;;

  let rec many1 p = List.cons <$> p <*>| lazy (many p)
  and many p = many1 p <|> return []

  let sep_by1 sep p = List.cons <$> p <*> many (sep *> p)
  let sep_by sep p = sep_by1 sep p <|> return []

  let run p s =
    let%bind.Or_error x, st = p (Sequence.of_list s) in
    match Sequence.next st with
    | Some ((_, pos), _) -> fail ~pos "run: stream not fully consumed"
    | None -> Ok x
  ;;

  let tok t = satisfy (equal_token t)

  (* TODO: Implement this like a MonadFail instance *)
  let fail msg = Fn.const (Or_error.error_string msg)
end
