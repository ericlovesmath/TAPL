open Core
include Chomp_intf
module Lexer = Lexer

module Make (M : Parsable) : Parser with type token = M.token and type pos = M.pos =
struct
  include M

  module Maybe = struct
    type error_info =
      { message : string
      ; found : (token * pos) option
      ; contexts : (string * pos option) list
      }

    let sexp_of_error_info { message; found; contexts } =
      let chomp_error =
        match found with
        | None -> message
        | Some (token, pos) ->
          let token = Sexplib.Sexp.to_string_hum (M.sexp_of_token token) in
          let pos = Sexplib.Sexp.to_string_hum (M.sexp_of_pos pos) in
          [%string "%{message} on token %{token} at %{pos}"]
      in
      let string_of_context = function
        | s, None -> s
        | s, Some pos ->
          let pos = Sexp.to_string_hum (M.sexp_of_pos (pos : pos)) in
          [%string "%{s} at %{pos}"]
      in
      let contexts = List.rev (List.map ~f:string_of_context contexts) in
      [%message (chomp_error : string) (contexts : string list)]
    ;;

    type 'a t =
      | Success of 'a
      | Fail of error_info
      | Fatal of error_info

    let to_or_error = function
      | Fail e | Fatal e -> Error (Error.of_lazy_sexp (lazy (sexp_of_error_info e)))
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

  open Maybe

  type 'a maybe = 'a Maybe.t
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

    let pos_of_stream st = Option.map ~f:(fun ((_, pos), _) -> pos) (Sequence.next st)

    let ( <??> ) p tag =
      fun st ->
      match p st with
      | Success res -> Success res
      | Fatal e -> Fatal { e with contexts = (tag, pos_of_stream st) :: e.contexts }
      | Fail e -> Fail { e with contexts = (tag, pos_of_stream st) :: e.contexts }
    ;;
  end

  open Let_syntax
  open Infix_syntax

  let fail ?pos ?tok message =
    Fail { message; found = Option.both tok pos; contexts = [] }
  ;;

  let commit p =
    fun st ->
    match p st with
    | Success v -> Success v
    | Fail e | Fatal e -> Fatal e
  ;;

  let satisfy (pred : token -> bool) : token t =
    fun st ->
    match Sequence.next st with
    | Some ((tok, pos), st') ->
      if pred tok then Success (tok, st') else fail ~pos ~tok "satisfy_fail"
    | None -> fail "satisfy_eof"
  ;;

  let peek : token t =
    fun st ->
    match Sequence.next st with
    | Some ((c, _), _) -> Success (c, st)
    | None -> fail "peek_eof"
  ;;

  let satisfy_map (pred : token -> 'a option) : 'a t =
    fun st ->
    match Sequence.next st with
    | None -> fail "satisfy_map_eof"
    | Some ((c, pos), st') ->
      (match pred c with
       | Some c -> Success (c, st')
       | None -> fail ~pos "satisfy_map_fail")
  ;;

  let rec many1 p = List.cons <$> p <*>| lazy (many p)
  and many p = many1 p <|> return []

  let sep_by1 sep p = List.cons <$> p <*> many (sep *> p)
  let sep_by sep p = sep_by1 sep p <|> return []

  let run p s =
    Maybe.to_or_error
      (let%bind.Maybe x, st = p (Sequence.of_list s) in
       match Sequence.next st with
       | Some ((_, pos), _) -> fail ~pos "run_stream_not_fully_consumed"
       | None -> Success x)
  ;;

  let tok t = satisfy (equal_token t)
  let fail message = Fn.const (Fail { message; found = None; contexts = [] })
  let fatal message = Fn.const (Fatal { message; found = None; contexts = [] })
end
