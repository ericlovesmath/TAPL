open Core

type pos =
  { line : int
  ; col : int
  }
[@@deriving sexp]

type stream = (char * pos) Sequence.t
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

let satisfy (pred : char -> bool) : char t =
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
let char_p c = satisfy (Char.equal c)
let string_p st = all (List.map ~f:char_p (String.to_list st))
let alpha_p = many1 (satisfy Char.is_alpha)
let numeric_p = many1 (satisfy Char.is_digit)
let empty_p = char_p ' ' <|> char_p '\n' <|> char_p '\t'
let strip p = ignore_m (many empty_p) *> p <* ignore_m (many empty_p)
let spaces_p = many1 empty_p

let run p s =
  let advance c pos =
    if Char.equal c '\n'
    then { line = pos.line + 1; col = 1 }
    else { line = pos.line; col = pos.col + 1 }
  in
  let stream_of_string s =
    Sequence.unfold_step
      ~init:(`Chars (String.to_list s, { line = 1; col = 1 }))
      ~f:(function
        | `Done -> Done
        | `Chars ([], _) -> Done
        | `Chars (c :: cs, pos) ->
          Yield { value = c, pos; state = `Chars (cs, advance c pos) })
  in
  let%bind.Or_error x, st = p (stream_of_string s) in
  match Sequence.next st with
  | Some ((_, pos), _) -> fail ~pos "run: stream not fully consumed"
  | None -> Ok x
;;

let%expect_test "tests" =
  let integer_p =
    let%bind num =
      numeric_p
      <|> (List.cons <$> char_p '-' <*> numeric_p)
      <|> (List.cons <$> char_p '+' <*> numeric_p)
    in
    return (int_of_string (String.of_list num))
  in
  let two_nums_p =
    let%bind n = integer_p in
    let%bind _ = spaces_p in
    let%bind m = integer_p in
    return (n, m)
  in
  let x = run two_nums_p "12 -31" in
  print_s [%message (x : (int * int) Or_error.t)];
  [%expect {| (x (Ok (12 -31))) |}];
  let x = run two_nums_p "12 -a1" in
  print_s [%message (x : (int * int) Or_error.t)];
  [%expect {| (x (Error ((pos ((line 1) (col 4))) "satisfy: pred not satisfied"))) |}];
  let x = run (string_p "abcde") "abcde  " in
  print_s [%message (x : char list Or_error.t)];
  [%expect {| (x (Error ((pos ((line 1) (col 6))) "run: stream not fully consumed"))) |}]
;;
