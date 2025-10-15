open Core

type 'a t = char Sequence.t -> ('a * char Sequence.t) Or_error.t

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

let fail : 'a t = fun _ -> error_s [%message "fail"]

let satisfy (pred : char -> bool) : char t =
  fun st ->
  match Sequence.next st with
  | Some (c, st') when pred c -> Ok (c, st')
  | Some _ -> Or_error.error_string "satisfy fail invalid"
  | None -> Or_error.error_string "satisfy fail EOF"
;;

let rec many1 p = List.cons <$> p <*>| lazy (many p)
and many p = many1 p <|> return []

let choice = List.fold_right ~f:( <|> ) ~init:fail
let sepBy1 sep p = List.cons <$> p <*> many (sep *> p)
let sepBy sep p = sepBy1 sep p <|> return []
let charP c = satisfy (Char.equal c)
let stringP st = all (List.map ~f:charP (String.to_list st))
let alphaP = many1 (satisfy Char.is_alpha)
let numericP = many1 (satisfy Char.is_digit)
let emptyP = charP ' ' <|> charP '\n' <|> charP '\t'
let strip p = ignore_m (many emptyP) *> p <* ignore_m (many emptyP)
let spacesP = many1 emptyP

let%expect_test "check" =
  let integerP =
    let%bind num =
      numericP
      <|> (List.cons <$> charP '-' <*> numericP)
      <|> (List.cons <$> charP '+' <*> numericP)
    in
    return (int_of_string (String.of_list num))
  in
  let dosP =
    let%bind n = integerP in
    let%bind _ = spacesP in
    let%bind m = integerP in
    return (n, m)
  in
  let x = dosP (String.to_sequence "12 -31") in
  print_s [%message (x : ((int * int) * char Sequence.t) Or_error.t)];
  [%expect {| test |}];
  let x = stringP "abcde" (String.to_sequence "abcde") in
  print_s [%message (x : (char list * char Sequence.t) Or_error.t)];
  [%expect {| test |}]
;;
