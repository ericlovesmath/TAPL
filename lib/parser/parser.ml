open Core

let explode (s : string) : char list = Sequence.to_list (String.to_sequence s)
let implode (st : char list) : string = String.of_sequence (Sequence.of_list st)

(* TODO: Parse Seq.t instead *)
type 'a parser = char list -> ('a * char list) option

(* TODO: Document these Haskell-like functions *)
let pure (x : 'a) : 'a parser = fun st -> Some (x, st)
let fail : 'a parser = fun _ -> None

let ( >>= ) (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
  fun st ->
  match p st with
  | None -> None
  | Some (a, st') -> f a st'
;;

let ( let* ) = ( >>= )

let ( <*> ) (pf : ('a -> 'b) parser) (px : 'a parser) : 'b parser =
  let open Option.Let_syntax in
  fun st ->
    let%bind f, st' = pf st in
    let%bind x, st'' = px st' in
    Some (f x, st'')
;;

let ( <*>| ) (pf : ('a -> 'b) parser) (px : 'a parser lazy_t) : 'b parser =
  let open Option.Let_syntax in
  fun st ->
    let%bind f, st' = pf st in
    let%bind x, st'' = (Lazy.force px) st' in
    Some (f x, st'')
;;

let ( <$> ) (f : 'a -> 'b) (p : 'a parser) : 'b parser =
  fun st ->
  match p st with
  | None -> None
  | Some (a, st') -> Some (f a, st')
;;

let ( <|> ) (p : 'a parser) (p' : 'a parser) : 'a parser =
  fun st ->
  match p st with
  | None -> p' st
  | Some res -> Some res
;;

let ( <$ ) f p = Fun.const <$> pure f <*> p
let ( $> ) p f = f <$ p
let ( *> ) p q = Fun.id <$ p <*> q
let ( <* ) p q = Fun.const <$> p <*> q

let rec seq = function
  | [] -> pure []
  | hd :: tl -> List.cons <$> hd <*> seq tl
;;

let choice l = List.fold_right ~f:( <|> ) ~init:(Fun.const None) l

let rec many1 p = List.cons <$> p <*>| lazy (many p)
and many p = many1 p <|> pure []

let sepBy1 sep p = List.cons <$> p <*> many (sep *> p)
let sepBy sep p = sepBy1 sep p <|> pure []

let satisfy (pred : char -> bool) : char parser =
  fun st ->
  match st with
  | c :: st' when pred c -> Some (c, st')
  | _ -> None
;;

let charP c = satisfy (Char.equal c)
let stringP st = seq (List.map ~f:charP (explode st))

let alphaP =
  let is_alpha c = Char.O.((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) in
  many1 (satisfy is_alpha)
;;

let numericP =
  let is_numeric c = Char.O.(c >= '0' && c <= '9') in
  many1 (satisfy is_numeric)
;;

let emptyP = charP ' ' <|> charP '\n' <|> charP '\t'
let strip p = many emptyP *> p <* many emptyP
let spacesP = many1 emptyP

let%expect_test "check" =
  print_endline "test";
  [%expect {| test |}]
;;
