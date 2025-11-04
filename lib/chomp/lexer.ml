open Core

type pos =
  { i : int
  ; line : int
  ; col : int
  }
[@@deriving sexp]

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
  | SEMI
  | COLON
  | COMMA
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | FUN
  | AS
  | BAR
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
  | INT of int
  | BASE of char
  | ID of string
[@@deriving sexp, equal]

type t =
  { str : string
  ; len : int
  ; mutable pos : pos
  }

let of_string s = { str = s; len = String.length s; pos = { i = 0; line = 1; col = 1 } }
let eof t = t.pos.i = t.len

let advance c t =
  t.pos
  <- (if Char.equal c '\n'
      then { i = t.pos.i + 1; line = t.pos.line + 1; col = 1 }
      else { i = t.pos.i + 1; line = t.pos.line; col = t.pos.col + 1 })
;;

let peek t = if eof t then failwith "peek: EOF" else String.get t.str t.pos.i
let skip t = advance (peek t) t

let next t =
  let c = peek t in
  skip t;
  c
;;

let rec strip t =
  if (not (eof t)) && Char.is_whitespace (peek t)
  then (
    skip t;
    strip t)
;;

let read_while f t =
  let rec go () =
    if eof t
    then []
    else (
      let c = peek t in
      if f c
      then (
        skip t;
        c :: go ())
      else [])
  in
  String.of_list (go ())
;;

let read_lexeme t =
  let pos = t.pos in
  let token =
    match peek t with
    | '(' -> skip t; LPAREN
    | '=' -> skip t; EQ
    | ')' -> skip t; RPAREN
    | '{' -> skip t; LCURLY
    | '}' -> skip t; RCURLY
    | '<' -> skip t; LANGLE
    | '>' -> skip t; RANGLE
    | ';' -> skip t; SEMI
    | ',' -> skip t; COMMA
    | '.' -> skip t; DOT
    | '|' -> skip t; BAR
    | '!' -> skip t; BANG
    | '#' -> (
        skip t;
        match next t with
        | 'u' -> UNIT
        | 't' -> TRUE
        | 'f' -> FALSE
        | _ -> failwith "invalid token starting with #")
    | c when Char.is_digit c ->
        INT (Int.of_string (read_while Char.is_digit t))
    | c when Char.is_alpha c -> (
        let s = read_while (fun c -> Char.is_alpha c) t in
        match s with
        | "unit" -> UNITTY
        | "Z" -> ZERO
        | "S" -> SUCC
        | "pred" -> PRED
        | "iszero" -> ISZERO
        | "fix" -> FIX
        | "let" -> LET
        | "letrec" -> LETREC
        | "in" -> IN
        | "if" -> IF
        | "then" -> THEN
        | "else" -> ELSE
        | "fun" -> FUN
        | "as" -> AS
        | "match" -> MATCH
        | "with" -> WITH
        | "bool" -> BOOL
        | "nat" -> NAT
        | "ref" -> REF
        | "top" -> TOP
        | "bot" -> BOT
        | _ ->
            if String.length s = 1 && Char.(is_uppercase (of_string s))
            then BASE (Char.of_string s)
            else ID s)
    | _ ->
        let s = read_while (fun c -> not Char.(is_alpha c || equal '#' c || is_whitespace c || equal '.' c)) t in
        match s with
        | ":" -> COLON
        | ":=" -> ASSIGN
        | "->" -> ARROW
        | _ -> failwith ("invalid token " ^ s)
    in
    strip t;
    (token, pos)
  [@@ocamlformat "disable"]

let lex t =
  let rec aux acc = if eof t then List.rev acc else aux (read_lexeme t :: acc) in
  strip t;
  aux []
;;

let%expect_test "lexer" =
  let test s =
    s |> of_string |> lex |> List.map ~f:fst |> List.sexp_of_t sexp_of_token |> print_s
  in
  test ".";
  test "#u";
  test "bool";
  test "->";
  test "abcd";
  test "if #f then #u else #f";
  test "if #f then #u";
  test "if    if #u then #f   else #t then(if #t then #f)else   #f";
  test "f x y z";
  test "f (x y) z";
  test "match f (x y) z with | some x => #t | none => #f";
  test "x as bool";
  test "match pos as < p : nat , end > with | p n -> n | end -> #u";
  test "match pos as<p:nat,end>with|p n->n|end->#u";
  test "{ x : nat , y : { bool:bool}}";
  test "< some : nat, none >";
  test "{ x = #t , y = v.0 }.x";
  test "v := S; let x = ref v in !x";
  [%expect
    {|
    (DOT)
    (UNIT)
    (BOOL)
    (ARROW)
    ((ID abcd))
    (IF FALSE THEN UNIT ELSE FALSE)
    (IF FALSE THEN UNIT)
    (IF IF UNIT THEN FALSE ELSE TRUE THEN LPAREN IF TRUE THEN FALSE RPAREN ELSE
     FALSE)
    ((ID f) (ID x) (ID y) (ID z))
    ((ID f) LPAREN (ID x) (ID y) RPAREN (ID z))
    (MATCH (ID f) LPAREN (ID x) (ID y) RPAREN (ID z) WITH BAR (ID some) (ID x) EQ
     RANGLE TRUE BAR (ID none) EQ RANGLE FALSE)
    ((ID x) AS BOOL)
    (MATCH (ID pos) AS LANGLE (ID p) COLON NAT COMMA (ID end) RANGLE WITH BAR
     (ID p) (ID n) ARROW (ID n) BAR (ID end) ARROW UNIT)
    (MATCH (ID pos) AS LANGLE (ID p) COLON NAT COMMA (ID end) RANGLE WITH BAR
     (ID p) (ID n) ARROW (ID n) BAR (ID end) ARROW UNIT)
    (LCURLY (ID x) COLON NAT COMMA (ID y) COLON LCURLY BOOL COLON BOOL RCURLY
     RCURLY)
    (LANGLE (ID some) COLON NAT COMMA (ID none) RANGLE)
    (LCURLY (ID x) EQ TRUE COMMA (ID y) EQ (ID v) DOT (INT 0) RCURLY DOT (ID x))
    ((ID v) ASSIGN SUCC SEMI LET (ID x) EQ REF (ID v) IN BANG (ID x))
    |}]
;;
