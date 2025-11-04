open Core
open Types
module Lexer = Chomp.Lexer
open Lexer
module Parser = Chomp.Make (Chomp.Lexer)
open Parser
open Parser.Let_syntax
open Parser.Infix_syntax

let ident_p : string t =
  satisfy_map (function
    | ID s -> Some s
    | _ -> None)
;;

let between brace_type p =
  let l, r =
    match brace_type with
    | `Paren -> LPAREN, RPAREN
    | `Curly -> LCURLY, RCURLY
    | `Angle -> LANGLE, RANGLE
  in
  tok l *> p <* tok r
;;

let commas p = sep_by1 (tok COMMA) p

let rec ty_p =
  fun st ->
  let ty_p = ty_arrow_p <|> ty_atom_p in
  (ty_p <|> between `Paren ty_p) st

and ty_atom_p =
  fun st -> (ty_singles_p <|> ty_tuple_p <|> ty_record_p <|> ty_variant_p <|> ty_ref_p) st

and ty_singles_p =
  satisfy_map (function
    | UNITTY -> Some TyUnit
    | BOOL -> Some TyBool
    | NAT -> Some TyNat
    | BASE c -> Some (TyBase c)
    | _ -> None)

and ty_tuple_p =
  fun st ->
  (let%map tys = between `Curly (commas ty_p) in
   TyTuple tys)
    st

and field_p =
  fun st ->
  (let%bind l = ident_p in
   let%bind _ = tok COLON in
   let%bind r = ty_p in
   return (l, r))
    st

and ty_record_p =
  fun st ->
  (let%map fields = between `Curly (commas field_p) in
   TyRecord fields)
    st

and ty_variant_p =
  fun st ->
  let unit_field_p =
    let%map l = ident_p in
    l, TyUnit
  in
  let record_p =
    let%map fields = field_p <|> unit_field_p |> commas |> between `Angle in
    TyVariant fields
  in
  record_p st

and ty_arrow_p =
  fun st ->
  (let%bind l = ty_atom_p <|> between `Paren ty_p in
   let%bind _ = tok ARROW in
   let%bind r = ty_p in
   return (TyArrow (l, r)))
    st

and ty_ref_p = fun st -> (tok REF *> ty_p) st

let%expect_test "ty parse tests" =
  let test s =
    s
    |> Chomp.Lexer.of_string
    |> Chomp.Lexer.lex
    |> run ty_p
    |> Or_error.sexp_of_t sexp_of_ty
    |> print_s
  in
  test "A";
  test "bool";
  test "((  nat) )";
  test "nat -> unit";
  test "A -> X -> nat -> bool";
  test "(A -> X) -> nat -> bool";
  test "A -> (X -> nat) -> bool";
  test "(A -> X) -> (nat -> bool)";
  test "{  nat  , nat  }";
  test "{bool,{unit->bool->bool,nat} -> {nat,nat}}";
  test "{ x : nat , y : { b :bool} }";
  test "< some : nat, none >";
  [%expect
    {|
    (Ok A)
    (Ok bool)
    (Error ((pos ((i 1) (line 1) (col 2))) "satisfy: pred not satisfied"))
    (Ok (nat -> unit))
    (Ok (A -> (X -> (nat -> bool))))
    (Ok ((A -> X) -> (nat -> bool)))
    (Ok (A -> ((X -> nat) -> bool)))
    (Ok ((A -> X) -> (nat -> bool)))
    (Ok ({ nat , nat }))
    (Ok ({ bool , (({ (unit -> (bool -> bool)) , nat }) -> ({ nat , nat })) }))
    (Ok ({ x : nat , y : ({ b : bool }) }))
    (Ok (< some : nat , none >))
    |}];
  test "";
  test "{}";
  test "{ a , b : bool }";
  test "<>";
  test "()";
  [%expect
    {|
    (Error "satisfy: EOF")
    (Error ((pos ((i 0) (line 1) (col 1))) "satisfy: pred not satisfied"))
    (Error ((pos ((i 0) (line 1) (col 1))) "satisfy: pred not satisfied"))
    (Error ((pos ((i 0) (line 1) (col 1))) "satisfy: pred not satisfied"))
    (Error ((pos ((i 1) (line 1) (col 2))) "satisfy: pred not satisfied"))
    |}]
;;

let rec t_p =
  fun st ->
  (let%bind t = t_atom_p <|> between `Paren t_p in
   t_proj_p t <|> t_seq_p t <|> t_as_p t <|> t_app_p t <|> return t)
    st

and t_atom_p =
  fun st ->
  let t_singles_p =
    satisfy_map (function
      | UNIT -> Some EUnit
      | TRUE -> Some ETrue
      | FALSE -> Some EFalse
      | ID v -> Some (EVar v)
      | ZERO -> Some EZero
      | _ -> None)
  in
  let t_commit_prefix_p =
    match%bind peek with
    | LET -> t_let_p
    | LETREC -> t_letrec_p
    | LCURLY -> t_tuple_p <|> t_record_p
    | LANGLE -> t_variant_p
    | MATCH -> t_match_p
    | IF -> t_if_p
    | SUCC -> t_succ_p
    | PRED -> t_pred_p
    | ISZERO -> t_iszero_p
    | FIX -> t_fix_p
    | FUN -> t_abs_p
    | REF -> tok REF *> t_p >>| fun t -> ERef t
    | BANG -> tok BANG *> t_p >>| fun t -> EDeref t
    | _ -> fail "commit: not a fixed prefix"
  in
  (t_assign_p <|> t_singles_p <|> t_commit_prefix_p) st

and t_assign_p =
  fun st ->
  (let%bind v = ident_p in
   let%bind t = tok ASSIGN *> t_p in
   return (EAssign (v, t)))
    st

and t_let_p =
  fun st ->
  (let%bind id = tok LET *> ident_p in
   let%bind bind = tok EQ *> t_p in
   let%bind body = tok IN *> t_p in
   return (ELet (id, bind, body)))
    st

and t_letrec_p =
  fun st ->
  (let%bind id = tok LETREC *> ident_p in
   let%bind ty = tok COLON *> ty_p in
   let%bind bind = tok EQ *> t_p in
   let%bind body = tok IN *> t_p in
   return (ELet (id, EFix (EAbs (id, ty, bind)), body)))
    st

and t_tuple_p =
  fun st ->
  (let%map ts = between `Curly (commas t_p) in
   ETuple ts)
    st

and t_proj_p t =
  let%bind _ = tok DOT in
  (let%map i =
     satisfy_map (function
       | INT i -> Some i
       | _ -> None)
   in
   EProjTuple (t, i))
  <|>
  let%map l = ident_p in
  EProjRecord (t, l)

and t_record_p =
  fun st ->
  let field_p =
    let%bind l = ident_p in
    let%bind r = tok EQ *> t_p in
    return (l, r)
  in
  (let%map fields = between `Curly (commas field_p) in
   ERecord fields)
    st

and t_variant_p =
  fun st ->
  (let variant_p =
     let%bind label = ident_p in
     let%bind value = t_p <|> return EUnit in
     return (label, value)
   in
   let%bind label, value = between `Angle variant_p in
   return (EVariant (label, value)))
    st

and t_seq_p t =
  match%map tok SEMI *> t_p with
  | ESeq (t', t'') -> ESeq (ESeq (t, t'), t'')
  | t' -> ESeq (t, t')

and t_match_p =
  fun st ->
  let case_p =
    let%bind label = tok BAR *> ident_p in
    let%bind v = ident_p <|> return "$_" in
    let%bind body = tok ARROW *> t_p in
    return (label, v, body)
  in
  (let%bind t = tok MATCH *> t_p in
   let%bind cases = tok WITH *> many case_p in
   return (EMatch (t, cases)))
    st

and t_if_p =
  fun st ->
  (let%bind c = tok IF *> t_p in
   let%bind t = tok THEN *> t_p in
   let%bind f = tok ELSE *> t_p <|> return EUnit in
   return (EIf (c, t, f)))
    st

and t_app_p t =
  let%bind ts = many (t_atom_p <|> between `Paren t_p) in
  return (List.fold_left ~f:(fun f x -> EApp (f, x)) ~init:t ts)

and t_as_p t =
  let%map ty = tok AS *> ty_p in
  EAs (t, ty)

and t_succ_p = fun st -> (tok SUCC *> t_p >>| fun t -> ESucc t) st
and t_pred_p = fun st -> (tok PRED *> t_p >>| fun t -> EPred t) st
and t_iszero_p = fun st -> (tok ISZERO *> t_p >>| fun t -> EIsZero t) st
and t_fix_p = fun st -> (tok FIX *> t_p >>| fun t -> EFix t) st

and t_abs_p =
  fun st ->
  (let%bind id = tok FUN *> ident_p in
   let%bind ty = tok COLON *> ty_p in
   let%bind t = tok ARROW *> t_p in
   return (EAbs (id, ty, t)))
    st
;;

let%expect_test "t parse tests" =
  let test s =
    s
    |> Chomp.Lexer.of_string
    |> Chomp.Lexer.lex
    |> run t_p
    |> Or_error.sexp_of_t sexp_of_t
    |> print_s
  in
  test "#u";
  test "if #f then #u else #f";
  test "if #f then #u";
  test "if if #u then #f else #t then (if #t then #f) else #f";
  test "let x = if #f then #f   in    #t";
  test "letrec x : bool = x in #t";
  [%expect
    {|
    (Ok #u)
    (Ok (if #f then #u else #f))
    (Ok (if #f then #u))
    (Ok (if (if #u then #f else #t) then (if #t then #f) else #f))
    (Ok (let x = (if #f then #f) in #t))
    (Ok (let x = (fix (fun x : bool -> x)) in #t))
    |}];
  test "{ {#t,if #t then b} , #f,#t}";
  test "v.0";
  test "{ #t , #f,#t}.0";
  test "{ #t , #f,#t}.22";
  test "{ x = #t , y = v.0 }.x";
  test "< some x >";
  test "< none >";
  [%expect
    {|
    (Ok ({ ({ #t , (if #t then b) }) , #f , #t }))
    (Ok (v . 0))
    (Ok (({ #t , #f , #t }) . 0))
    (Ok (({ #t , #f , #t }) . 22))
    (Ok (({ x : #t , y : (v . 0) }) . x))
    (Ok (< some : x >))
    (Ok (< none : #u >))
    |}];
  test "let x = a; b; c in #t; #f";
  test "let x = a ; b ; c in #t; #f";
  test
    {|
    match x with
    | some x -> #t
    | none -> #f
    |};
  test "f x y z";
  test "f (x y) z";
  test "match f (x y) z with | some x -> #t | none -> #f";
  test "x as bool";
  test "match pos with | p n -> n | end -> #u";
  [%expect
    {|
    (Ok (let x = ((a ";" b) ";" c) in (#t ";" #f)))
    (Ok (let x = ((a ";" b) ";" c) in (#t ";" #f)))
    (Ok (match x with (some x -> #t) (none $_ -> #f)))
    (Ok (((f x) y) z))
    (Ok ((f (x y)) z))
    (Ok (match ((f (x y)) z) with (some x -> #t) (none $_ -> #f)))
    (Ok (x as bool))
    (Ok (match pos with (p n -> n) (end $_ -> #u)))
    |}];
  test "Z Z Z Z";
  test "iszero (pred (S (S Z)))";
  test "fix (S Z)";
  test "fun x : bool -> x";
  [%expect
    {|
    (Ok (((Z Z) Z) Z))
    (Ok (iszero (pred (S (S Z)))))
    (Ok (fix (S Z)))
    (Ok (fun x : bool -> x))
    |}];
  test "(v := S Z); let x = ref v in !x";
  [%expect {| (Ok ((v := (S Z)) ";" (let x = (ref v) in (! x)))) |}]
;;
