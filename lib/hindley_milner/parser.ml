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

let rec ty_p =
  fun st ->
  let ty_p = ty_arrow_p <|> ty_atom_p in
  (ty_p <|> between `Paren ty_p) st

and ty_atom_p = fun st -> (ty_singles_p <|> ty_ref_p <|> ty_var_p) st

and ty_singles_p =
  satisfy_map (function
    | UNITTY -> Some TyUnit
    | BOOL -> Some TyBool
    | NAT -> Some TyNat
    | _ -> None)

and ty_arrow_p =
  fun st ->
  (let%bind l = ty_atom_p <|> between `Paren ty_p in
   let%bind _ = tok ARROW in
   let%bind r = ty_p in
   return (TyArrow (l, r)))
    st

and ty_ref_p = fun st -> (tok REF *> ty_p >>| fun ty -> TyRef ty) st
and ty_var_p = fun st -> (tok TICK *> ident_p >>| fun v -> TyVar v) st

let%expect_test "ty parse tests" =
  let test s =
    s
    |> Chomp.Lexer.of_string
    |> Chomp.Lexer.lex
    |> run ty_p
    |> Or_error.sexp_of_t sexp_of_ty
    |> print_s
  in
  test "'a";
  test "bool";
  test "(nat)";
  test "nat -> unit";
  test "'a -> 'b -> nat -> bool";
  test "('a -> 'b) -> nat -> bool";
  test "'a -> ('b -> nat) -> bool";
  test "('a -> 'b) -> (nat -> bool)";
  [%expect
    {|
    (Ok 'a)
    (Ok bool)
    (Ok nat)
    (Ok (nat -> unit))
    (Ok ('a -> ('b -> (nat -> bool))))
    (Ok (('a -> 'b) -> (nat -> bool)))
    (Ok ('a -> (('b -> nat) -> bool)))
    (Ok (('a -> 'b) -> (nat -> bool)))
    |}];
  test "";
  test "()";
  test "'nat";
  [%expect
    {|
    (Error ((chomp_error satisfy_eof) (contexts ())))
    (Error ((chomp_error "satisfy_fail on token RPAREN at 1:2") (contexts ())))
    (Error ((chomp_error "satisfy_fail on token TICK at 1:1") (contexts ())))
    |}]
;;

let rec t_p =
  fun st ->
  (let%bind t = t_atom_p <|> between `Paren t_p in
   t_seq_p t <|> t_app_p t <|> return t)
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
    | IF -> t_if_p
    | SUCC -> t_succ_p
    | PRED -> t_pred_p
    | ISZERO -> t_iszero_p
    | FUN -> t_abs_p
    | FIX -> t_fix_p
    | REF -> tok REF *> t_p >>| fun t -> ERef t
    | BANG -> tok BANG *> t_p >>| fun t -> EDeref t
    | _ -> fail "commit: not a fixed prefix"
  in
  (t_assign_p <|> t_singles_p <|> t_commit_prefix_p) st

and t_let_p =
  fun st ->
  (let%bind id = tok LET *> ident_p in
   let%bind bind = tok EQ *> t_p in
   let%bind body = tok IN *> t_p in
   return (ELet (id, bind, body)))
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

and t_succ_p = fun st -> (tok SUCC *> t_p >>| fun t -> ESucc t) st
and t_pred_p = fun st -> (tok PRED *> t_p >>| fun t -> EPred t) st
and t_iszero_p = fun st -> (tok ISZERO *> t_p >>| fun t -> EIsZero t) st
and t_fix_p = fun st -> (tok FIX *> t_p >>| fun t -> EFix t) st

and t_abs_p =
  fun st ->
  (let%bind id = tok FUN *> ident_p in
   let%bind t = tok ARROW *> t_p in
   return (EAbs (id, t)))
    st

and t_letrec_p =
  fun st ->
  (let%bind id = tok LETREC *> ident_p in
   let%bind bind = tok EQ *> t_p in
   let%bind body = tok IN *> t_p in
   return (ELet (id, EFix (EAbs (id, bind)), body)))
    st

and t_assign_p =
  fun st ->
  (let%bind v = ident_p in
   let%bind t = tok ASSIGN *> t_p in
   return (EAssign (v, t)))
    st

and t_seq_p t =
  match%map tok SEMI *> t_p with
  | ESeq (t', t'') -> ESeq (ESeq (t, t'), t'')
  | t' -> ESeq (t, t')
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
  [%expect
    {|
    (Ok #u)
    (Ok (if #f then #u else #f))
    (Ok (if #f then #u))
    (Ok (if (if #u then #f else #t) then (if #t then #f) else #f))
    (Ok (let x = (if #f then #f) in #t))
    |}];
  test "f x y z";
  test "f (x y) z";
  [%expect
    {|
    (Ok (((f x) y) z))
    (Ok ((f (x y)) z))
    |}];
  test "Z Z Z Z";
  test "iszero (pred (S (S Z)))";
  test "fun x -> x";
  [%expect
    {|
    (Ok (((Z Z) Z) Z))
    (Ok (iszero (pred (S (S Z)))))
    (Ok (fun x -> x))
    |}]
;;
