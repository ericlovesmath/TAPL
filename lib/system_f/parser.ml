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
    | BASE c -> Some (String.of_char c)
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

and ty_atom_p = fun st -> (ty_singles_p <|> ty_forall <|> ty_exists <|> ty_ref_p) st

and ty_singles_p =
  satisfy_map (function
    | UNITTY -> Some TyUnit
    | BOOL -> Some TyBool
    | NAT -> Some TyNat
    | ID s -> Some (TyVar s)
    | BASE c -> Some (TyVar (String.of_char c))
    | _ -> None)

and ty_arrow_p =
  fun st ->
  (let%bind l = ty_atom_p <|> between `Paren ty_p in
   let%bind _ = tok ARROW in
   let%bind r = ty_p in
   return (TyArrow (l, r)))
    st

and ty_ref_p = fun st -> (tok REF *> ty_p) st

and ty_forall =
  fun st ->
  (let%bind v = tok FORALL *> ident_p in
   let%bind ty = tok DOT *> ty_p in
   return (TyForall (v, ty)))
    st

and ty_exists =
  fun st ->
  (let%bind v = tok LCURLY *> tok EXISTS *> ident_p in
   let%bind ty = tok COMMA *> ty_p <* tok RCURLY in
   return (TyExists (v, ty)))
    st
;;

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
  test "nat -> unit";
  test "A -> X -> nat -> bool";
  test "(A -> X) -> nat -> bool";
  test "A -> (X -> nat) -> bool";
  test "(A -> X) -> (nat -> bool)";
  test "forall X . X";
  test "{exists X, X -> bool}";
  test "";
  test "()";
  [%expect
    {|
    (Ok A)
    (Ok bool)
    (Ok (nat -> unit))
    (Ok (A -> (X -> (nat -> bool))))
    (Ok ((A -> X) -> (nat -> bool)))
    (Ok (A -> ((X -> nat) -> bool)))
    (Ok ((A -> X) -> (nat -> bool)))
    (Ok (forall X . X))
    (Ok ({ exists X , (X -> bool) }))
    (Error "satisfy: EOF")
    (Error ((pos ((i 1) (line 1) (col 2))) "satisfy: pred not satisfied"))
    |}]
;;

let rec t_p =
  fun st ->
  (let%bind t = t_atom_p <|> between `Paren t_p in
   t_ty_app_p t <|> t_app_p t <|> return t)
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
    | LET -> t_let_p <|> t_unpack_p
    | IF -> t_if_p
    | SUCC -> t_succ_p
    | PRED -> t_pred_p
    | ISZERO -> t_iszero_p
    | FUN -> t_abs_p <|> t_ty_abs_p
    | LCURLY -> t_pack_p
    | _ -> fail "commit: not a fixed prefix"
  in
  (t_singles_p <|> t_commit_prefix_p) st

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

and t_abs_p =
  fun st ->
  (let%bind id = tok FUN *> tok LPAREN *> ident_p in
   let%bind ty = tok COLON *> ty_p in
   let%bind t = tok RPAREN *> tok ARROW *> t_p in
   return (EAbs (id, ty, t)))
    st

and t_ty_abs_p =
  fun st ->
  (let%bind v = tok FUN *> ident_p in
   let%bind t = tok DOT *> t_p in
   return (ETyAbs (v, t)))
    st

and t_ty_app_p t =
  let%map ty = tok LBRACKET *> ty_p <* tok RBRACKET in
  ETyApp (t, ty)

and t_pack_p =
  fun st ->
  (let%bind ty = tok LCURLY *> tok STAR *> ty_p in
   let%bind t = tok COMMA *> t_p in
   let%bind ty_as = tok RCURLY *> tok AS *> ty_p in
   return (EPack (ty, t, ty_as)))
    st

and t_unpack_p =
  fun st ->
  (let%bind v_ty = tok LET *> tok LCURLY *> ident_p in
   let%bind v_t = tok COMMA *> ident_p in
   let%bind t = tok RCURLY *> tok EQ *> t_p in
   let%bind t_bind = tok IN *> t_p in
   return (EUnpack (v_ty, v_t, t, t_bind)))
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
  test "fun (x : bool) -> x";
  [%expect
    {|
    (Ok (((Z Z) Z) Z))
    (Ok (iszero (pred (S (S Z)))))
    (Ok (fun x : bool -> x))
    |}];
  test "fun x . id";
  test "{*int, term} as bool";
  test "let {ty, v} = Z in S Z";
  test "id [bool]";
  [%expect
    {|
    (Ok (fun x . id))
    (Ok ({* int , term } as bool))
    (Ok (let { ty , v } = Z in (S Z)))
    (Ok (id [ bool ]))
    |}]
;;
