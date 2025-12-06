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
  <??> "ident"
;;

let between brace_type p =
  let l, r =
    match brace_type with
    | `Paren -> LPAREN, RPAREN
    | `Curly -> LCURLY, RCURLY
    | `Angle -> LANGLE, RANGLE
  in
  tok l *> p <* tok r <??> "between"
;;

let commas p = sep_by1 (tok COMMA) p

let rec ty_p =
  fun st ->
  let ty_p = ty_arrow_p <|> ty_atom_p in
  (ty_p <|> between `Paren ty_p <??> "ty") st

and ty_atom_p =
  fun st ->
  (ty_singles_p <|> ty_forall <|> ty_exists <|> ty_tuple_p <|> ty_record_p <|> ty_ref_p)
    st

and ty_singles_p =
  satisfy_map (function
    | UNITTY -> Some TyUnit
    | BOOL -> Some TyBool
    | NAT -> Some TyNat
    | ID s -> Some (TyVar s)
    | BASE c -> Some (TyVar (String.of_char c))
    | _ -> None)
  <??> "ty_single"

and ty_tuple_p =
  fun st ->
  (let%bind tys = between `Curly (commas ty_p) in
   return (TyTuple tys) <??> "ty_tuple")
    st

and field_p =
  fun st ->
  (let%bind l = ident_p in
   let%bind _ = tok COLON in
   let%bind r = ty_p in
   return (l, r) <??> "ty_field")
    st

and ty_record_p =
  fun st ->
  (let%bind fields = between `Curly (commas field_p) in
   return (TyRecord fields) <??> "ty_record")
    st

and ty_arrow_p =
  fun st ->
  (let%bind l = ty_atom_p <|> between `Paren ty_p in
   tok ARROW
   *> commit
        (let%bind r = ty_p in
         return (TyArrow (l, r)))
   <??> "ty_arrow")
    st

and ty_ref_p = fun st -> (tok REF *> commit (ty_p >>| fun v -> TyRef v) <??> "ty_ref") st

and ty_forall =
  fun st ->
  (tok FORALL
   *> commit
        (let%bind v = ident_p in
         let%bind ty = tok DOT *> ty_p in
         return (TyForall (v, ty)))
   <??> "ty_forall")
    st

and ty_exists =
  fun st ->
  (tok LCURLY
   *> tok EXISTS
   *> commit
        (let%bind v = ident_p in
         let%bind ty = tok COMMA *> ty_p <* tok RCURLY in
         return (TyExists (v, ty)))
   <??> "ty_exists")
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
    |}];
  test "forall X nat";
  test "(X -> forall X -> X)";
  test "(X -> {exists -> X)";
  test "()";
  [%expect
    {|
    (Error (ty (ty_forall (satisfy_fail (pos 1:10)))))
    (Error
     (ty (between (ty (ty_arrow (ty (ty_forall (satisfy_fail (pos 1:16)))))))))
    (Error
     (ty
      (between
       (ty (ty_arrow (ty (ty_exists (ident (satisfy_map_fail (pos 1:15))))))))))
    (Error (ty (between (ty_ref (satisfy_fail (pos 1:2))))))
    |}]
;;

let rec t_p =
  fun st ->
  (let%bind t = t_atom_p <|> between `Paren t_p in
   t_proj_p t <|> t_seq_p t <|> t_ty_app_p t <|> t_app_p t <|> return t <??> "t")
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
    <??> "t_single"
  in
  (t_assign_p
   <|> t_singles_p
   <|> t_ref_p
   <|> t_deref_p
   <|> t_record_p
   <|> t_tuple_p
   <|> t_abs_p
   <|> t_ty_abs_p
   <|> t_if_p
   <|> t_unpack_p
   <|> t_let_p
   <|> t_succ_p
   <|> t_pred_p
   <|> t_iszero_p
   <|> t_pack_p
   <??> "t_atom")
    st

and t_let_p =
  fun st ->
  (tok LET
   *> commit
        (let%bind id = ident_p in
         let%bind bind = tok EQ *> t_p in
         let%bind body = tok IN *> t_p in
         return (ELet (id, bind, body)))
   <??> "t_let")
    st

and t_tuple_p =
  fun st ->
  (let%bind ts = between `Curly (commas t_p) in
   return (ETuple ts) <??> "t_tuple")
    st

and t_proj_p t =
  let%bind _ = tok DOT in
  (let%bind i =
     satisfy_map (function
       | INT i -> Some i
       | _ -> None)
   in
   return (EProjTuple (t, i)) <??> "t_proj_tuple")
  <|>
  let%bind l = ident_p in
  return (EProjRecord (t, l)) <??> "t_proj_record"

and t_record_p =
  fun st ->
  let field_p =
    let%bind l = ident_p in
    let%bind r = tok EQ *> t_p in
    return (l, r)
  in
  (let%bind fields = between `Curly (commas field_p) in
   return (ERecord fields) <??> "t_record")
    st

and t_seq_p t =
  (match%map tok SEMI *> t_p with
   | ESeq (t', t'') -> ESeq (ESeq (t, t'), t'')
   | t' -> ESeq (t, t'))
  <??> "t_seq"

and t_ref_p = fun st -> (tok REF *> commit (t_p >>| fun t -> ERef t) <??> "t_ref") st

and t_deref_p =
  fun st -> (tok BANG *> commit (t_p >>| fun t -> EDeref t) <??> "t_deref") st

and t_assign_p =
  fun st ->
  (let%bind v = ident_p in
   let%bind t = tok ASSIGN *> t_p in
   return (EAssign (v, t)) <??> "t_assign")
    st

and t_if_p =
  fun st ->
  (tok IF
   *> commit
        (let%bind c = t_p in
         let%bind t = tok THEN *> t_p in
         let%bind f = tok ELSE *> t_p <|> return EUnit in
         return (EIf (c, t, f)))
   <??> "t_if")
    st

and t_app_p t =
  let%bind ts = many (t_atom_p <|> between `Paren t_p) in
  return (List.fold_left ~f:(fun f x -> EApp (f, x)) ~init:t ts) <??> "t_app"

and t_succ_p st = (tok SUCC *> commit t_p <??> "t_succ" >>| fun t -> ESucc t) st
and t_pred_p st = (tok PRED *> commit t_p <??> "t_pred" >>| fun t -> EPred t) st
and t_iszero_p st = (tok ISZERO *> commit t_p <??> "t_iszero" >>| fun t -> EIsZero t) st

and t_abs_p =
  fun st ->
  (tok FUN
   *> tok LPAREN
   *> commit
        (let%bind id = ident_p in
         let%bind ty = tok COLON *> ty_p in
         let%bind t = tok RPAREN *> tok ARROW *> t_p in
         return (EAbs (id, ty, t)))
   <??> "t_abs")
    st

and t_ty_abs_p =
  fun st ->
  (tok FUN
   *> commit
        (let%bind v = ident_p in
         let%bind t = tok DOT *> t_p in
         return (ETyAbs (v, t)))
   <??> "t_ty_abs")
    st

and t_ty_app_p t =
  let%bind ty = tok LBRACKET *> ty_p <* tok RBRACKET in
  return (ETyApp (t, ty)) <??> "t_ty_app"

and t_pack_p =
  fun st ->
  (tok LCURLY
   *> commit
        (let%bind ty = tok STAR *> ty_p in
         let%bind t = tok COMMA *> t_p in
         let%bind ty_as = tok RCURLY *> tok AS *> ty_p in
         return (EPack (ty, t, ty_as)))
   <??> "t_pack")
    st

and t_unpack_p =
  fun st ->
  (tok LET
   *> tok LCURLY
   *> commit
        (let%bind v_ty = ident_p in
         let%bind v_t = tok COMMA *> ident_p in
         let%bind t = tok RCURLY *> tok EQ *> t_p in
         let%bind t_bind = tok IN *> t_p in
         return (EUnpack (v_ty, v_t, t, t_bind)))
   <??> "t_unpack")
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
    |}];
  test
    [%string
      {|
      let add =
        (fun (x : nat) ->
          (fun (y : nat) ->
            (if (iszero x)
              then y
              else (add (pred x) (S y)))))
      in add (S (S Z)) (S (S Z))
      |}];
  [%expect
    {|
    (Ok
     (let add =
      (fun x : nat ->
       (fun y : nat -> (if (iszero x) then y else ((add (pred x)) (S y)))))
      in ((add (S (S Z))) (S (S Z)))))
    |}]
;;
