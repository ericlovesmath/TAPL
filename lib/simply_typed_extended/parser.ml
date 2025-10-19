open Core
open Types
open Lexer
include Chomp.Make (Lexer)
open Let_syntax
open Infix_syntax

let ident_p : string t =
  let%bind next = satisfy (Fn.const true) in
  match next with
  | ID s -> return s
  | _ -> fail "not a ident"
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
  fun st ->
  (ty_tuple_p
   <|> ty_record_p
   <|> ty_variant_p
   <|> ty_base_p
   <|> ty_unit_p
   <|> ty_bool_p
   <|> ty_nat_p)
    st

and ty_base_p =
  let%bind next = satisfy (Fn.const true) in
  match next with
  | BASE c -> return (TyBase c)
  | _ -> fail "not a tybase"

and ty_unit_p = return TyUnit <* tok UNITTY
and ty_bool_p = return TyBool <* tok BOOL
and ty_nat_p = return TyNat <* tok NAT

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
;;

let%expect_test "ty parse tests" =
  let test s =
    s
    |> Lexer.of_string
    |> Lexer.lex
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
    (Ok (| x : nat , y : (| b : bool |) |))
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
  let t_p = t_proj_p <|> t_seq_p <|> t_as_p <|> t_app_p <|> t_atom_p in
  (t_p <|> between `Paren t_p) st

and t_atom_p =
  fun st ->
  (t_unit_p
   <|> t_true_p
   <|> t_false_p
   <|> t_if_p
   <|> t_var_p
   <|> t_let_p
   <|> t_letrec_p
   <|> t_tuple_p
   <|> t_record_p
   <|> t_variant_p
   <|> t_match_p
   <|> t_zero_p
   <|> t_succ_p
   <|> t_pred_p
   <|> t_iszero_p
   <|> t_fix_p
   <|> t_abs_p)
    st

and t_unit_p = return EUnit <* tok UNIT
and t_true_p = return ETrue <* tok TRUE
and t_false_p = return EFalse <* tok FALSE

and t_var_p =
  let%map id = ident_p in
  EVar id

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

and t_proj_p =
  fun st ->
  (let%bind t = t_atom_p <|> between `Paren t_p <* tok DOT in
   (let%map i =
      let%bind next = satisfy (Fn.const true) in
      match next with
      | INT i -> return i
      | _ -> fail "not an int"
    in
    EProjTuple (t, i))
   <|>
   let%map l = ident_p in
   EProjRecord (t, l))
    st

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
   let%bind ty = tok AS *> ty_p in
   return (EVariant (label, ty, value)))
    st

and t_seq_p =
  fun st ->
  (let%bind t = t_atom_p <|> between `Paren t_p in
   let%bind t' = tok SEMI *> t_p in
   return (ESeq (t, t')))
    st

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

and t_app_p =
  fun st ->
  (let%bind ts = many1 (t_atom_p <|> between `Paren t_p) in
   match ts with
   | [] -> fail "app: many1 should never return an empty list"
   | hd :: tl -> return (List.fold_left ~f:(fun f x -> EApp (f, x)) ~init:hd tl))
    st

and t_as_p =
  fun st ->
  (let%bind t = t_atom_p <|> between `Paren t_p in
   let%bind ty = tok AS *> ty_p in
   return (EAs (t, ty)))
    st

and t_zero_p = return EZero <* tok ZERO
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
    |> Lexer.of_string
    |> Lexer.lex
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
    (Ok (if #f #u #f))
    (Ok (if #f #u #u))
    (Ok (if (if #u #f #t) (if #t #f #u) #f))
    (Ok (let x = (if #f #f #u) in #t))
    (Ok (let x = (fix (fun x : bool -> x)) in #t))
    |}];
  test "{ {#t,if #t then b} , #f,#t}";
  test "v.0";
  test "{ #t , #f,#t}.0";
  test "{ #t , #f,#t}.22";
  test "{ x = #t , y = v.0 }.x";
  test "< some x > as < some : nat, none >";
  test "< none > as < some : nat, none >";
  [%expect
    {|
    (Ok ({ ({ #t , (if #t b #u) }) , #f , #t }))
    (Ok (v . 0))
    (Ok (({ #t , #f , #t }) . 0))
    (Ok (({ #t , #f , #t }) . 22))
    (Ok ((| x : #t , y : (v . 0) |) . x))
    (Ok (< some : x > as (< some : nat , none >)))
    (Ok (< none : #u > as (< some : nat , none >)))
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
  test "match pos as < p : nat , end > with | p n -> n | end -> #u";
  [%expect
    {|
    (Ok (let x = (seq a (seq b c)) in (seq #t #f)))
    (Ok (let x = (seq a (seq b c)) in (seq #t #f)))
    (Ok (case x of (some x => #t) (none $_ => #f)))
    (Ok (((f x) y) z))
    (Ok ((f (x y)) z))
    (Ok (case ((f (x y)) z) of (some x => #t) (none $_ => #f)))
    (Ok (x as bool))
    (Ok (case (pos as (< p : nat , end >)) of (p n => n) (end $_ => #u)))
    |}];
  test "Z Z Z Z";
  test "iszero (pred (S (S Z)))";
  test "fix (S Z)";
  test "fun x : bool -> x";
  [%expect
    {|
    (Ok (((0 0) 0) 0))
    (Ok (iszero (pred (succ (succ 0)))))
    (Ok (fix (succ 0)))
    (Ok (fun x : bool -> x))
    |}]
;;
