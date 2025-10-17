open Core
open Parser
open Parser.Let_syntax
open Parser.Infix_syntax
open Types

let sep_commas p = sep_by1 (strip (char_p ',')) p

(* TODO: Disable ocamlformat in region *)
let reserved =
  String.Set.of_list
    [ "#u"
    ; "#t"
    ; "#f"
    ; "if"
    ; "then"
    ; "else"
    ; "Z"
    ; "let"
    ; "in"
    ; "="
    ; "."
    ; "{"
    ; "}"
    ; "["
    ; "]"
    ; "("
    ; ")"
    ; "<"
    ; ">"
    ]
;;

let ident_p =
  let%bind s = String.of_list <$> alpha_p in
  if Set.mem reserved s
  then
    (* TODO: There has to be a better way to track position as well *)
    Fn.const (Or_error.error_string "var: reserved keyword")
  else return s
;;

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
  let%map c = satisfy Char.is_uppercase in
  TyBase c

and ty_unit_p = return TyUnit <* string_p "unit"
and ty_bool_p = return TyBool <* string_p "bool"
and ty_nat_p = return TyNat <* string_p "nat"

and ty_tuple_p =
  fun st ->
  (let%map tys = between `Curly (sep_commas ty_p) in
   TyTuple tys)
    st

and field_p =
  fun st ->
  (let%bind l = ident_p in
   let%bind _ = strip (char_p ':') in
   let%bind r = ty_p in
   return (l, r))
    st

and ty_record_p =
  fun st ->
  (let%map fields = between `Curly (sep_commas field_p) in
   TyRecord fields)
    st

and ty_variant_p =
  fun st ->
  let unit_field_p =
    let%map l = ident_p in
    l, TyUnit
  in
  let record_p =
    let%map fields = between `Angle (sep_commas (field_p <|> unit_field_p)) in
    TyRecord fields
  in
  record_p st

and ty_arrow_p =
  fun st ->
  (let%bind l = ty_atom_p <|> between `Paren ty_p in
   let%bind _ = strip (string_p "->") in
   let%bind r = ty_p in
   return (TyArrow (l, r)))
    st
;;

let%expect_test "ty parse tests" =
  let test s =
    let ty = run ty_p s in
    print_s (Or_error.sexp_of_t sexp_of_ty ty)
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
  test "{bool,{unit->bool->bool,nat}->{nat,nat}}";
  test "{ x : nat , y :{ bool:bool}}";
  test "< some : nat, none >";
  [%expect
    {|
    (Ok A)
    (Ok bool)
    (Error ((pos ((line 1) (col 2))) "satisfy: pred not satisfied"))
    (Ok (nat -> unit))
    (Ok (A -> (X -> (nat -> bool))))
    (Ok ((A -> X) -> (nat -> bool)))
    (Ok (A -> ((X -> nat) -> bool)))
    (Ok ((A -> X) -> (nat -> bool)))
    (Ok ({ nat , nat }))
    (Ok ({ bool , (({ (unit -> (bool -> bool)) , nat }) -> ({ nat , nat })) }))
    (Ok (| x : nat , y : (| bool : bool |) |))
    (Ok (| some : nat , none : unit |))
    |}];
  test "";
  test "{}";
  test "{ a , b : bool }";
  test "<>";
  test "()";
  (* TODO: Better error messages *)
  (* TODO: How does angstrong handle the fun st -> st) type issue *)
  [%expect
    {|
    (Error "satisfy: EOF")
    (Error ((pos ((line 1) (col 1))) "satisfy: pred not satisfied"))
    (Error ((pos ((line 1) (col 1))) "satisfy: pred not satisfied"))
    (Error ((pos ((line 1) (col 1))) "satisfy: pred not satisfied"))
    (Error ((pos ((line 1) (col 2))) "satisfy: pred not satisfied"))
    |}]
;;

let rec t_p =
  fun st ->
  let t_p = t_proj_p <|> t_seq_p <|> t_atom_p in
  (t_p <|> between `Paren t_p) st

and t_atom_p =
  fun st ->
  (t_unit_p
   <|> t_true_p
   <|> t_false_p
   <|> t_if_p
   <|> t_var_p
   <|> t_let_p
   <|> t_tuple_p
   <|> t_record_p
   <|> t_variant_p)
    st

and t_unit_p = return EUnit <* string_p "#u"
and t_true_p = return ETrue <* string_p "#t"
and t_false_p = return EFalse <* string_p "#f"

and t_var_p =
  fun st ->
  (let%map id = ident_p in
   EVar id)
    st

and t_let_p =
  fun st ->
  (let%bind id = string_p "let" *> empty_p *> strip ident_p in
   let%bind bind = char_p '=' *> strip t_p in
   let%bind body = string_p "in" *> empty_p *> strip t_p in
   return (ELet (id, bind, body)))
    st

and t_tuple_p =
  fun st ->
  (let%map ts = between `Curly (sep_commas t_p) in
   ETuple ts)
    st

and t_proj_p =
  fun st ->
  (let%bind t = t_atom_p <* char_p '.' in
   (let%map i = numeric_p in
    EProjTuple (t, Int.of_string (String.of_list i)))
   <|>
   let%map l = ident_p in
   EProjRecord (t, l))
    st

and t_record_p =
  fun st ->
  let field_p =
    let%bind l = ident_p in
    let%bind _ = strip (char_p '=') in
    let%bind r = t_p in
    return (l, r)
  in
  (let%map fields = between `Curly (sep_commas field_p) in
   ERecord fields)
    st

and t_variant_p =
  fun st ->
  (let variant_p =
     let%bind label = ident_p in
     let%bind value = empty_p *> (t_p <|> return EUnit) in
     return (label, value)
   in
   let%bind label, value = between `Angle variant_p in
   let%bind ty = empty_p *> string_p "as" *> empty_p *> ty_p in
   return (EVariant (label, ty, value)))
    st

and t_seq_p =
  fun st ->
  (let%bind t = t_atom_p in
   let%bind _ = strip (char_p ';') in
   let%bind t' = t_p in
   return (ESeq (t, t')))
    st

(* TODO: EMatch of t * (string * string * t) list *)

and t_if_p =
  fun st ->
  (let%bind c = string_p "if" *> empty_p *> strip t_p in
   let%bind t = string_p "then" *> empty_p *> strip t_p in
   let%bind f = string_p "else" *> empty_p *> strip t_p <|> return EUnit in
   return (EIf (c, t, f)))
    st

and t_zero_p = return EZero <* string_p "Z"

(* TODO: EAbs of string * ty * t *)
(* TODO: EApp of t * t *)
(* TODO: EAs of t * ty *)
(* TODO: ESucc of t *)
(* TODO: EPred of t *)
(* TODO: EIsZero of t *)
(* TODO: EFix of t *)

let%expect_test "t parse tests" =
  let test s = s |> run t_p |> Or_error.sexp_of_t sexp_of_t |> print_s in
  test "#u";
  test "if #f then #u else #f";
  test "if #f then #u";
  test "if if #u then #f else #t then (if #t then #f) else #f";
  test "let x = v in #t";
  test "let x=if #f then #f   in    #t";
  [%expect
    {|
    (Ok #u)
    (Ok (if #f #u #f))
    (Ok (if #f #u #u))
    (Ok (if (if #u #f #t) (if #t #f #u) #f))
    (Ok (let x = v in #t))
    (Ok (let x = (if #f #f #u) in #t))
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
    (Ok (< some : x > as (| some : nat , none : unit |)))
    (Ok (< none : #u > as (| some : nat , none : unit |)))
    |}];
  test "let x = a; b; c in #t; #f";
  [%expect
    {| (Ok (let x = (seq a (seq b c)) in (seq #t #f))) |}];
;;
