open Core
open Parser
open Parser.Let_syntax
open Parser.Infix_syntax
open Types

let parens p = ignore_m (char_p '(') *> p <* ignore_m (char_p ')')

let rec ty_p =
  fun st ->
  let ty_p = ty_arrow_p <|> ty_atom_p in
  (ty_p <|> parens ty_p) st

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

and ty_unit_p = return TyUnit <* ignore_m (string_p "unit")
and ty_bool_p = return TyBool <* ignore_m (string_p "bool")
and ty_nat_p = return TyNat <* ignore_m (string_p "nat")

and ty_tuple_p =
  fun st ->
  (let%bind _ = char_p '{' in
   let%bind tys = strip (sep_by1 (ignore_m (strip (char_p ','))) ty_p) in
   let%bind _ = char_p '}' in
   return (TyTuple tys))
    st

and field_p =
  fun st ->
  (let%bind l = alpha_p in
   let%bind _ = strip (char_p ':') in
   let%bind r = ty_p in
   return (String.of_list l, r))
    st

and ty_record_p =
  fun st ->
  let record_p =
    let%bind _ = char_p '{' in
    let%bind fields = strip (sep_by1 (ignore_m (strip (char_p ','))) field_p) in
    let%bind _ = char_p '}' in
    return (TyRecord fields)
  in
  record_p st

and ty_variant_p =
  fun st ->
  let unit_field_p =
    let%map l = alpha_p in
    String.of_list l, TyUnit
  in
  let record_p =
    let%bind _ = char_p '<' in
    let%bind fields =
      strip (sep_by1 (ignore_m (strip (char_p ','))) (field_p <|> unit_field_p))
    in
    let%bind _ = char_p '>' in
    return (TyRecord fields)
  in
  record_p st

and ty_arrow_p =
  fun st ->
  (let%bind l = ty_atom_p <|> parens ty_p in
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
  [%expect
    {|
    (Error "satisfy: EOF")
    (Error ((pos ((line 1) (col 1))) "satisfy: pred not satisfied"))
    (Error ((pos ((line 1) (col 1))) "satisfy: pred not satisfied"))
    (Error ((pos ((line 1) (col 1))) "satisfy: pred not satisfied"))
    (Error ((pos ((line 1) (col 2))) "satisfy: pred not satisfied"))
    |}]
;;
