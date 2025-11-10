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
    | BASE c -> Some (Char.to_string c)
    | _ -> None)
;;

let commas p = sep_by (tok COMMA) p
let parens p = tok LPAREN *> p <* tok RPAREN
let class_name_p = ident_p >>| class_name_of_string
let field_name_p = ident_p >>| field_name_of_string
let method_name_p = ident_p >>| method_name_of_string

let rec fix p t =
  (let%bind t = p t in
   fix p t)
  <|> return t
;;

let rec term_p =
  fun st ->
  (let prefix_p =
     let create_object_p =
       let%bind class_name = tok NEW *> class_name_p in
       let%bind terms = parens (commas term_p) in
       return (CreateObject (class_name, terms))
     in
     let cast_p =
       let%bind class_name = parens class_name_p in
       let%bind term = term_p in
       return (Cast (class_name, term))
     in
     let var_p = ident_p >>| fun v -> Var v in
     create_object_p <|> cast_p <|> var_p
   in
   let suffix_p term =
     let invoke_method_p term =
       let%bind method_name = tok DOT *> method_name_p in
       let%bind terms = parens (sep_by (tok COMMA) term_p) in
       return (InvokeMethod (term, method_name, terms))
     in
     let field_access_p term =
       let%bind field = tok DOT *> field_name_p in
       return (FieldAccess (term, field))
     in
     invoke_method_p term <|> field_access_p term
   in
   let%bind prefix = prefix_p <|> parens term_p in
   fix suffix_p prefix <|> return prefix)
    st
;;

let field_p =
  let%bind class_name = class_name_p in
  let%bind field_name = field_name_p in
  return (class_name, field_name)
;;

let method_p =
  let%bind _ = ident_p in
  (* TODO ASSERT: ^ SHOULD BE CLASS NAME *)
  let%bind method_name = method_name_p in
  let%bind fields = tok LPAREN *> commas field_p <* tok RPAREN in
  let%bind term = tok LCURLY *> tok RETURN *> term_p <* tok RCURLY in
  return { method_name; fields; term }
;;

let class_decl_p =
  let%bind class_name = tok CLASS *> class_name_p in
  let%bind superclass_name = tok EXTENDS *> class_name_p in
  (* Field Declarations *)
  let%bind params = tok LCURLY *> many (field_p <* tok SEMI) in
  (* Constructor *)
  let%bind _ =
    satisfy (equal_token (ID (string_of_class_name class_name))) *> tok LPAREN
  in
  let%bind constructor_fields = commas field_p <* tok RPAREN in
  ignore constructor_fields;
  (* TODO ASSERT: PARAMS AND ^ ARE THE SAME *)
  (* super() *)
  let%bind fields_to_super =
    tok LCURLY *> tok SUPER *> tok LPAREN *> many field_name_p <* tok RPAREN <* tok SEMI
  in
  let this_field_p =
    let%bind left = tok THIS *> tok DOT *> ident_p in
    let%bind right = tok EQ *> ident_p in
    return (left, right)
    (* TODO ASSERT: ^ left and right are the same *)
  in
  (* this.f = f *)
  let%bind this_fields = many (this_field_p <* tok SEMI) in
  ignore this_fields;
  (* TODO ASSERT: PARAMS AND ^ are compat *)
  (* Methods *)
  let%bind methods = tok RCURLY *> many method_p <* tok RCURLY in
  return { class_name; superclass_name; params; fields_to_super; methods }
;;

let program_p =
  let%bind class_decls = many class_decl_p in
  let%bind t = term_p in
  return (Program (class_decls, t))
;;

let test tag s =
  let run_and_print parser sexp_of =
    s
    |> Chomp.Lexer.of_string
    |> Chomp.Lexer.lex
    |> run parser
    |> Or_error.sexp_of_t sexp_of
    |> print_s
  in
  match tag with
  | `Term -> run_and_print term_p sexp_of_t
  | `Method -> run_and_print method_p sexp_of_method_decl
;;

let%expect_test "featherweight java parse term" =
  test `Term {| x |};
  test `Term {| x.field |};
  test `Term {| x.method() |};
  test `Term {| x.method(f.nested(h,y) , g.field) |};
  test `Term {| new Class() |};
  test `Term {| new Class(f, g.field, h.method(), new Again()) |};
  test `Term {| (Cast) new Object() |};
  test `Term {| (Cast) (DoubleCast) new Object() |};
  [%expect
    {|
    (Ok x)
    (Ok x.field)
    (Ok "x.method()")
    (Ok "x.method(f.nested(h, y), g.field)")
    (Ok "new Class()")
    (Ok "new Class(f, g.field, h.method(), new Again())")
    (Ok "(Cast) new Object()")
    (Ok "(Cast) (DoubleCast) new Object()")
    |}];
  test `Term {| x.one.two.three |};
  test `Term {| (x).one.two.three |};
  test `Term {| (x.one.two).three |};
  test `Term {| ((((((x).one)).two).three)) |};
  test `Term {| x.one().two().three() |};
  test `Term {| ((Cast) new Obj(x.two())).four() |};
  [%expect
    {|
    (Ok x.one.two.three)
    (Ok x.one.two.three)
    (Ok x.one.two.three)
    (Ok x.one.two.three)
    (Ok "x.one().two().three()")
    (Ok "(Cast) new Obj(x.two()).four()")
    |}]
;;
