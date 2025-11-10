open Core
open Types

(** Look up type of method in class, [fields -> term]  (Fig 19.2) *)
let rec method_type (tbl : class_decl list) (m : method_name) (c : class_name) =
  let cl = Utils.find_class tbl c in
  let md_opt = List.find cl.methods ~f:(fun md -> equal_method_name md.method_name m) in
  match md_opt with
  | Some md -> List.map ~f:fst md.fields, fst md.term
  | None -> method_type tbl m cl.superclass_name
;;

(* TODO: Better tests *)
(* TODO: M OK in C *)
(* TODO: C OK *)

let rec type_of (tbl : class_decl list) (ctx : class_name String.Map.t) =
  let open Or_error.Let_syntax in
  function
  | Var v ->
    (* T-Var *)
    Map.find_or_error ctx v
    |> Or_error.tag_s ~tag:[%message "no var in ctx" v (ctx : class_name String.Map.t)]
  | FieldAccess (t, field) ->
    (* T-Field *)
    let%bind class_name = type_of tbl ctx t in
    Utils.fields tbl class_name
    |> List.find ~f:(Fn.compose (equal_field_name field) snd)
    |> Option.map ~f:fst
    |> Or_error.of_option ~error:(Error.of_string "missing field")
  | InvokeMethod (t, mn, args) ->
    (* T-Invk *)
    let%bind cn = type_of tbl ctx t in
    let%bind arg_cns = Or_error.all (List.map ~f:(type_of tbl ctx) args) in
    let field_cns, ret = method_type tbl mn cn in
    (match List.for_all2 arg_cns field_cns ~f:(Utils.is_subtype tbl) with
     | Ok true -> return ret
     | Ok false -> Or_error.error_string "args and fields not subtyped"
     | Unequal_lengths -> Or_error.error_string "args and fields diff length")
  | CreateObject (class_name, args) ->
    (* T-New *)
    let%bind arg_cns = Or_error.all (List.map ~f:(type_of tbl ctx) args) in
    let field_cns = List.map ~f:fst (Utils.fields tbl class_name) in
    (match List.for_all2 arg_cns field_cns ~f:(Utils.is_subtype tbl) with
     | Ok true -> return class_name
     | Ok false -> Or_error.error_string "args and fields not subtyped"
     | Unequal_lengths -> Or_error.error_string "args and fields diff length")
  | Cast (cn, t) ->
    let%bind cn' = type_of tbl ctx t in
    if Utils.is_subtype tbl cn' cn || Utils.is_subtype tbl cn cn'
    then return cn (* T-UCast / T-DCast*)
    else (
      (* T-SCast *)
      print_endline "warning: stupid cast";
      return cn)
;;

let typecheck (Program (tbl, t)) = type_of tbl String.Map.empty t

let preamble =
  {|
   class A extends Object { A() { super(); }}
   class B extends Object { B() { super(); }}

   class Pair extends Object {
     Object fst;
     Object snd;

     Pair(Object fst, Object snd) {
       super();
       this.fst = fst;
       this.snd = snd;
     }

     Pair setfst(Object newfst) {
       return new Pair(newfst, this.snd);
     }
   }

   class Integer extends Object {
     Integer() { super(); }
     Integer prev() { return this; }
     Integer add(Integer other) { return this; }
   }

   class Zero extends Integer {
     Zero() { super(); }
     Integer prev() { return (Integer) this; }
     Integer add(Integer other) { return other; }
   }

   class Succ extends Integer {
     Integer prev;

     Succ(Integer prev) {
       super();
       this.prev = prev;
     }

     Integer prev() { return this.prev; }
     Integer add(Integer other) {
       return new Succ(((Integer) this).prev().add(other));
     }
   }
  |}
;;

let test s =
  s
  |> String.append preamble
  |> Chomp.Lexer.of_string
  |> Chomp.Lexer.lex
  |> Parser.(Parser.run program_p)
  |> Or_error.bind ~f:typecheck
  |> Or_error.sexp_of_t sexp_of_class_name
  |> print_s
;;

let%expect_test "featherweight java typecheck pair tests" =
  test "new Pair(new A(), new B()).fst";
  test "new Pair(new A(), new B()).setfst(new B())";
  [%expect
    {|
    (Ok Object)
    (Ok Pair)
    |}];
  test "(Object) new Pair(new A(), new B())";
  test "(B) new Pair((B) new A(), new B())";
  [%expect
    {|
    (Ok Object)
    warning: stupid cast
    warning: stupid cast
    (Ok B)
    |}]
;;

let%expect_test "featherweight java typecheck int tests" =
  test "new Succ(new Succ(new Zero()))";
  test "(new Succ(new Succ(new Zero()))).add(new Succ(new Succ(new Zero())))";
  [%expect
    {|
    (Ok Succ)
    (Ok Integer)
    |}]
;;
