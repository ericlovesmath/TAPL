open Core
open Types

let is_object = equal_class_name (class_name_of_string "Object")

let find_class (tbl : class_decl list) (c : class_name) =
  List.find_exn tbl ~f:(fun cl -> equal_class_name cl.class_name c)
;;

let find_method (cl : class_decl) (m : method_name) =
  List.find_exn cl.methods ~f:(fun md -> equal_method_name md.method_name m)
;;

let rec is_subtype (tbl : class_decl list) (c : class_name) (c' : class_name) =
  if is_object c' || equal_class_name c c'
  then true
  else if is_object c
  then false
  else (
    let cl = find_class tbl c in
    if equal_class_name cl.superclass_name c'
    then true
    else is_subtype tbl cl.superclass_name c')
;;

let rec fields (tbl : class_decl list) (c : class_name) =
  if is_object c
  then []
  else (
    let cl = find_class tbl c in
    cl.params @ fields tbl cl.superclass_name)
;;

let method_body (tbl : class_decl list) (m : method_name) (c : class_name) =
  let cl = find_class tbl c in
  let md = find_method cl m in
  List.map md.fields ~f:snd, md.term
;;

let rec subst (v : string) (t : t) (term : t) : t =
  let go = subst v t in
  match term with
  | Var v' -> if String.equal v v' then t else term
  | FieldAccess (e, f) -> FieldAccess (go e, f)
  | InvokeMethod (e, m, args) -> InvokeMethod (go e, m, List.map ~f:go args)
  | CreateObject (c, args) -> CreateObject (c, List.map ~f:go args)
  | Cast (c, e) -> Cast (c, go e)
;;

let rec is_value (t : t) : bool =
  match t with
  | CreateObject (_, args) -> List.for_all ~f:is_value args
  | _ -> false
;;

exception NoRuleApplies

let rec eval1 (tbl : class_decl list) =
  let rec eval1_args = function
    | [] -> raise NoRuleApplies
    | t :: ts when is_value t -> t :: eval1_args ts
    | t :: ts -> eval1 tbl t :: ts
  in
  function
  | FieldAccess ((CreateObject (class_name, args) as t), field_name) when is_value t ->
    (* E-ProjNew *)
    fields tbl class_name
    |> List.map ~f:snd
    |> List.findi_exn ~f:(Fn.const (equal_field_name field_name))
    |> fst
    |> List.nth_exn args
  | InvokeMethod ((CreateObject (cn, args) as t), mn, args') when is_value t ->
    (* E-InvokeNew *)
    let fields, t = method_body tbl mn cn in
    let t = subst "this" (CreateObject (cn, args)) t in
    List.fold_right2_exn fields args' ~init:t ~f:(Fn.compose subst string_of_field_name)
  | Cast (up_cn, (CreateObject (cn, args) as t))
    when is_value t && is_subtype tbl cn up_cn ->
    (* E-CastNew *)
    CreateObject (cn, args)
  | FieldAccess (t, fn) -> FieldAccess (eval1 tbl t, fn) (* E-Field *)
  | InvokeMethod (t, mn, args) when is_value t ->
    (* E-InvokeArg *)
    InvokeMethod (t, mn, eval1_args args)
  | InvokeMethod (t, mn, args) -> InvokeMethod (eval1 tbl t, mn, args) (* E-InvokeRecv *)
  | CreateObject (cn, args) -> CreateObject (cn, eval1_args args) (* E-New-Arg *)
  | Cast (cn, t) -> Cast (cn, eval1 tbl t) (* E-Cast *)
  | _ -> raise NoRuleApplies
;;

let eval program =
  let (Program (class_decls, t)) = program in
  let rec aux t =
    try aux (eval1 class_decls t) with
    | NoRuleApplies -> t
  in
  aux t
;;

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
  |> Or_error.map ~f:eval
  |> Or_error.sexp_of_t sexp_of_t
  |> print_s
;;

let%expect_test "featherweight java eval pair tests" =
  test "new Pair(new A(), new B()).fst";
  test "new Pair(new A(), new B()).setfst(new B())";
  [%expect
    {|
    (Ok "new A()")
    (Ok "new Pair(new B(), new B())")
    |}];
  test "(Object) new Pair(new A(), new B())";
  test "(B) new Pair(new A(), new B())";
  [%expect
    {|
    (Ok "new Pair(new A(), new B())")
    (Ok "(B) new Pair(new A(), new B())")
    |}]
;;

let%expect_test "featherweight java eval int tests" =
  test "new Succ(new Succ(new Zero()))";
  test "(new Succ(new Succ(new Zero()))).add(new Succ(new Succ(new Zero())))";
  [%expect
    {|
    (Ok "new Succ(new Succ(new Zero()))")
    (Ok "new Succ(new Succ(new Succ(new Succ(new Zero()))))")
    |}]
;;
