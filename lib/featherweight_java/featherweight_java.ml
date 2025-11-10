open Core
open Parser
module Types = Types
module Eval = Eval
module Typecheck = Typecheck

let repl (s : string) =
  let t = s |> Lexer.of_string |> Lexer.lex |> Parser.run program_p in
  match t with
  | Error parse_err -> print_s [%message (parse_err : Error.t)]
  | Ok t ->
    (match Typecheck.typecheck t with
     | Error ty_error -> print_s [%message (ty_error : Error.t)]
     | Ok ty ->
       let result = Eval.eval t in
       print_s [%message (ty : Types.class_name) (result : Types.t)])
;;

let preamble_pair =
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
  |}
;;

let%expect_test "featherweight java pair tests" =
  let test t = repl (preamble_pair ^ t) in
  test "new Pair(new A(), new B()).fst";
  test "new Pair(new A(), new B()).setfst(new B())";
  [%expect
    {|
    ((ty Object) (result "new A()"))
    ((ty Pair) (result "new Pair(new B(), new B())"))
    |}]
;;

let%expect_test "featherweight java fail pair tests" =
  let test t = repl (preamble_pair ^ t) in
  test "new Pair(new A())";
  test "new Pair(new B(), new C())";
  test "new Pair((Pair) new A(), new A())";
  [%expect
    {|
    (ty_error "args and fields diff length")
    (ty_error "List.find failed")
    warning: stupid cast
    ((ty Pair) (result "new Pair((Pair) new A(), new A())"))
    |}]
;;

let preamble_integers =
  {|
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

let%expect_test "featherweight java int tests" =
  let test t = repl (preamble_integers ^ t) in
  let two = "new Succ(new Succ(new Zero()))" in
  test two;
  test [%string "(%{two}).add(%{two})"];
  [%expect
    {|
    ((ty Succ) (result "new Succ(new Succ(new Zero()))"))
    ((ty Integer) (result "new Succ(new Succ(new Succ(new Succ(new Zero()))))"))
    |}]
;;

let%expect_test "featherweight java override tests" =
  repl
    {|
     class A extends Object { A() { super(); } }
  
     class B extends A {
       Object f;
  
       B(Object f) {
         super();
         this.f = f;
       }
  
       B setf(Object f) {
         return new B(f);
       }
     }

     new B(new A()).setf(new Object())
   |};
  [%expect {| ((ty B) (result "new B(new Object())")) |}];
  repl
    {|
     class A extends Object {
       A() { super(); }
       A setf() { return new Object(); }
     }
  
     class B extends A {
       Object f;
  
       B(Object f) {
         super();
         this.f = f;
       }
  
       B setf(Object f) {
         return new B(f);
       }
     }

     new B(new A()).setf(new Object())
   |};
  [%expect {| (ty_error ("expr is not expected return type of method" "invalid override")) |}];
;;
