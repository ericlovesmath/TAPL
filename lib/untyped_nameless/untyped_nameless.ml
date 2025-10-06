open Core
include Types
module U = Untyped_lambda_calculus
module Unique_id = Unique_id.Int ()

type context = string list

let find (ctx : context) (v : string) : int option =
  let rec find' acc = function
    | [] -> None
    | h :: _ when String.equal h v -> Some acc
    | _ :: t -> find' (acc + 1) t
  in
  find' 0 ctx
;;

let rec remove_names (ctx : context) (t : U.t) : t option =
  let open Option.Let_syntax in
  match t with
  | Var v ->
    let%map index = find ctx v in
    Var index
  | Lam (v, t) ->
    let%map t = remove_names (v :: ctx) t in
    Abs t
  | App (f, x) ->
    let%bind f = remove_names ctx f in
    let%bind x = remove_names ctx x in
    return (App (f, x))
;;

let gen_sym () = "x." ^ Unique_id.to_string (Unique_id.create ())

let rec restore_names (ctx : context) (t : t) : U.t option =
  let open Option.Let_syntax in
  match t with
  | Var i ->
    let%map v = List.nth ctx i in
    U.Var v
  | Abs t ->
    let sym = gen_sym () in
    let%map t = restore_names (sym :: ctx) t in
    U.Lam (sym, t)
  | App (f, x) ->
    let%bind f = restore_names ctx f in
    let%bind x = restore_names ctx x in
    return (U.App (f, x))
;;

let shift d t =
  let rec aux c t =
    match t with
    | Var i -> if i >= c then Var (i + d) else Var i
    | Abs t -> Abs (aux (c + 1) t)
    | App (f, x) -> App (aux c f, aux c x)
  in
  aux 0 t
;;

let subst j s t =
  let rec aux c t =
    match t with
    | Var i -> if i = j + c then shift c s else Var i
    | Abs t -> Abs (aux (c + 1) t)
    | App (f, x) -> App (aux c f, aux c x)
  in
  aux 0 t
;;

let rec eval t =
  match t with
  | Abs _ | Var _ -> t
  | App (Abs t, ((Var _ | Abs _) as x)) -> eval (shift (-1) (subst 0 (shift 1 x) t))
  | App (f, x) -> eval (App (eval f, eval x))
;;

(* Examples selected from [Exercise 6.1.1] *)
let%expect_test "remove and restore names" =
  let test s =
    Unique_id.For_testing.reset_counter ();
    let original = U.t_of_sexp (Sexp.of_string s) in
    let nameless = remove_names [] original in
    let renamed = Option.bind ~f:(restore_names []) nameless in
    [%message (nameless : t option) (renamed : U.t option)]
    |> Sexp.to_string_hum
    |> print_endline
  in
  (* simple = λx. x *)
  test "(fun x -> x)";
  [%expect {| ((nameless ((abs 0))) (renamed ((fun x.0 -> x.0)))) |}];
  (* c_0 = λs. λz. z *)
  test "(fun s -> fun z -> z)";
  [%expect {| ((nameless ((abs (abs 0)))) (renamed ((fun x.0 -> (fun x.1 -> x.1))))) |}];
  (* c_2 = λs. λz. s (s z) *)
  test "(fun s -> fun z -> s (s z))";
  [%expect
    {|
    ((nameless ((abs (abs (1 (1 0))))))
     (renamed ((fun x.0 -> (fun x.1 -> (x.0 (x.0 x.1)))))))
    |}];
  (* plus = λm. λn. λs. λz. m s (n z s) *)
  test "(fun m -> fun n -> fun s -> fun z -> m s (n z s))";
  [%expect
    {|
    ((nameless ((abs (abs (abs (abs ((3 1) ((2 0) 1))))))))
     (renamed
      ((fun x.0 ->
        (fun x.1 -> (fun x.2 -> (fun x.3 -> ((x.0 x.2) ((x.1 x.3) x.2)))))))))
    |}];
  (* fix = λf. (λx. f (λy. (x x) y)) (λx. f (λy. (x x) y)) *)
  test "(fun f -> (fun x -> f (fun y -> x x y)) (fun x -> f (fun y -> x x y)))";
  [%expect
    {|
    ((nameless ((abs ((abs (1 (abs ((1 1) 0)))) (abs (1 (abs ((1 1) 0))))))))
     (renamed
      ((fun x.0 ->
        ((fun x.1 -> (x.0 (fun x.2 -> ((x.1 x.1) x.2))))
         (fun x.3 -> (x.0 (fun x.4 -> ((x.3 x.3) x.4)))))))))
    |}];
  (* foo = (λx. (λx. x)) (λx. x) *)
  test "((fun x -> fun x -> x) (fun x -> x))";
  [%expect
    {|
    ((nameless (((abs (abs 0)) (abs 0))))
     (renamed (((fun x.0 -> (fun x.1 -> x.1)) (fun x.2 -> x.2)))))
    |}]
;;

let%quick_test "quickcheck round trip restore and remove names" =
  fun (t : t) ->
  let open Option.Let_syntax in
  assert (
    let result =
      let%bind named = restore_names [] t in
      let%bind nameless = remove_names [] named in
      Some (equal nameless t)
    in
    (* TODO: Quickcheck generator sometimes generates invalid expressions,
       capping [Var i] to [0 <= i <= 5] results in ~7% of inputs being valid.
       Right now, we just ignore invalid inputs.

       Rewrite a proper Quickcheck generator that only emits valid forms. *)
    Option.value result ~default:true)
;;

let test ctx t =
  t
  |> Sexp.of_string
  |> U.t_of_sexp
  |> remove_names ctx
  |> Option.value_exn
  |> eval
  |> sexp_of_t
  |> Sexp.to_string_hum
  |> print_endline
;;

let%expect_test "eval simple" =
  test [ "x" ] "x";
  [%expect "0"];
  test [ "x" ] "(fun x -> x x)";
  [%expect "(abs (0 0))"]
;;

let%expect_test "eval booleans" =
  let tru = "(fun t -> fun f -> t)" in
  let fls = "(fun t -> fun f -> f)" in
  let andb = [%string "(fun b -> fun c -> b c %{fls})"] in
  let test_branch b =
    let branch = "(fun l -> fun m -> fun n -> l m n)" in
    (* We test with [gamma = [ "f"; "t" ]], so ["f" = 0] and ["t" = 1] *)
    test [ "f"; "t" ] [%string "(%{branch} %{b} t f)"]
  in
  test_branch tru;
  test_branch fls;
  [%expect
    {|
    1
    0
    |}];
  test_branch [%string "(%{andb} %{tru} %{tru})"];
  test_branch [%string "(%{andb} %{tru} %{fls})"];
  test_branch [%string "(%{andb} %{fls} %{tru})"];
  test_branch [%string "(%{andb} %{fls} %{fls})"];
  [%expect
    {|
    1
    0
    0
    0
    |}]
;;
