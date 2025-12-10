open Core
open Sexplib.Sexp

exception NoRuleApplies

module Unnamed = struct
  type t =
    | UUnit
    | UTrue
    | UFalse
    | UTuple of t list
    | UProjTuple of t * int
    | URecord of (string * t) list
    | UProjRecord of t * string
    | UVariant of string * t
    | UMatch of t * (string * t) list
    | USeq of t * t
    | UIf of t * t * t
    | UVar of int
    | UAbs of t
    | UApp of t * t
    | UZero
    | USucc of t
    | UPred of t
    | UIsZero of t
    | UFix of t
    | URef of t
    | ULoc of int
    | UDeref of t
    | UAssign of int * t
    | UError
  [@@deriving equal]

  let sexp_of_t t =
    let rec parse = function
      | UUnit -> Atom "#u"
      | UTrue -> Atom "#t"
      | UFalse -> Atom "#f"
      | UTuple ts ->
        List
          ([ Atom "{" ]
           @ List.intersperse ~sep:(Atom ",") (List.map ~f:parse ts)
           @ [ Atom "}" ])
      | UProjTuple (t, i) -> List [ parse t; Atom "."; Atom (Int.to_string i) ]
      | URecord record ->
        let sexp_of_fields fields =
          fields
          |> List.map ~f:(fun (l, t) -> [ Atom l; Atom ":"; parse t ])
          |> List.intersperse ~sep:[ Atom "," ]
          |> List.concat
        in
        List ([ Atom "{" ] @ sexp_of_fields record @ [ Atom "}" ])
      | UProjRecord (t, l) -> List [ parse t; Atom "."; Atom l ]
      | UVariant (l, t) -> List [ Atom "<"; Atom l; Atom ":"; parse t; Atom ">" ]
      | UMatch (t, cases) ->
        let sexp_of_case (l, t) = List [ Atom l; Atom "->"; parse t ] in
        List ([ Atom "match"; parse t; Atom "with" ] @ List.map cases ~f:sexp_of_case)
      | USeq (t, t') -> List [ parse t; Atom ";"; parse t' ]
      | UIf (c, t, UUnit) -> List [ Atom "if"; parse c; Atom "then"; parse t ]
      | UIf (c, t, f) ->
        List [ Atom "if"; parse c; Atom "then"; parse t; Atom "else"; parse f ]
      | UVar v -> Atom (Int.to_string v)
      | UAbs t -> List [ Atom "abs"; Atom "."; parse t ]
      | UApp (f, x) -> List [ parse f; parse x ]
      | UZero -> Atom "Z"
      | USucc t -> List [ Atom "S"; parse t ]
      | UPred t -> List [ Atom "pred"; parse t ]
      | UIsZero t -> List [ Atom "iszero"; parse t ]
      | UFix t -> List [ Atom "fix"; parse t ]
      | URef t -> List [ Atom "ref"; parse t ]
      | ULoc i -> Atom (Int.to_string i)
      | UDeref t -> List [ Atom "!"; parse t ]
      | UAssign (i, t) -> List [ Atom (Int.to_string i); Atom ":="; parse t ]
      | UError -> Atom "error"
    in
    parse t
  ;;

  let shift d t =
    let rec walk c = function
      | UVar i -> if i >= c then UVar (i + d) else UVar i
      | UAbs t -> UAbs (walk (c + 1) t)
      | UApp (f, x) -> UApp (walk c f, walk c x)
      | UIf (cond, t, f) -> UIf (walk c cond, walk c t, walk c f)
      | USeq (t, t') -> USeq (walk c t, walk c t')
      | UTuple ts -> UTuple (List.map ~f:(walk c) ts)
      | UProjTuple (t, i) -> UProjTuple (walk c t, i)
      | URecord r -> URecord (List.map ~f:(Tuple2.map_snd ~f:(walk c)) r)
      | UProjRecord (t, l) -> UProjRecord (walk c t, l)
      | UVariant (l, t) -> UVariant (l, walk c t)
      | UMatch (t, cs) ->
        UMatch (walk c t, List.map ~f:(Tuple2.map_snd ~f:(walk (c + 1))) cs)
      | UFix t -> UFix (walk c t)
      | USucc t -> USucc (walk c t)
      | UPred t -> UPred (walk c t)
      | UIsZero t -> UIsZero (walk c t)
      | UUnit -> UUnit
      | UTrue -> UTrue
      | UFalse -> UFalse
      | UZero -> UZero
      | URef t -> URef (walk c t)
      | ULoc i -> ULoc i
      | UDeref t -> UDeref (walk c t)
      | UAssign (v, t2) -> UAssign (v, walk c t2)
      | UError -> UError
    in
    walk 0 t
  ;;

  let subst j s t =
    let rec walk c = function
      | UVar i -> if i = j + c then shift c s else UVar i
      | UAbs t -> UAbs (walk (c + 1) t)
      | UApp (f, x) -> UApp (walk c f, walk c x)
      | UIf (cond, t, f) -> UIf (walk c cond, walk c t, walk c f)
      | USeq (t, t') -> USeq (walk c t, walk c t')
      | UTuple ts -> UTuple (List.map ~f:(walk c) ts)
      | UProjTuple (t, i) -> UProjTuple (walk c t, i)
      | URecord r -> URecord (List.map ~f:(Tuple2.map_snd ~f:(walk c)) r)
      | UProjRecord (t, l) -> UProjRecord (walk c t, l)
      | UVariant (l, t) -> UVariant (l, walk c t)
      | UMatch (t, cs) ->
        UMatch (walk c t, List.map ~f:(Tuple2.map_snd ~f:(walk (c + 1))) cs)
      | UFix t -> UFix (walk c t)
      | USucc t -> USucc (walk c t)
      | UPred t -> UPred (walk c t)
      | UIsZero t -> UIsZero (walk c t)
      | UUnit -> UUnit
      | UTrue -> UTrue
      | UFalse -> UFalse
      | UZero -> UZero
      | URef t -> URef (walk c t)
      | ULoc i -> ULoc i
      | UDeref t -> UDeref (walk c t)
      | UAssign (t1, t2) -> UAssign (t1, walk c t2)
      | UError -> UError
    in
    walk 0 t
  ;;

  let rec is_value = function
    | UAbs _ | UTrue | UFalse | UUnit | UZero | ULoc _ -> true
    | USucc t | UPred t | UVariant (_, t) -> is_value t
    | UTuple ts -> List.for_all ~f:is_value ts
    | URecord r -> List.for_all ~f:(Fn.compose is_value snd) r
    | _ -> false
  ;;

  let is_uerror = function
    | UError -> true
    | _ -> false
  ;;

  let subst_top b t = shift (-1) (subst 0 (shift 1 b) t)
end

module Store : sig
  type 'a t

  include Monad.S with type 'a t := 'a t

  val set : int -> Unnamed.t -> unit t
  val put : Unnamed.t -> int t
  val index : int -> Unnamed.t t
  val fix : ('a -> 'a t) -> 'a -> 'a
end = struct
  module T = struct
    type 'a t = Unnamed.t list -> 'a * Unnamed.t list

    let return x s = x, s

    let bind m ~f s =
      let x, s' = m s in
      f x s'
    ;;

    let map = `Define_using_bind
  end

  include T
  include Monad.Make (T)

  let put x s = List.length s, s @ [ x ]
  let set i x s = (), List.mapi s ~f:(fun i' n -> if i = i' then x else n)
  let index n s = List.nth_exn s n, s

  let fix m x =
    let rec aux x st =
      try
        let x, st = (m x) st in
        aux x st
      with
      | NoRuleApplies -> x
    in
    aux x []
  ;;
end

include Unnamed

let rec eval1 (t : t) : t Store.t =
  let open Store.Let_syntax in
  match t with
  | UApp (UError, _)
  | UApp (_, UError)
  | UIf (UError, _, _)
  | USucc UError
  | UPred UError
  | UIsZero UError
  | USeq (UError, _)
  | USeq (_, UError)
  | UFix UError
  | URef UError
  | UDeref UError
  | UAssign (_, UError)
  | UProjRecord (UError, _)
  | UProjTuple (UError, _)
  | UMatch (UError, _) -> return UError
  | UTuple ts when List.exists ts ~f:is_uerror -> return UError
  | URecord ts when List.exists ts ~f:(Fn.compose is_uerror snd) -> return UError
  | UApp (UAbs f, x) when is_value x -> return (subst_top x f)
  | UApp (f, x) when is_value f ->
    let%map x = eval1 x in
    UApp (f, x)
  | UApp (f, x) ->
    let%map f = eval1 f in
    UApp (f, x)
  | UIf (UTrue, t, _) -> return t
  | UIf (UFalse, _, f) -> return f
  | UIf (c, t, f) ->
    let%map c = eval1 c in
    UIf (c, t, f)
  | USucc t ->
    let%map t = eval1 t in
    USucc t
  | UPred UZero -> return UZero
  | UPred (USucc t) when is_value t -> return t
  | UPred t ->
    let%map t = eval1 t in
    UPred t
  | UIsZero UZero -> return UTrue
  | UIsZero (USucc t) when is_value t -> return UFalse
  | UIsZero t ->
    let%map t = eval1 t in
    UIsZero t
  | USeq (UUnit, t) -> return t
  | USeq (t, t') ->
    let%map t = eval1 t in
    USeq (t, t')
  | UFix (UAbs t) -> return (subst_top (UFix (UAbs t)) t)
  | UFix t ->
    let%map t = eval1 t in
    UFix t
  | UProjRecord (URecord r, l) -> return (List.Assoc.find_exn r ~equal:String.equal l)
  | UProjRecord (t, l) ->
    let%map t = eval1 t in
    UProjRecord (t, l)
  | UVariant (v, t) when not (is_value t) ->
    let%map t = eval1 t in
    UVariant (v, t)
  | UTuple ts ->
    let rec step = function
      | [] -> raise NoRuleApplies
      | t :: ts when not (is_value t) ->
        let%map t = eval1 t in
        t :: ts
      | t :: ts ->
        let%map ts = step ts in
        t :: ts
    in
    let%map ts = step ts in
    UTuple ts
  | URecord ts ->
    let rec step = function
      | [] -> raise NoRuleApplies
      | (l, t) :: ts when not (is_value t) ->
        let%map t = eval1 t in
        (l, t) :: ts
      | t :: ts ->
        let%map ts = step ts in
        t :: ts
    in
    let%map ts = step ts in
    URecord ts
  | UProjTuple (UTuple ts, i) -> return (List.nth_exn ts i)
  | UProjTuple (t, i) ->
    let%map t = eval1 t in
    UProjTuple (t, i)
  | UMatch (UVariant (l, v), cases) when is_value v ->
    return (subst_top v (List.Assoc.find_exn cases ~equal:String.equal l))
  | UMatch (t, cases) ->
    let%map t = eval1 t in
    UMatch (t, cases)
  | URef t when not (is_value t) ->
    let%map t = eval1 t in
    URef t
  | URef v when is_value v ->
    let%map l = Store.put v in
    ULoc l
  | UDeref t when not (is_value t) ->
    let%map t = eval1 t in
    UDeref t
  | UDeref (ULoc i) -> Store.index i
  | UAssign (i, v) when is_value v ->
    let%bind () = Store.set i v in
    return UUnit
  | UAssign (i, v) ->
    let%map v = eval1 v in
    UAssign (i, v)
  | _ -> raise NoRuleApplies
;;

let eval t = Store.fix eval1 t
