open Core
open Types

let find (ctx : string list) (v : string) : int =
  let rec find' acc = function
    | [] -> failwith "failed to find variable"
    | h :: _ when String.equal h v -> acc
    | _ :: t -> find' (acc + 1) t
  in
  find' 0 ctx
;;

let remove_names (t : t) : nameless =
  let rec aux ctx (t : t) : nameless =
    match t with
    | EUnit -> UUnit
    | ETrue -> UTrue
    | EFalse -> UFalse
    | ETuple ts -> UTuple (List.map ts ~f:(aux ctx))
    | EProjTuple (t, i) -> UProjTuple (aux ctx t, i)
    | ERecord r -> URecord (List.map r ~f:(fun (l, t) -> l, aux ctx t))
    | EProjRecord (t, l) -> UProjRecord (aux ctx t, l)
    | EVariant (l, t) -> UVariant (l, aux ctx t)
    | EMatch (t, cases) ->
      UMatch (aux ctx t, List.map cases ~f:(fun (l, v, tcase) -> l, aux (v :: ctx) tcase))
    | ESeq (t, t') -> USeq (aux ctx t, aux ctx t')
    | EIf (c, t, f) -> UIf (aux ctx c, aux ctx t, aux ctx f)
    | ELet (v, b, t) -> UApp (UAbs (aux (v :: ctx) t), aux ctx b)
    | EAbs (v, _, t) -> UAbs (aux (v :: ctx) t)
    | EVar v -> UVar (find ctx v)
    | EApp (f, x) -> UApp (aux ctx f, aux ctx x)
    | EAs (t, _) -> aux ctx t
    | EZero -> UZero
    | ESucc t -> USucc (aux ctx t)
    | EPred t -> UPred (aux ctx t)
    | EIsZero t -> UIsZero (aux ctx t)
    | EFix t -> UFix (aux ctx t)
    | ERef t -> URef (aux ctx t)
    | EDeref t -> UDeref (aux ctx t)
    | EAssign (v, t) -> UAssign (find ctx v, aux ctx t)
    | EError -> UError
  in
  aux [] t
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

let subst_top b t = shift (-1) (subst 0 (shift 1 b) t)

exception NoRuleApplies

module Store : sig
  type 'a t

  include Monad.S with type 'a t := 'a t

  val set : int -> nameless -> unit t
  val put : nameless -> int t
  val index : int -> nameless t
  val fix : ('a -> 'a t) -> 'a -> 'a
end = struct
  module T = struct
    type 'a t = nameless list -> 'a * nameless list

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

let is_uerror = function
  | UError -> true
  | _ -> false
;;

let rec eval1 (t : nameless) : nameless Store.t =
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
