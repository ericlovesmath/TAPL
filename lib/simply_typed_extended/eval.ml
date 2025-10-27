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
    | EVariant (l, _, t) -> UVariant (l, aux ctx t)
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
    | t -> t
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
    | t -> t
  in
  walk 0 t
;;

let rec is_value = function
  | UAbs _ | UTrue | UFalse | UUnit | UZero -> true
  | USucc t | UPred t | UVariant (_, t) | UProjRecord (t, _) | UProjTuple (t, _) ->
    is_value t
  | UTuple ts -> List.for_all ~f:is_value ts
  | URecord r -> List.for_all ~f:(Fn.compose is_value snd) r
  | _ -> false
;;

let subst_top b t = shift (-1) (subst 0 (shift 1 b) t)

exception NoRuleApplies

let rec eval1 = function
  | UApp (UAbs f, x) when is_value x -> subst_top x f
  | UApp (f, x) when is_value f -> UApp (f, eval1 x)
  | UApp (f, x) -> UApp (eval1 f, x)
  | UIf (UTrue, t, _) -> t
  | UIf (UFalse, _, f) -> f
  | UIf (c, t, f) -> UIf (eval1 c, t, f)
  | USucc t -> USucc (eval1 t)
  | UPred UZero -> UZero
  | UPred (USucc t) when is_value t -> t
  | UPred t -> UPred (eval1 t)
  | UIsZero UZero -> UTrue
  | UIsZero (USucc t) when is_value t -> UFalse
  | UIsZero t -> UIsZero (eval1 t)
  | USeq (UUnit, t) -> t
  | USeq (t, t') -> USeq (eval1 t, t')
  | UFix (UAbs t) -> subst_top (UFix (UAbs t)) t
  | UFix t -> UFix (eval1 t)
  | UProjRecord (URecord r, l) -> List.Assoc.find_exn r ~equal:String.equal l
  | UProjRecord (t, l) -> UProjRecord (eval1 t, l)
  | UTuple ts ->
    let rec step = function
      | [] -> raise NoRuleApplies
      | t :: ts when not (is_value t) -> eval1 t :: ts
      | t :: ts -> t :: step ts
    in
    UTuple (step ts)
  | UProjTuple (UTuple ts, i) -> List.nth_exn ts i
  | UProjTuple (t, i) -> UProjTuple (eval1 t, i)
  | UMatch (UVariant (l, v), cases) when is_value v ->
    subst_top v (List.Assoc.find_exn cases ~equal:String.equal l)
  | UMatch (t, cases) -> UMatch (eval1 t, cases)
  | _ -> raise NoRuleApplies
;;

let rec eval t =
  try eval (eval1 t) with
  | NoRuleApplies ->
    assert (is_value t);
    t
;;
