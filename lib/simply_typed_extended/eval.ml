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
    | ERef t -> URef (aux ctx t)
    | EDeref t -> UDeref (aux ctx t)
    | EAssign (v, t) -> UAssign (find ctx v, aux ctx t)
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
  in
  walk 0 t
;;

let rec is_value = function
  | UAbs _ | UTrue | UFalse | UUnit | UZero | ULoc _ -> true
  | USucc t | UPred t | UVariant (_, t) | UProjRecord (t, _) | UProjTuple (t, _) ->
    is_value t
  | UTuple ts -> List.for_all ~f:is_value ts
  | URecord r -> List.for_all ~f:(Fn.compose is_value snd) r
  | _ -> false
;;

let subst_top b t = shift (-1) (subst 0 (shift 1 b) t)

exception NoRuleApplies

let rec eval1 (mu : nameless list) = function
  | UApp (UAbs f, x) when is_value x -> mu, subst_top x f
  | UApp (f, x) when is_value f ->
    let mu, x = eval1 mu x in
    mu, UApp (f, x)
  | UApp (f, x) ->
    let mu, f = eval1 mu f in
    mu, UApp (f, x)
  | UIf (UTrue, t, _) -> mu, t
  | UIf (UFalse, _, f) -> mu, f
  | UIf (c, t, f) ->
    let mu, c = eval1 mu c in
    mu, UIf (c, t, f)
  | USucc t ->
    let mu, t = eval1 mu t in
    mu, USucc t
  | UPred UZero -> mu, UZero
  | UPred (USucc t) when is_value t -> mu, t
  | UPred t ->
    let mu, t = eval1 mu t in
    mu, UPred t
  | UIsZero UZero -> mu, UTrue
  | UIsZero (USucc t) when is_value t -> mu, UFalse
  | UIsZero t ->
    let mu, t = eval1 mu t in
    mu, UIsZero t
  | USeq (UUnit, t) -> mu, t
  | USeq (t, t') ->
    let mu, t = eval1 mu t in
    mu, USeq (t, t')
  | UFix (UAbs t) -> mu, subst_top (UFix (UAbs t)) t
  | UFix t ->
    let mu, t = eval1 mu t in
    mu, UFix t
  | UProjRecord (URecord r, l) -> mu, List.Assoc.find_exn r ~equal:String.equal l
  | UProjRecord (t, l) ->
    let mu, t = eval1 mu t in
    mu, UProjRecord (t, l)
  | UTuple ts ->
    let rec step = function
      | [] -> raise NoRuleApplies
      | t :: ts when not (is_value t) ->
        let mu, t = eval1 mu t in
        mu, t :: ts
      | t :: ts ->
        let mu, ts = step ts in
        mu, t :: ts
    in
    let mu, ts = step ts in
    mu, UTuple ts
  | URecord ts ->
    let rec step = function
      | [] -> raise NoRuleApplies
      | (l, t) :: ts when not (is_value t) ->
        let mu, t = eval1 mu t in
        mu, (l, t) :: ts
      | t :: ts ->
        let mu, ts = step ts in
        mu, t :: ts
    in
    let mu, ts = step ts in
    mu, URecord ts
  | UProjTuple (UTuple ts, i) -> mu, List.nth_exn ts i
  | UProjTuple (t, i) ->
    let mu, t = eval1 mu t in
    mu, UProjTuple (t, i)
  | UMatch (UVariant (l, v), cases) when is_value v ->
    mu, subst_top v (List.Assoc.find_exn cases ~equal:String.equal l)
  | UMatch (t, cases) ->
    let mu, t = eval1 mu t in
    mu, UMatch (t, cases)
  | URef t when not (is_value t) ->
    let mu, t' = eval1 mu t in
    mu, URef t'
  | URef v when is_value v ->
    let l = List.length mu in
    let mu' = mu @ [ v ] in
    mu', ULoc l
  | UDeref t when not (is_value t) ->
    let mu, t' = eval1 mu t in
    mu, UDeref t'
  | UDeref (ULoc i) ->
    (match List.nth mu i with
     | Some v -> mu, v
     | None -> failwith "Invalid memory location dereferenced")
  | UAssign (i, v) when is_value v ->
    (match List.nth mu i with
     | Some _ ->
       let mu' = List.mapi mu ~f:(fun j ov -> if j = i then v else ov) in
       mu', UUnit
     | None -> failwith "Invalid memory location assigned")
  | _ -> raise NoRuleApplies
;;

let eval t =
  let rec aux mu t =
    try
      let mu, t = eval1 mu t in
      aux mu t
    with
    | NoRuleApplies when is_value t -> t
  in
  aux [] t
;;
