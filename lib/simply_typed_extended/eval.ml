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
    | ELet (v, b, t) -> ULet (v, aux ctx b, aux (v :: ctx) t)
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
    | UVar k -> if k >= c then UVar (k + d) else UVar k
    | UAbs t1 -> UAbs (walk (c + 1) t1)
    | UApp (t1, t2) -> UApp (walk c t1, walk c t2)
    | UIf (t1, t2, t3) -> UIf (walk c t1, walk c t2, walk c t3)
    | USeq (t1, t2) -> USeq (walk c t1, walk c t2)
    | UTuple ts -> UTuple (List.map ~f:(walk c) ts)
    | UProjTuple (t, i) -> UProjTuple (walk c t, i)
    | URecord r -> URecord (List.map ~f:(fun (l, t) -> l, walk c t) r)
    | UProjRecord (t, l) -> UProjRecord (walk c t, l)
    | UVariant (l, t) -> UVariant (l, walk c t)
    | UMatch (t, cases) ->
      UMatch (walk c t, List.map ~f:(fun (l, tcase) -> l, walk (c + 1) tcase) cases)
    | ULet (x, t1, t2) -> ULet (x, walk c t1, walk (c + 1) t2)
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
    | UVar k -> if k = j + c then shift c s else UVar k
    | UAbs t1 -> UAbs (walk (c + 1) t1)
    | UApp (t1, t2) -> UApp (walk c t1, walk c t2)
    | UIf (t1, t2, t3) -> UIf (walk c t1, walk c t2, walk c t3)
    | USeq (t1, t2) -> USeq (walk c t1, walk c t2)
    | UTuple ts -> UTuple (List.map ~f:(walk c) ts)
    | UProjTuple (t, i) -> UProjTuple (walk c t, i)
    | URecord r -> URecord (List.map ~f:(fun (l, t) -> l, walk c t) r)
    | UProjRecord (t, l) -> UProjRecord (walk c t, l)
    | UVariant (l, t) -> UVariant (l, walk c t)
    | UMatch (t, cases) ->
      UMatch (walk c t, List.map ~f:(fun (l, tcase) -> l, walk (c + 1) tcase) cases)
    | ULet (x, t1, t2) -> ULet (x, walk c t1, walk (c + 1) t2)
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

exception NoRuleApplies

let rec eval1 = function
  | UApp (UAbs t12, v2) when is_value v2 -> shift (-1) (subst 0 (shift 1 v2) t12)
  | UApp (v1, t2) when is_value v1 -> UApp (v1, eval1 t2)
  | UApp (t1, t2) -> UApp (eval1 t1, t2)
  | UIf (UTrue, t2, _) -> t2
  | UIf (UFalse, _, t3) -> t3
  | UIf (t1, t2, t3) -> UIf (eval1 t1, t2, t3)
  | USucc t -> USucc (eval1 t)
  | UPred UZero -> UZero
  | UPred (USucc nv1) when is_value nv1 -> nv1
  | UPred t1 -> UPred (eval1 t1)
  | UIsZero UZero -> UTrue
  | UIsZero (USucc nv1) when is_value nv1 -> UFalse
  | UIsZero t1 -> UIsZero (eval1 t1)
  | USeq (UUnit, t2) -> t2
  | USeq (t1, t2) -> USeq (eval1 t1, t2)
  | ULet (_, v1, t2) when is_value v1 -> shift (-1) (subst 0 (shift 1 v1) t2)
  | ULet (x, t1, t2) -> ULet (x, eval1 t1, t2)
  | UFix (UAbs t1) -> shift (-1) (subst 0 (shift 1 (UFix (UAbs t1))) t1)
  | UFix t1 -> UFix (eval1 t1)
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
    let body = List.Assoc.find_exn cases ~equal:String.equal l in
    shift (-1) (subst 0 (shift 1 v) body)
  | UMatch (t, cases) -> UMatch (eval1 t, cases)
  | _ -> raise NoRuleApplies
;;

let rec eval t =
  try eval (eval1 t) with
  | NoRuleApplies -> t
;;
