open Core
open Types

type nameless = Simply_typed_extended.Types.nameless

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
  in
  aux [] t
;;

let eval = Simply_typed_extended.Eval.eval
