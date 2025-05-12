open Types
open Option

(** Implementation of Robinson's Unification algorithm, based on Robinson 1965
    page 32, https://dl.acm.org/doi/pdf/10.1145/321250.321253 This is the same
    paper referenced by Damas and Milner *)

let rec uses_var v t =
  match t with
  | Primitive _ -> false
  | TVar v' -> v' = v
  | Arrow (t1, t2) -> uses_var v t1 || uses_var v t2

let rec sub_type sub (t : type') =
  match t with
  | TVar v -> (
      match List.assoc_opt v sub with
      | Some st -> st
      | None -> t)
  | Arrow (t1, t2) -> Arrow (sub_type sub t1, sub_type sub t2)
  | _ -> t

let rec sub_type_scheme sub (ts : type_scheme) =
  match ts with
  | Type t -> Type (sub_type sub t)
  | Quantify (_, ts) -> sub_type_scheme sub ts

let rec unify t1 t2 =
  match (t1, t2) with
  | Primitive x, Primitive y ->
      if x = y then
        Some []
      else
        None
  | TVar v, TVar v' when v = v' -> Some []
  | TVar v, t2' ->
      if uses_var v t2' then
        None
      else
        Some [ (v, t2') ]
  | t1', TVar v ->
      if uses_var v t1' then
        None
      else
        Some [ (v, t1') ]
  | Arrow (t11, t12), Arrow (t21, t22) ->
      (* OCaml's way of doing Haskell's do-notation *)
      let ( let* ) x f = Option.bind x f in
      let* sub1 = unify t11 t21 in
      let* sub2 = unify (sub_type sub1 t12) (sub_type sub1 t22) in
      Some (List.append sub1 @@ sub2)
  | _ -> None

let%test "unify_simple" =
  unify (TVar "x") (Primitive Bool) = Some [ ("x", Primitive Bool) ]

let%test "unify_complex" =
  unify
    (Arrow (TVar "x", TVar "y"))
    (Arrow (Arrow (Primitive Int, Primitive Int), Primitive Bool))
  = Some [ ("x", Arrow (Primitive Int, Primitive Int)); ("y", Primitive Bool) ]

let%test "unify_no_extra" =
  unify
    (Arrow (TVar "x", TVar "y"))
    (Arrow (Arrow (Primitive Int, Primitive Int), TVar "y"))
  = Some [ ("x", Arrow (Primitive Int, Primitive Int)) ]
