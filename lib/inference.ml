open Types
open Expressions
open Unification

type assertion = expression * type_scheme
type assertions = assertion list

let sub_assertions sub (a : assertions) =
  List.map
    (function
      | e, ts -> (e, sub_type_scheme sub ts))
    a

let exclude_var (a : assertions) (x : var) =
  List.filter
    (fun (a1 : assertion) ->
      match a1 with
      | Var v, _ when v <> x -> true
      | _ -> true)
    a

let string_of_assertions asserts =
  List.fold_left
    (fun s -> function
      | e, ts -> s ^ string_of_exp e ^ ":" ^ string_of_type_scheme ts ^ ",")
    "{ " asserts
  ^ " }"

type fresh_var_state = int

let fresh_var (s : fresh_var_state) = (string_of_int s, s + 1)

let rec fresh_names' (t : type_scheme) (s : fresh_var_state)
    (subs : substitution) =
  match t with
  | Quantify (alpha, ts) ->
      let beta, s = fresh_var s in
      fresh_names' ts s ((alpha, TVar beta) :: subs)
  | Type t -> (sub_type subs t, s, subs)

let fresh_names t s =
  let t, s, _ = fresh_names' t s [] in
  (t, s)

let closure a t =
  (* Use a set to make things easier *)
  let free_in_type = free_type_variables t in
  (* Remove the type variables that are not free in the assertions *)
  (* let S = Types.S *)
  let free_in_assertions =
    List.fold_left
      (fun s -> function
        | _, ts -> S.union s @@ free_type_scheme_variables ts)
      S.empty a
  in
  let free_list = S.to_list (S.diff free_in_type free_in_assertions) in
  (* Quantify over the free variables *)
  List.fold_left (fun ts v -> Quantify (v, ts)) (Type t) free_list

(* OCaml's way of doing Haskell's do-notation *)
let ( let* ) x f = Option.bind x f

(** The main algorithm of the paper: Algorithm W *)
let rec algorithm_w (a : assertions) (e : expression)
    (var_state : fresh_var_state) =
  match e with
  | Var _ ->
      let* ts = List.assoc_opt e a in
      let renamed, var_state = fresh_names ts var_state in
      Some ([], renamed, var_state)
  | Apply (e1, e2) ->
      let* s1, t1, var_state = algorithm_w a e1 var_state in
      let* s2, t2, var_state = algorithm_w (sub_assertions s1 a) e2 var_state in
      let beta, var_state = fresh_var var_state in
      let* v = unify (sub_type s2 t1) (Arrow (t2, TVar beta)) in
      Some (List.flatten [ s1; s2; v ], sub_type v (TVar beta), var_state)
  | Lambda (x, e1) ->
      let beta, var_state = fresh_var var_state in
      let new_assertions = (Var x, Type (TVar beta)) :: exclude_var a x in
      let* s1, t1, var_state = algorithm_w new_assertions e1 var_state in
      Some (s1, sub_type s1 @@ Arrow (TVar beta, t1), var_state)
  | Let (x, e1, e2) ->
      let* s1, t1, var_state = algorithm_w a e1 var_state in
      let new_assertions =
        (Var x, closure (sub_assertions s1 a) t1) :: exclude_var a x
      in
      let* s2, t2, var_state = algorithm_w new_assertions e2 var_state in
      Some (List.append s1 s2, t2, var_state)
  | LiteralBool _ ->
    Some ([], Primitive Bool, var_state)
  | LiteralInt _ ->
    Some ([], Primitive Int, var_state)


let infer_with_assumptions a e =
  let* s, t, _ = algorithm_w a e 0 in
  Some (s, t)

let infer = infer_with_assumptions []

