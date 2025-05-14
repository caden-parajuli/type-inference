open Expressions

type primitive_type =
  | Bool
  | Int

type type' =
  | TVar of var
  | Primitive of primitive_type
  | Arrow of type' * type'

type type_scheme =
  | Type of type'
  | Quantify of var * type_scheme

type substitution = (var * type') list

module S = Set.Make (struct
  type t = var

  let compare = compare
end)

let free_type_variables t =
  let rec free_vars ty s =
    match ty with
    | TVar x -> S.add x s
    | Arrow (t1, t2) -> S.union (free_vars t1 s) (free_vars t2 s)
    | _ -> s
  in
  free_vars t S.empty

let free_type_scheme_variables ts =
  let rec free_vars ts s =
    match ts with
    | Type t -> S.diff (free_type_variables t) s
    | Quantify (v, ts') -> free_vars ts' @@ S.add v s
  in
  free_vars ts S.empty

let rec is_monotype = function
  | TVar _ -> false
  | Primitive _ -> true
  | Arrow (t1, t2) -> is_monotype t1 && is_monotype t2

let rec string_of_type = function
  | TVar v -> v
  | Primitive p -> (
      match p with
      | Bool -> "bool"
      | Int -> "int")
  | Arrow (t1, t2) -> (
      match t1 with
      | Arrow (_, _) -> "(" ^ string_of_type t1 ^ ") -> " ^ string_of_type t2
      | _ -> string_of_type t1 ^ " -> " ^ string_of_type t2)

let rec string_of_type_scheme = function
    | Quantify (v,ts) -> "∀" ^ v ^ (string_of_type_scheme ts)
    | Type t -> string_of_type t
