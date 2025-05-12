type var = string

type expression =
  | Var of var
  | Apply of expression * expression
  | Lambda of var * expression
  | Let of var * expression * expression
  | LiteralBool of bool
  | LiteralInt of int

let rec string_of_exp = function
  | Var x -> x
  | Apply (e1, e2) ->
      "(" ^ string_of_exp e1 ^ ")" ^ " (" ^ string_of_exp e2 ^ ")"
  | Lambda (v, e) -> "λ" ^ v ^ "." ^ string_of_exp e
  | Let (v, e1, e2) ->
      "let " ^ v ^ " = " ^ string_of_exp e1 ^ " in " ^ string_of_exp e2
  | LiteralBool b -> string_of_bool b
  | LiteralInt i -> string_of_int i
