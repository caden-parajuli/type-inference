type var = string

type expression =
  | Var of var
  | Apply of expression * expression
  | Lambda of var * expression
  | Let of var * expression * expression

