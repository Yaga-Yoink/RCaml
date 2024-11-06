type bop =
  | Add
  | Minus
  | Mult
  | Div

type expr =
  | Float of float
  | Var of string
  | Binop of bop * expr * expr
  | Vector of expr list
  | Assignment of expr * expr
