(** The type of binary operators. *)
type bop =
  | Add
  | Minus
  | Mult
  | Div

(** The type of the abstract syntax tree. *)
type expr =
  | Float of float
  | Var of string
  | Binop of bop * expr * expr
  | Vector of expr list
  | Assignment of expr * expr
  | Function of expr * expr list * expr list
  | Return of expr

(** The type representing R types. *)
type typ =
  | TFloat
  | TVector of typ
