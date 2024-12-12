(** The type of binary operators. *)
type bop =
  | Add
  | Minus
  | Mult
  | Div
  | And
  | Or

(** The type of unary operators. *)
type unop = Not

(** The type of the abstract syntax tree. *)
type expr =
  | Float of float
  | Var of string
  | Binop of bop * expr * expr
  | Unop of unop * expr
  | Vector of expr list
  | Assignment of expr * expr
  | Function of expr * expr list * expr list
  | Return of expr
  | Bool of bool
  | Readcsv of expr
  | Plot of expr * expr * expr

(** The type representing R types. *)
type typ =
  | TFloat
  | TBool
  | TString
  | TVector of typ
