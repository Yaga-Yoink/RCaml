open Interp

type t
(* The type of the value. *)

(* val eval_val : string list -> t [eval_val lst] is the evaluation of the
   values and operations in lst. *)

val value_of_expr : Ast.expr -> t
(** [value_of_expr x] is value representation of a Ast.expr. Requires: x is an
    Ast.Float. *)

val add : t -> t -> t
(** [add x y] is [x] added to [y]. *)

val minus : t -> t -> t
(** [minus x y] is [y] subtracted from [x]. *)

val mult : t -> t -> t
(** [mult x y] is [x] multiplied by [y]. *)

val div : t -> t -> t
(** [div x y] is [x] divided by [y]. Requires: [y] is not equal to 0.*)

val to_string : t -> string
(* [to_string v] is the string representation of value t. *)
