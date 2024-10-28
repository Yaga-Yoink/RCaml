type t
(* The type of the value. *)

val eval_val : string list -> t
(* [eval_val lst] is the evaluation of the values and operations in lst. *)

val to_string : t -> string
(* [to_string v] is the string representation of value t. *)
