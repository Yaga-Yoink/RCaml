open Ast

exception TypeException of string
(** [TypeException message] is the exception raised when a static typing error
    is found with a helpful [message]. *)

val typecheck_lines : expr list -> typ list
(** [typecheck_lines e] checks whether each element of [e] is well-typed
    initially in the empty static environment which maintains assignments from
    previous lines and returns the type of each expression. *)

val non_var_assignment_e : string
(** The error message when the left operand of an assignment is not a variable. *)

val bop_type_mismatch_e : string
(** The error messaage when a binary operator has differing types for the left
    and right operand. *)

val vector_multi_type_e : string
(** The error message when a vector has more than one type of element. *)
