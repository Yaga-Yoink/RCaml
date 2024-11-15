open Interp

val process_input : Ast.expr list -> string list
(** [process_input lst] processes a list of lines [lst], which are abstract
    syntax trees, using the global variable hashmap for assignments and returns
    a list of results. *)
