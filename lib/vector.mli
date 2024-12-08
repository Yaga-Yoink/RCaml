open Interp

type elt = Ast.expr
(** The type of elements in the vector *)

type t
(** The type of a vector of elements. *)

val empty : t
(** [empty] is a vector with no elements. *)

val init_vec : Ast.expr list -> t
(** [init_vec x] is the vector representation of [x]. *)

exception UnequalLength
(** [UnequalLength] is raised when two vectors are unequal in length when they
    are required to be equal. *)

val map : (Ast.expr -> Ast.expr) -> t -> t
(** [map f vec] is the vector [vec] with [f] applied to every element of [vec]. *)

val map2 : (Ast.expr -> Ast.expr -> Ast.expr) -> t -> t -> t
(** [map2 f [a1, ..., an] [b1, ..., bn] ] is [f] applied to every ai and bi. *)

val string_of_vec : (Ast.expr -> string) -> t -> string
(** [string_of_vec string_of x] is the string representation of [x] using the to
    [string_of] function in each element of vector [x]. *)

val expr_of_vector : t -> Ast.expr list
(** [expr_of_vectorx] is the AST representation of the vector [x].*)
