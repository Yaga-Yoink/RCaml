open Interp

type 'a t
(** The type of a vector of elements. *)

val empty : 'a t
(** [empty] is a vector with no elements. *)

val init_vec : 'a list -> 'a t
(** [init_vec x] is the vector representation of [x]. *)

exception UnequalLength
(** [UnequalLength] is raised when two vectors are unequal in length when they
    are required to be equal. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f vec] is the vector [vec] with [f] applied to every element of [vec]. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f [a1, ..., an] [b1, ..., bn] ] is [f] applied to every ai and bi. *)

val string_of_vec : ('a -> string) -> 'a t -> string
(** [string_of_vec string_of x] is the string representation of [x] using the to
    [string_of] function in each element of vector [x]. *)
