open Ast

type t
(** The type of the static environment. *)

val empty : t
(** [empty] is an empty static environment with no bindings. *)

val lookup : t -> string -> typ
(** [lookup env x] is binding in [env] of [x]. Raises: Failure if x is not boun
    in [env]. *)

val extend : t -> string -> typ -> t
(** [extend env x ty] is the environment [env] with the binding of [x] to [ty]. *)
