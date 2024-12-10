open Ast

type t
(** The type of the dynamic environment. *)

val empty : t
(** [empty] is an empty static environment with no bindings. *)

val lookup : t -> string -> expr
(** [lookup env x] is binding in [env] of [x]. Raises: Failure if x is not bound
    in [env]. *)

val extend : t -> string -> expr -> t
(** [extend env x e] is the environment [env] with the binding of [x] to [e]. *)
