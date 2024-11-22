open Interp

module type Vector' = sig
  type elt
  (** The type of an element in the vector. *)

  type t
  (** The type of a vector. *)

  val empty : t
  (** [empty] is a vector with no elements. *)

  val init_vec : Ast.expr list -> t
  (** [init_vec x] is the vector representation of [x]. *)

  exception UnequalLength
  (** [UnequalLength] is raised when two vectors are unequal in length when they
      are required to be equal. *)

  val add : t -> t -> t
  (** [add x y] is an elementwise addition of [x] and [y]. Raises:
      [UnequalLength] if [x] and [y] are not the same length. *)

  val minus : t -> t -> t
  (** [minus x y] is an elementwise subtraction of [y] from [x]. Raises:
      [UnequalLength] if [x] and [y] are not the same length. *)

  val mult : t -> t -> t
  (** [mult x y] is an elementwise multiplication of [x] and [y]. Raises:
      [UnequalLength] if [x] and [y] are not the same length.*)

  val div : t -> t -> t
  (** [div x y] is an elementwise division of [x] and [y]. Raises:
      [UnequalLength] if [x] and [y] are not the same length. Requires: [y] does
      not equal 0. *)

  val add_val : t -> elt -> t
  (** [add_val vec v] is the addition of each element in vector [vec] by value
      [v]. *)

  val minus_val : t -> elt -> t
  (** [minus_val vec v] is the subtraction of each element in vector [vec] by
      value [v]. *)

  val mult_val : t -> elt -> t
  (** [mult_val vec v] is the multiplication of each element in vector [vec] by
      value [v]. *)

  val div_val : t -> elt -> t
  (** [div_val vec v] is the division of each element in vector [vec] by value
      [v]. Requires: [v] does not equal 0. *)

  val string_of_vec : t -> string
  (** [string_of_vec x] is the string representation of [x]. *)
end

module ValueVector : Vector' with type elt = Value.t = struct
  type elt = Value.t
  type t = elt list
  (* AF: The list [v1; v2; .... ; vn] represents the vector (v1, v2, ... , vn).
     The list elements are in the same order as the vector elements. The empty
     vector is []. *)
  (* RI: None *)

  let empty = []

  let rec init_vec (x : Ast.expr list) =
    (*x*)
    match x with
    | [] -> []
    | h :: t -> Value.value_of_expr h :: init_vec t

  let string_of_vec x =
    let rec elements = function
      | [] -> ""
      | h :: [] -> Value.to_string h
      | h :: t -> Value.to_string h ^ ", " ^ elements t
    in
    "c(" ^ elements x ^ ")"

  exception UnequalLength

  (** [prim_operation vec1 vec2 prim_op] is each element of [vec1] and [vec2]
      after applying [prim_op] on the the element v1 of vec1 and corresponding
      element v1 of vec2. *)
  let prim_operation vec1 vec2 prim_op =
    if List.length vec1 <> List.length vec2 then raise UnequalLength
    else List.map2 (fun elem1 elem2 -> prim_op elem1 elem2) vec1 vec2

  let add vec1 vec2 = prim_operation vec1 vec2 Value.add
  let minus vec1 vec2 = prim_operation vec1 vec2 Value.minus
  let mult vec1 vec2 = prim_operation vec1 vec2 Value.mult
  let div vec1 vec2 = prim_operation vec1 vec2 Value.div

  let add_val vec v =
    prim_operation vec (List.init (List.length vec) (fun _ -> v)) Value.add

  let minus_val vec v =
    prim_operation vec (List.init (List.length vec) (fun _ -> v)) Value.minus

  let mult_val vec v =
    prim_operation vec (List.init (List.length vec) (fun _ -> v)) Value.mult

  let div_val vec v =
    prim_operation vec (List.init (List.length vec) (fun _ -> v)) Value.div
end
