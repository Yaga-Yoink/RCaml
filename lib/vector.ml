open Str

module type Vector = sig
  type elt
  (** The type of an element in the vector. *)

  type t
  (** The type of a vector. *)

  val eval_vec : string list -> t
  (** [eval_vecc x] is the result of creating the vectors in [x] and applying
      the operations in [x]. *)

  val empty : t
  (** [empty] is a vector with no elements. *)

  val init_vec : string -> t
  (** [init_vec x] is the vector representation of [x]. *)

  val string_of_vec : t -> string
  (** [string_of_vec x] is the string representation of [x]. *)

  exception UnequalLength
  (** [UnequalLength] is raised when two vectors are unequal in length when they
      are required to be equal. *)

  val add : t -> t -> t
  (** [add x y] is an elementwise addition of [x] and [y]. Raises:
      [UnequalLength] if [x] and [y] are not the same length. *)

  val mult : t -> t -> t
  (** [mult x y] is an elementwise multiplication of [x] and [y]. Raises:
      [UnequalLength] if [x] and [y] are not the same length.*)
end

module NumericVector : Vector with type elt = float = struct
  type elt = float
  type t = elt list
  (* AF: The list [v1; v2; .... ; vn] represents the vector (v1, v2, ... , vn).
     The list elements are in the same order as the vector elements. The empty
     vector is []. *)
  (* RI: None *)

  let eval_vec x = failwith "TODO"
  let empty = []
  let letter_regex = Str.regexp "[a-zA-Z]+"
  let number_regex = Str.regexp "0-9"

  (* TODO: add check that vector inputted in R is formatted correctly. *)

  (** [list_of_rvec x] is the list of elements in the R vector [x]. *)
  let list_of_rvec x =
    let comma_string = Str.string_after x 2 in
    let comma_string = Str.string_before comma_string (String.length x - 3) in
    String.split_on_char ',' comma_string

  (* TODO: figure out how to match the characters inside of the parentheses*)
  let init_vec x =
    let elem_list = list_of_rvec x in
    elem_list |> List.map (fun num_str -> float_of_string num_str)

  let string_of_vec x =
    let rec elements y =
      match y with
      | [] -> ""
      | h :: [] ->
          let () = print_endline (string_of_float h) in
          string_of_float h
      | h :: t -> string_of_float h ^ ", " ^ elements t
    in
    "(" ^ elements x ^ ")"

  exception UnequalLength

  let add vec1 vec2 = failwith "TODO"
  let mult x y = failwith "TODO"
end
