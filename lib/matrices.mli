type t

val process_csv : string -> string array array
(** [process_csv fileName] reads a CSV file named [fileName], where elements are
    separated by whitespace. It processes the content and returns it as a
    two-dimensional array of strings, with each row representing a line in the
    CSV and each element in the row representing a field. *)

val set_element : string array array -> int -> int -> string -> unit
(** [set_element arr row col new_element] sets the element at the specified
    [row] and [col] in the two-dimensional string array [arr] to [new_element].
    This function mutates [arr] in-place without returning a new array. Raises
    an exception if [row] or [col] is out of bounds. *)

val get_element : string array array -> int -> int -> string
(** [get_element arr row col] returns the element at the specified [row] and
    [col] in the two-dimensional string array [arr]. Raises an exception if
    [row] or [col] is out of bounds. *)
