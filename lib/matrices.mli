type t
type mat

val process_csv : string -> string array array
(** [process_csv fileName] reads a CSV file named [fileName], where elements are
    separated by whitespace. It processes the content and returns it as a
    two-dimensional array of strings, with each row representing a line in the
    CSV and each element in the row representing a field. *)

val matrix : int array -> int -> int -> mat
(** [matrix vec nrow ncol] returns a matrix with [nrow] rows and [ncol] columns,
    initialized by row from input vector [vec]. Raises failure if the matrix
    dimensions are impossible given the vector size. *)

val transpose : mat -> mat
(** [transpose mat] returns the transpose of matrix [mat]. *)

val get_row : mat -> int -> int array
(** [get_row mat nrow] gets the [nrow] row from [mat]. *)

val get_col : mat -> int -> int array
(** [get_col mat ncol] gets the [ncol] col from [mat]. *)

val dot_product : int array -> int array -> int
(** [dot_product vec1 vec2] returns the dot product of [vec1] and [vec2]. *)

val multiply : mat -> mat -> mat
(** [multiply lmat rmat] returns the product of [lmat] and [rmat] where [lmat]
    is the left matrix and [rmat] is the right matrix. *)

val inverse : mat -> float array array
(** [inverse mat] returns the inverse of [mat]. *)

val set_element : string array array -> int -> int -> string -> unit
(** [set_element arr row col new_element] sets the element at the specified
    [row] and [col] in the two-dimensional string array [arr] to [new_element].
    This function mutates [arr] in-place without returning a new array. Raises
    an exception if [row] or [col] is out of bounds. *)

val get_element : string array array -> int -> int -> string
(** [get_element arr row col] returns the element at the specified [row] and
    [col] in the two-dimensional string array [arr]. Raises an exception if
    [row] or [col] is out of bounds. *)
