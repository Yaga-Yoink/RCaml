type t
(** The type for matrices consisting of floating point numbers. *)

exception NotNumMat
(** An excpetion raised when a numeric matrix cannot be constructed. *)

val process_csv : string -> string -> t
(** [process_csv fileName] reads a CSV file named [fileName], where elements are
    separated by whitespace. It processes the content and returns it as a
    two-dimensional array of floats, with each row representing a line in the
    CSV and each element in the row representing a field. Raises [NotNumMat] if
    the entries cannot be processes as floats. *)

val matrix : float array -> int -> int -> t
(** [matrix vec nrow ncol] returns a matrix with [nrow] rows and [ncol] columns,
    initialized by row from input vector [vec]. Raises failure if the matrix
    dimensions are impossible given the vector size. *)

val transpose : t -> t
(** [transpose mat] returns the transpose of matrix [mat]. *)

val get_row : t -> int -> float array
(** [get_row mat nrow] gets the [nrow] row from [mat]. *)

val get_col : t -> int -> float array
(** [get_col mat ncol] gets the [ncol] col from [mat]. *)

val nrow : t -> int
(** [nrow mat] returns the number of rows in matrix [mat]. *)

val ncol : t -> int
(** [ncol mat] returns the number of columns in matrix [mat]. *)

val dot_product : float array -> float array -> float
(** [dot_product vec1 vec2] returns the dot product of [vec1] and [vec2]. *)

val add : t -> t -> t
val subtract : t -> t -> t

val multiply : t -> t -> t
(** [multiply lmat rmat] returns the product of [lmat] and [rmat] where [lmat]
    is the left matrix and [rmat] is the right matrix. *)

val inverse : t -> float array array
(** [inverse mat] returns the inverse of [mat]. *)

val set_element : t -> int -> int -> float -> unit
(** [set_element arr row col new_element] sets the element at the specified
    [row] and [col] in the two-dimensional string array [arr] to [new_element].
    This function mutates [arr] in-place without returning a new array. Raises
    an exception if [row] or [col] is out of bounds. *)

val get_element : t -> int -> int -> float
(** [get_element arr row col] returns the element at the specified [row] and
    [col] in the two-dimensional string array [arr]. Raises an exception if
    [row] or [col] is out of bounds. *)

val linear_regression : t -> t -> t
(** [linear_regression obs response] returns the beta/intercept values, an n x 1
    matrix, given the observations in [obs] and the response matrix [response].
*)

val predict : t -> t -> float array -> float
(** [predict obs response new_vals] returns the predicted value given the
    [new_vals] using the model created by [obs] and [response]. *)

val string_of_t : t -> string
val to_expr : t -> Interp.Ast.expr list list
val of_expr : Interp.Ast.expr list list -> t
