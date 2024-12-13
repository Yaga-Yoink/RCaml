val plot_vectors : Vector.elt list -> Vector.elt list -> string -> unit
(** [plot_vectors [x1, ..., xn] [y1, ..., yn] name] outputs the plot of xi, yi
    with straight line interpolation between points. Side Effects: Outputs a two
    png file with name [name].*)
