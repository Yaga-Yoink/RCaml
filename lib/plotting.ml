open Plplot

exception UnequalLength

(* Function to plot two lists of floats with a specified output file name *)
let plot_vectors (vec1 : float list) (vec2 : float list) (output_file : string)
    =
  if List.length vec1 <> List.length vec2 then raise UnequalLength;

  let x = Array.of_list vec1 in
  let y = Array.of_list vec2 in

  plsdev "png";
  let name = output_file ^ ".png" in
  plsfnam name;

  plinit ();

  let xmin = Array.fold_left min max_float x in
  let xmax = Array.fold_left max min_float x in
  let ymin = Array.fold_left min max_float y in
  let ymax = Array.fold_left max min_float y in
  plenv xmin xmax ymin ymax 0 0;
  pllab "X-axis" "Y-axis" output_file;

  plline x y;

  plend ();
  Printf.printf "Plot saved as %s\n" output_file
