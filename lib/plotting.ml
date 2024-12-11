open Plplot

let () =
  (* Set the output device to PNG and specify the file name *)
  plsdev "png";
  plsfnam "output.png";

  (* Initialize PLplot *)
  plinit ();

  (* Set up the plot environment *)
  plenv 0. 10. 0. 10. 0 0;
  pllab "X-axis" "Y-axis" "A Simple Plot";

  (* Data points *)
  let x = [| 0.; 1.; 2.; 3. |] in
  let y = [| 0.; 1.; 4.; 9. |] in

  (* Draw a line connecting the points *)
  plline x y;

  (* Clean up resources *)
  plend ()
