open Batteries

type t = string array array
type mat = int array array

let process_csv (fileName : string) : string array array =
  BatFile.lines_of fileName
  |> BatEnum.map (fun line ->
         line |> String.split_on_char ' '
         |> List.filter (fun word -> word <> "")
         |> BatSet.of_list |> BatSet.elements |> Array.of_list)
  |> BatList.of_enum |> Array.of_list

let set_element (arr : string array array) (row : int) (col : int)
    (new_element : string) : unit =
  try arr.(row).(col) <- new_element
  with Invalid_argument _ -> failwith "Index out of bounds"

let get_element (arr : string array array) (row : int) (col : int) : string =
  try arr.(row).(col)
  with Invalid_argument _ -> failwith "Index out of bounds"

let matrix (vec : int array) nrow ncol : int array array =
  if Array.length vec <> nrow * ncol then failwith "Invalid dimensions"
  else Array.init_matrix nrow ncol (fun i j -> vec.((i * ncol) + j))

let transpose mat =
  Array.init_matrix
    (Array.length mat.(0))
    (Array.length mat)
    (fun i j -> mat.(j).(i))

let get_row mat nrow = mat.(nrow - 1)

let get_col mat ncol =
  Array.init (Array.length mat) (fun n -> mat.(n).(ncol - 1))

let dot_product vec1 vec2 =
  let n = Array.length vec1 in
  if n <> Array.length vec2 then failwith "The lengths of vectors don't match!"
  else Array.fold_right ( + ) (Array.init n (fun i -> vec1.(i) * vec2.(i))) 0

let multiply lmat rmat =
  if Array.length lmat.(0) <> Array.length rmat then
    failwith "Multiplication cannot be performed on these matrices"
  else
    Array.init_matrix (Array.length lmat)
      (Array.length rmat.(0))
      (fun i j -> dot_product (get_row lmat (i + 1)) (get_col rmat (j + 1)))

let inverse mat =
  let n = Array.length mat in
  if n <> Array.length mat.(0) then
    failwith "Matrix must be square to compute its inverse"
  else
    let augmented =
      Array.init n (fun i ->
          Array.append mat.(i) (Array.init n (fun j -> if i = j then 1 else 0)))
    in
    let augmented = Array.map (Array.map float_of_int) augmented in
    for i = 0 to n - 1 do
      let max_row = ref i in
      for k = i + 1 to n - 1 do
        if abs_float augmented.(k).(i) > abs_float augmented.(!max_row).(i) then
          max_row := k
      done;
      let temp = augmented.(i) in
      augmented.(i) <- augmented.(!max_row);
      augmented.(!max_row) <- temp;
      if augmented.(i).(i) = 0.0 then
        failwith "Matrix is singular and cannot be inverted";
      let pivot = augmented.(i).(i) in
      for j = 0 to (2 * n) - 1 do
        augmented.(i).(j) <- augmented.(i).(j) /. pivot
      done;
      for k = 0 to n - 1 do
        if k <> i then
          let factor = augmented.(k).(i) in
          for j = 0 to (2 * n) - 1 do
            augmented.(k).(j) <-
              augmented.(k).(j) -. (factor *. augmented.(i).(j))
          done
      done
    done;
    Array.init n (fun i -> Array.sub augmented.(i) n n)
