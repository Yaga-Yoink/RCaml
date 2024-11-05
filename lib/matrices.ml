open Batteries

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
