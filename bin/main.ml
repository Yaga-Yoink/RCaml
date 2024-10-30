open Batteries
open RCaml.ProcessLines

let fileProcessor (fileName : string) : string list list =
  BatFile.lines_of fileName
  |> BatEnum.map (fun line ->
         line |> String.split_on_char ' '
         |> List.filter (fun word -> word <> ""))
  |> BatList.of_enum

let printToOutput (lst : string list) (output_file : string) : unit =
  try
    let output_channel = open_out output_file in
    List.iter
      (fun line ->
        if line <> "NA" then Printf.fprintf output_channel "%s\n" line)
      lst;
    close_out output_channel;
    Printf.printf "Successfully wrote to file: %s\n" output_file
  with e -> Printf.printf "Error writing to file: %s\n" (Printexc.to_string e)

let print_string_list lst =
  List.iter (fun s -> if s <> "NA" then print_endline s) lst

let () =
  print_endline
    "Please insert a file to 'data' folder and insert Filename in form: \
     data/fileName. ";
  let fileName = read_line () in
  let sample_output = process_input (fileProcessor fileName) in
  try
    let outputFile = fileName ^ ".evaluated" in
    printToOutput sample_output outputFile;
    print_string_list sample_output;
    print_endline "Find file in data folder for reference."
  with Sys_error msg -> Printf.eprintf "Error: %s\n" msg
