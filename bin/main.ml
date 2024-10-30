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
    List.iter (fun line -> Printf.fprintf output_channel "%s\n" line) lst;
    close_out output_channel;
    Printf.printf "Successfully wrote to file: %s\n" output_file
  with e -> Printf.printf "Error writing to file: %s\n" (Printexc.to_string e)

let () =
  let sample_output = process_input (fileProcessor "data/sample.txt") in
  let outputFile = "data/sample_corrected.txt" in
  printToOutput sample_output outputFile
