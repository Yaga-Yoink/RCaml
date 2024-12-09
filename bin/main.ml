open Batteries

(* open RCaml.ProcessLines *)
open Interp
open RCaml.EvalAst

let fileProcessor (fileName : string) : Ast.expr list =
  BatFile.lines_of fileName
  |> BatEnum.map (fun line -> line |> Main.parse)
  |> BatList.of_enum

let printToOutput (lst : string list) (output_file : string) : unit =
  try
    let output_channel = open_out output_file in
    List.iter (fun line -> Printf.fprintf output_channel "%s\n" line) lst;
    close_out output_channel;
    Printf.printf "Successfully wrote to file: %s\n" output_file
  with e -> Printf.printf "Error writing to file: %s\n" (Printexc.to_string e)

let print_string_list lst = List.iter (fun s -> print_endline s) lst

let () =
  print_endline
    "Please insert a file to 'data' folder and insert Filename in form: \
     data/fileName. ";
  let fileName = read_line () in
  let lines = fileProcessor fileName in
  let _ = TypeCheck.typecheck_lines lines in
  let sample_output = process_input lines in
  try
    let outputFile = fileName ^ ".evaluated" in
    printToOutput sample_output outputFile;
    print_string_list sample_output;
    print_endline "Find file in data folder for reference."
  with Sys_error msg -> Printf.eprintf "Error: %s\n" msg
