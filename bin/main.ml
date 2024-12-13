open Batteries
open Interp
open RCaml.EvalAst

let print_help () =
  print_endline
    "First, create an .txt or .r file using the specified supported operations \
     and place it in the /data directory of the root.\n\
     Once you run the program, you will be prompted to enter the file you \
     would like to be evaluated.";
  print_endline
    "Supported operations:\n\
     \tValues: + - * / \n\
     \tVectors: + - * / \n\
     \tMatrices: transpose; dot product; multiply; inverse; linear regression; \
     prediction"

let fileProcessor (fileName : string) : Ast.expr list =
  BatFile.lines_of fileName
  |> BatEnum.map (fun line -> line |> Main.parse)
  |> BatList.of_enum

let printToOutput (lst : string list) (output_file : string) : unit =
  try
    let output_channel = open_out output_file in
    List.iter
      (fun line ->
        if line <> "NA" then Printf.fprintf output_channel "%s\n" line)
      lst;
    close_out output_channel;
    Printf.printf "Successfully wrote to file: %s.\nOutput:\n" output_file
  with e -> Printf.printf "Error writing to file: %s\n" (Printexc.to_string e)

let print_string_list lst =
  List.iter (fun s -> if s <> "NA" then print_endline s) lst

let () =
  if Array.length Sys.argv = 2 && String.lowercase_ascii Sys.argv.(1) = "help"
  then print_help ()
  else (
    print_endline
      "Please insert a file to 'data' folder and insert filename in form: \
       data/fileName.";
    try
      let fileName = read_line () in
      let lines = fileProcessor fileName in
      let _ = TypeCheck.typecheck_lines lines in
      let sample_output = process_input lines in
      let outputFile = fileName ^ ".evaluated" in
      printToOutput sample_output outputFile;
      print_string_list sample_output
    with Sys_error msg ->
      print_endline ("Error: " ^ msg);
      print_endline "Run the program with 'help' as a parameter for assistance.")
