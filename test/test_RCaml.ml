open OUnit
open RCaml
open RCaml.Vector
open RCaml.Matrices

(** [string_of_ast_type typ] is the string representation of [typ]. *)
let rec string_of_ast_type = function
  | Interp.Ast.TFloat -> "TFloat"
  | Interp.Ast.(TVector TFloat) -> "TFVector (TFloat)"
  | Interp.Ast.(TVector x) ->
      Printf.sprintf "TVector (%s)" (string_of_ast_type x)
  | Interp.Ast.(TBool) -> "TBool"

let string_of_string_list lst = "[" ^ String.concat "; " lst ^ "]"

(** [string_of_ast_type_lst lst] is the string representation of the AST typ
    list [lst]. *)
let string_of_ast_type_lst lst =
  let ast_str_lst = List.map string_of_ast_type lst in
  string_of_string_list ast_str_lst

(** [make_simple_test input output] is the assert_equal OUnit test for comparing
    an [input] and [output] where input is the R code. *)
let make_simple_test input (output : string list) =
  "" >:: fun _ ->
  assert_equal output
    (List.map Interp.Main.parse input |> EvalAst.process_input)
    ~printer:string_of_string_list

(** [make_type_check_test input output] is the asssert_equal OUnit test for
    comparing a type-checked parsed [input] with an expected [output] of the
    types. *)
let make_type_check_test (input : string list) output =
  "" >:: fun _ ->
  assert_equal output
    (List.map Interp.Main.parse input |> Interp.TypeCheck.typecheck_lines)
    ~printer:string_of_ast_type_lst

(** [make_invalid_type_check_test input] is an OUnit test for checking that an
    incorrectly typed R expression [input] raises a static typecheck error with
    exception message [message]. *)
let make_invalid_type_check_test input message =
  "" >:: fun _ ->
  assert_raises ~msg:(Printf.sprintf "TypeException Raised: %s" message)
    (Interp.TypeCheck.TypeException message) (fun () ->
      List.map Interp.Main.parse input |> Interp.TypeCheck.typecheck_lines)

(** [make_process_csv_test input output] creates a test to check that the
    [input] file returns a matrix equivalent to [output]. *)
let make_process_csv_test input output =
  print_endline (Sys.getcwd ());
  "" >:: fun _ ->
  assert_equal output (Matrices.process_csv input) ~printer:Matrices.string_of_t

let vector_tests =
  "vector test suite"
  >::: [
         (* ********* VECTOR TESTS ********* *)
         make_simple_test [ "c(  )" ] [ "c()" ];
         make_simple_test [ "c(2,3)" ] [ "c(2., 3.)" ];
         make_simple_test [ "c(2,3) + c(5,2)" ] [ "c(7., 5.)" ];
         make_simple_test [ "c(2,3) * c(5,2)" ] [ "c(10., 6.)" ];
         make_simple_test [ "c(2,4) / c(1, 4)" ] [ "c(2., 1.)" ];
         make_simple_test [ "c(2,4) - c(1, 4)" ] [ "c(1., 0.)" ];
         make_simple_test
           [ "VARIABLE_NAME <- c(2,9)"; "VARIABLE_NAME" ]
           [ "NA"; "c(2., 9.)" ];
         (********** VALUE TESTS **********)
         make_simple_test [ "0 + 1" ] [ "1." ];
         make_simple_test [ "0.5 + 1" ] [ "1.5" ];
         make_simple_test [ "2 - 1" ] [ "1." ];
         make_simple_test [ "2.5 - 1" ] [ "1.5" ];
         make_simple_test [ "1*2" ] [ "2." ];
         make_simple_test [ "0.5 * 3" ] [ "1.5" ];
         make_simple_test [ "2/1" ] [ "2." ];
         make_simple_test [ "0.5 + 1" ] [ "1.5" ];
         make_simple_test [ "4.5/ 1.5" ] [ "3." ];
         make_simple_test [ "TRUE" ] [ "TRUE" ];
         make_simple_test [ "FALSE" ] [ "FALSE" ];
         make_simple_test [ "TRUE & TRUE" ] [ "TRUE" ];
         make_simple_test [ "TRUE | TRUE" ] [ "TRUE" ];
         make_simple_test [ "TRUE & FALSE" ] [ "FALSE" ];
         make_simple_test [ "FALSE & TRUE" ] [ "FALSE" ];
         make_simple_test [ "TRUE | FALSE" ] [ "TRUE" ];
         make_simple_test [ "FALSE | TRUE" ] [ "TRUE" ];
         make_simple_test [ "FALSE | TRUE | TRUE" ] [ "TRUE" ];
         make_simple_test [ "FALSE | TRUE | FALSE" ] [ "TRUE" ];
         (* fix parentheses precedence *)
         (* make_simple_test [ "TRUE | (FALSE & TRUE)" ] [ "FALSE" ]; *)
         (********** VECTOR-FLOAT TESTS **********)
         make_simple_test [ "c(1, 2, 3) + 1" ] [ "c(2., 3., 4.)" ];
         make_simple_test [ "1 + c(0, 1, 2)" ] [ "c(1., 2., 3.)" ];
         make_simple_test [ "c(3, 2, 1) - 1" ] [ "c(2., 1., 0.)" ];
         make_simple_test [ "c(1, 2, 3) * 2" ] [ "c(2., 4., 6.)" ];
         make_simple_test [ "3 * c(3, 2, 1)" ] [ "c(9., 6., 3.)" ];
         make_simple_test [ "c(10, 8, 6) / 2" ] [ "c(5., 4., 3.)" ];
         make_simple_test [ "1 - c(3, 2, 1)" ] [ "c(-2., -1., 0.)" ];
         make_simple_test [ "480 / c(10, 8, 6)" ] [ "c(48., 60., 80.)" ];
         (********** VECTOR-BOOL TESTS **********)
         make_simple_test
           [ "!c(TRUE, FALSE, TRUE)" ]
           [ "c(FALSE, TRUE, FALSE)" ];
         make_simple_test
           [ "c(FALSE, FALSE, FALSE) & c(FALSE, FALSE, FALSE)" ]
           [ "c(FALSE, FALSE, FALSE)" ];
         make_simple_test
           [ "c(FALSE, FALSE, FALSE) | c(FALSE, FALSE, FALSE)" ]
           [ "c(FALSE, FALSE, FALSE)" ];
         make_simple_test
           [ "c(FALSE, TRUE, FALSE) | c(FALSE, FALSE, FALSE)" ]
           [ "c(FALSE, TRUE, FALSE)" ];
         make_simple_test
           [ "c(TRUE, TRUE, FALSE) | c(FALSE, FALSE, TRUE)" ]
           [ "c(TRUE, TRUE, TRUE)" ];
         make_simple_test
           [
             "c(TRUE, TRUE, FALSE) | c(FALSE, FALSE, TRUE) | c(FALSE, FALSE, \
              FALSE)";
           ]
           [ "c(TRUE, TRUE, TRUE)" ];
         make_simple_test
           [
             "c(TRUE, TRUE, FALSE) | c(FALSE, FALSE, FALSE) | c(FALSE, FALSE, \
              TRUE)";
           ]
           [ "c(TRUE, TRUE, TRUE)" ];
         make_simple_test
           [
             "c(TRUE, TRUE, FALSE) & c(FALSE, FALSE, FALSE) | c(FALSE, FALSE, \
              TRUE)";
           ]
           [ "c(FALSE, FALSE, TRUE)" ];
         make_simple_test
           [ "TRUE & c(TRUE, FALSE, TRUE)" ]
           [ "c(TRUE, FALSE, TRUE)" ];
         make_simple_test
           [ "TRUE | c(TRUE, FALSE, TRUE)" ]
           [ "c(TRUE, TRUE, TRUE)" ];
         make_simple_test
           [ "c(TRUE, FALSE, TRUE) & TRUE" ]
           [ "c(TRUE, FALSE, TRUE)" ];
         make_simple_test
           [ "c(TRUE, FALSE, TRUE) & FALSE" ]
           [ "c(FALSE, FALSE, FALSE)" ];
         (* RECURSIVE OPERATIONS INSIDE VECTOR *)
         make_simple_test [ "c((2+ 8), 8, 6) / 2" ] [ "c(5., 4., 3.)" ];
         make_simple_test
           [ "c((2+ 8), 8, ((10/2) + 1)) / 2" ]
           [ "c(5., 4., 3.)" ];
         make_simple_test [ "c(TRUE & FALSE)" ] [ "c(FALSE)" ];
         make_simple_test [ "c(TRUE | FALSE)" ] [ "c(TRUE)" ];
         make_simple_test [ "c(TRUE & FALSE | TRUE)" ] [ "c(TRUE)" ];
         make_simple_test [ "c(!TRUE)" ] [ "c(FALSE)" ];
         make_simple_test [ "c(!FALSE)" ] [ "c(TRUE)" ];
         (********** TYPE CHECk TESTS **********)
         make_type_check_test
           [ "x <- 2"; "x <- c(1,2)" ]
           [ Interp.Ast.TFloat; Interp.Ast.TVector Interp.Ast.TFloat ];
         make_type_check_test
           [ "x <- c(1,2)"; "y <- c(3, 4)"; "x + y" ]
           [
             Interp.Ast.TVector Interp.Ast.TFloat;
             Interp.Ast.TVector Interp.Ast.TFloat;
             Interp.Ast.TVector Interp.Ast.TFloat;
           ];
         make_invalid_type_check_test [ "c(1,2) + 3" ]
           Interp.TypeCheck.bop_type_mismatch_e;
         make_invalid_type_check_test [ "TRUE + 3" ]
           Interp.TypeCheck.bop_type_mismatch_e;
         make_invalid_type_check_test [ "FALSE - 3" ]
           Interp.TypeCheck.bop_type_mismatch_e;
         make_invalid_type_check_test [ "4 <- c(1,2)" ]
           Interp.TypeCheck.non_var_assignment_e;
         make_invalid_type_check_test [ "TRUE <- c(1,2)" ]
           Interp.TypeCheck.non_var_assignment_e;
         make_invalid_type_check_test
           [ "FALSE & FALSE <- c(1,2)" ]
           Interp.TypeCheck.non_var_assignment_e;
         make_invalid_type_check_test
           [ "c(FALSE, TRUE) & c(TRUE, FALSE) <- c(1,2)" ]
           Interp.TypeCheck.non_var_assignment_e;
         make_invalid_type_check_test [ "x <- c(1, c(1,2))" ]
           Interp.TypeCheck.vector_multi_type_e;
         make_invalid_type_check_test [ "x <- c(TRUE, 1)" ]
           Interp.TypeCheck.vector_multi_type_e;
         make_invalid_type_check_test
           [ "x <- c(TRUE, c(TRUE, FALSE))" ]
           Interp.TypeCheck.vector_multi_type_e;
         make_invalid_type_check_test
           [ "x <- c(TRUE, c(TRUE & FALSE))" ]
           Interp.TypeCheck.vector_multi_type_e;
         (********** PROCESSLINES TESTS **********)
         ( "" >:: fun _ ->
           assert_equal [ "7."; "NA"; "6." ]
             (EvalAst.process_input
                (List.map Interp.Main.parse [ "3 + 4"; "x <- 5 + 3"; "x - 2" ]))
             ~printer:string_of_string_list );
         ( "" >:: fun _ ->
           assert_equal
             [ "c(3., 5.)"; "NA"; "c(7., 9.)" ]
             (EvalAst.process_input
                (List.map Interp.Main.parse
                   [ "c(1,2) + c(2,3)"; "x <- c(5,7)"; "x + c(2,2)" ]))
             ~printer:string_of_string_list );
         (********** MATRIX TESTS **********)
         make_process_csv_test "sample_csv.csv"
           Matrices.(
             matrix
               (Array.of_list (List.init 16 (fun i -> float_of_int (i + 1))))
               4 4);
       ]

let _ = run_test_tt_main vector_tests
