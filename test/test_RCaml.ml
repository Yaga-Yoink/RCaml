open OUnit
open RCaml
open RCaml.Vector

(** [string_of_ast_type typ] is the string representation of [typ]. *)
let rec string_of_ast_type = function
  | Interp.Ast.TFloat -> "TFloat"
  | Interp.Ast.(TVector TFloat) -> "TFVector (TFloat)"
  | Interp.Ast.(TVector x) ->
      Printf.sprintf "TVector (%s)" (string_of_ast_type x)

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

let vector_tests =
  "vector test suite"
  >::: [
         (* ********* VECTOR TESTS ********* *)
         make_simple_test [ "c(  )" ] [ "c()" ];
         make_simple_test [ "c(2,3)" ] [ "c(2., 3.)" ];
         make_simple_test [ "c(2,3) + c(5,2)" ] [ "c(7., 5.)" ];
         make_simple_test [ "c(2,3) * c(5,2)" ] [ "c(10., 6.)" ];
         make_simple_test [ "c(2,4) / c(1, 4)" ] [ "c(2., 1.)" ];
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
         (********** VECTOR-VALUE TESTS **********)
         make_simple_test [ "c(1, 2, 3) + 1" ] [ "c(2., 3., 4.)" ];
         make_simple_test [ "1 + c(0, 1, 2)" ] [ "c(1., 2., 3.)" ];
         make_simple_test [ "c(3, 2, 1) - 1" ] [ "c(2., 1., 0.)" ];
         make_simple_test [ "c(1, 2, 3) * 2" ] [ "c(2., 4., 6.)" ];
         make_simple_test [ "3 * c(3, 2, 1)" ] [ "c(9., 6., 3.)" ];
         make_simple_test [ "c(10, 8, 6) / 2" ] [ "c(5., 4., 3.)" ];
         (* RECURSIVE OPERATIONS INSIDE VECTOR *)
         make_simple_test [ "c((2+ 8), 8, 6) / 2" ] [ "c(5., 4., 3.)" ];
         make_simple_test
           [ "c((2+ 8), 8, ((10/2) + 1)) / 2" ]
           [ "c(5., 4., 3.)" ];
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
         make_invalid_type_check_test [ "4 <- c(1,2)" ]
           Interp.TypeCheck.non_var_assignment_e;
         make_invalid_type_check_test [ "x <- c(1, c(1,2))" ]
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
       ]

let _ = run_test_tt_main vector_tests
