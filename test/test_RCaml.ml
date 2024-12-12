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
  | Interp.Ast.(TMatrix) -> "TMatrix"

let string_of_string_list lst = "[" ^ String.concat "; " lst ^ "]"

(** [string_of_float_array arr] returns the string representation of input array
    [arr]. *)
let string_of_float_array (arr : float array) : string =
  arr |> Array.map string_of_float |> Array.to_list |> String.concat ", "

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
  "" >:: fun _ ->
  assert_equal output
    (Matrices.process_csv input "../data/")
    ~printer:Matrices.string_of_t

(** [make_set_element_test input output row col i] creates a test to check that
    setting value [i] in the matrix in [input] at [row] and [col] gives
    [output]. *)
let make_set_element_test input output row col i =
  let mat = Matrices.process_csv input "../data/" in
  Matrices.set_element mat row col i;
  "" >:: fun _ ->
  assert_equal
    (Matrices.process_csv output "../data/")
    mat ~printer:Matrices.string_of_t

(** [make_set_element_test input output row col i] creates a test to check that
    getting the value at [row] and [col] in the matrix in [input] gives
    [output]. *)
let make_get_element_test input output row col =
  let mat = Matrices.process_csv input "../data/" in
  "" >:: fun _ ->
  assert_equal output
    (Matrices.get_element mat row col)
    ~printer:string_of_float

(** [test_nrow input output] creates a test to check that the number of rows in
    matrix [input] equals [ouput]. *)
let test_nrow input output =
  "" >:: fun _ ->
  assert_equal output (Matrices.nrow input) ~printer:string_of_int

(** [test_ncol input output] creates a test to check that the number of columns
    in matrix [input] equals [ouput]. *)
let test_ncol input output =
  "" >:: fun _ ->
  assert_equal output (Matrices.ncol input) ~printer:string_of_int

(** [test_transpose input output] creates a test to check that the transpose of
    the matrix [input] equals the matrix [output]. *)
let test_transpose input output =
  "" >:: fun _ ->
  assert_equal output (Matrices.transpose input) ~printer:string_of_t

(** [get_row input output] creates a test to check that get_row will return
    array [output] when asked for row [row] of the matrix [input]. *)
let test_get_row input output row =
  "" >:: fun _ ->
  assert_equal output
    (Matrices.get_row input row)
    ~printer:string_of_float_array

(** [get_col input output] creates a test to check that get_col will return
    array [output] when asked for row [row] of the matrix [input]. *)
let test_get_col input output col =
  "" >:: fun _ ->
  assert_equal output
    (Matrices.get_col input col)
    ~printer:string_of_float_array

(** [test_dot_product input1 input2 output] creates a test to check that the dot
    produce of [input1] and [input2] is [output]. *)
let test_dot_product input1 input2 output =
  "" >:: fun _ ->
  assert_equal output
    (Matrices.dot_product input1 input2)
    ~printer:string_of_float

(** [test_add_matrix input1 input2 output] creates a test to check that the sum
    of matrix [input1] and matrix [input2] is matrix [output]. *)
let test_add_matrix input1 input2 output =
  "" >:: fun _ ->
  assert_equal output (Matrices.add input1 input2) ~printer:string_of_t

(** [test_subtract_matrix input1 input2 output] creates a test to check that the
    sum of matrix [input1] and matrix [input2] is matrix [output]. *)
let test_subtract_matrix input1 input2 output =
  "" >:: fun _ ->
  assert_equal output (Matrices.subtract input1 input2) ~printer:string_of_t

let vector_tests =
  [
    (********** VECTOR TESTS **********)
    make_simple_test [ "c(  )" ] [ "c()" ];
    make_simple_test [ "c(2,3)" ] [ "c(2., 3.)" ];
    make_simple_test [ "c(2,3) + c(5,2)" ] [ "c(7., 5.)" ];
    make_simple_test [ "c(2,3) * c(5,2)" ] [ "c(10., 6.)" ];
    make_simple_test [ "c(2,4) / c(1, 4)" ] [ "c(2., 1.)" ];
    make_simple_test [ "c(2,4) - c(1, 4)" ] [ "c(1., 0.)" ];
    make_simple_test
      [ "VARIABLE_NAME <- c(2,9)"; "VARIABLE_NAME" ]
      [ "NA"; "c(2., 9.)" ];
  ]

let value_tests =
  [
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
  ]

let vector_float_tests =
  [
    (********** VECTOR-FLOAT TESTS **********)
    make_simple_test [ "c(1, 2, 3) + 1" ] [ "c(2., 3., 4.)" ];
    make_simple_test [ "1 + c(0, 1, 2)" ] [ "c(1., 2., 3.)" ];
    make_simple_test [ "c(3, 2, 1) - 1" ] [ "c(2., 1., 0.)" ];
    make_simple_test [ "c(1, 2, 3) * 2" ] [ "c(2., 4., 6.)" ];
    make_simple_test [ "3 * c(3, 2, 1)" ] [ "c(9., 6., 3.)" ];
    make_simple_test [ "c(10, 8, 6) / 2" ] [ "c(5., 4., 3.)" ];
    make_simple_test [ "1 - c(3, 2, 1)" ] [ "c(-2., -1., 0.)" ];
    make_simple_test [ "480 / c(10, 8, 6)" ] [ "c(48., 60., 80.)" ];
    ( "init_vec_of_list with valid Float list" >:: fun _ ->
      let vec =
        [ Interp.Ast.Float 1.2; Interp.Ast.Float 3.4; Interp.Ast.Float 5.6 ]
      in
      let expected = [ 1.2; 3.4; 5.6 ] in
      assert_equal expected (init_vec_of_list vec) );
    ( "init_vec_of_list with mixed-type list" >:: fun _ ->
      let vec = [ Interp.Ast.Float 1.2; Interp.Ast.Bool true ] in
      assert_raises (Failure "Expected a float expression") (fun () ->
          init_vec_of_list vec) );
  ]

let vector_bool_tests =
  [
    (********** VECTOR-BOOL TESTS **********)
    make_simple_test [ "!c(TRUE, FALSE, TRUE)" ] [ "c(FALSE, TRUE, FALSE)" ];
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
        "c(TRUE, TRUE, FALSE) | c(FALSE, FALSE, TRUE) | c(FALSE, FALSE, FALSE)";
      ]
      [ "c(TRUE, TRUE, TRUE)" ];
    make_simple_test
      [
        "c(TRUE, TRUE, FALSE) | c(FALSE, FALSE, FALSE) | c(FALSE, FALSE, TRUE)";
      ]
      [ "c(TRUE, TRUE, TRUE)" ];
    make_simple_test
      [
        "c(TRUE, TRUE, FALSE) & c(FALSE, FALSE, FALSE) | c(FALSE, FALSE, TRUE)";
      ]
      [ "c(FALSE, FALSE, TRUE)" ];
    make_simple_test
      [ "TRUE & c(TRUE, FALSE, TRUE)" ]
      [ "c(TRUE, FALSE, TRUE)" ];
    make_simple_test [ "TRUE | c(TRUE, FALSE, TRUE)" ] [ "c(TRUE, TRUE, TRUE)" ];
    make_simple_test
      [ "c(TRUE, FALSE, TRUE) & TRUE" ]
      [ "c(TRUE, FALSE, TRUE)" ];
    make_simple_test
      [ "c(TRUE, FALSE, TRUE) & FALSE" ]
      [ "c(FALSE, FALSE, FALSE)" ];
  ]

let vector_rec_tests =
  [
    (********** RECURSIVE OPERATIONS INSIDE VECTOR **********)
    make_simple_test [ "c((2+ 8), 8, 6) / 2" ] [ "c(5., 4., 3.)" ];
    make_simple_test [ "c((2+ 8), 8, ((10/2) + 1)) / 2" ] [ "c(5., 4., 3.)" ];
    make_simple_test [ "c(TRUE & FALSE)" ] [ "c(FALSE)" ];
    make_simple_test [ "c(TRUE | FALSE)" ] [ "c(TRUE)" ];
    make_simple_test [ "c(TRUE & FALSE | TRUE)" ] [ "c(TRUE)" ];
    make_simple_test [ "c(!TRUE)" ] [ "c(FALSE)" ];
    make_simple_test [ "c(!FALSE)" ] [ "c(TRUE)" ];
  ]

let typecheck_tests =
  [
    (********** TYPE CHECK TESTS **********)
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
  ]

let processlines_tests =
  [
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

let matrix_tests =
  [
    (********** MATRIX TESTS **********)
    make_process_csv_test "sample_csv.csv"
      Matrices.(
        matrix
          (Array.of_list (List.init 16 (fun i -> float_of_int (i + 1))))
          4 4);
    make_set_element_test "sample_csv.csv" "sample_csv2.csv" 2 2 100.;
    make_get_element_test "sample_csv.csv" 15. 4 3;
    make_get_element_test "sample_csv2.csv" 100. 2 2;
    test_ncol (Matrices.process_csv "sample_csv.csv" "../data/") 4;
    test_ncol
      (matrix
         (Array.of_list (List.init 16 (fun i -> float_of_int (2 * i))))
         16 1)
      1;
    test_nrow (Matrices.process_csv "sample_csv.csv" "../data/") 4;
    test_nrow
      (matrix
         (Array.of_list (List.init 16 (fun i -> float_of_int ((i * i) - 1))))
         16 1)
      16;
    test_transpose
      (Matrices.process_csv "t_sample_csv.csv" "../data/")
      (Matrices.process_csv "sample_csv.csv" "../data/");
    test_get_row
      (Matrices.process_csv "sample_csv.csv" "../data/")
      (Array.of_list [ 1.; 2.; 3.; 4. ])
      1;
    test_get_row
      (Matrices.process_csv "sample_csv3.csv" "../data/")
      (Array.of_list [ 18.; 1.; 14.; 18.; 20. ])
      2;
    test_get_col
      (Matrices.process_csv "sample_csv.csv" "../data/")
      (Array.of_list [ 1.; 5.; 9.; 13. ])
      1;
    test_get_col
      (Matrices.process_csv "sample_csv3.csv" "../data/")
      (Array.of_list [ 16.; 18. ])
      4;
    test_dot_product (Array.of_list [ 1.; 2. ]) (Array.of_list [ 3.; 4. ]) 11.;
    test_dot_product
      (Array.of_list [ 0.; 2.2; 6.; 3. ])
      (Array.of_list [ 3.; 4.; 2.; 0. ])
      20.8;
    test_add_matrix
      (Matrices.process_csv "sample_csv.csv" "../data/")
      (Matrices.process_csv "sample_csv2.csv" "../data/")
      (Matrices.process_csv "sum_csv.csv" "../data/");
    test_subtract_matrix
      (Matrices.process_csv "sample_csv.csv" "../data/")
      (Matrices.process_csv "sample_csv2.csv" "../data/")
      (Matrices.process_csv "subtract_csv.csv" "../data/");
    test_subtract_matrix
      (Matrices.process_csv "sample_csv.csv" "../data/")
      (Matrices.process_csv "sample_csv2.csv" "../data/")
      (Matrices.process_csv "subtract_csv.csv" "../data");
  ]

let additional_matrix_tests =
  [
    ( "NotNumMat exception test" >:: fun _ ->
      assert_raises Matrices.NotNumMat (fun () ->
          Matrices.process_csv "invalid_csv.csv") );
    ( "Index out of bounds test for set_element" >:: fun _ ->
      assert_raises (Failure "Index out of bounds") (fun () ->
          let mat = Matrices.process_csv "sample_csv.csv" "../data/" in
          Matrices.set_element mat 10 10 100.0) );
    ( "Index out of bounds test for get_element" >:: fun _ ->
      assert_raises (Failure "Index out of bounds") (fun () ->
          let mat = Matrices.process_csv "sample_csv.csv" "../data/" in
          Matrices.get_element mat 10 10) );
    ( "Matrix multiplication test with valid input" >:: fun _ ->
      let lmat =
        Matrices.matrix (Array.of_list [ 1.; 2.; 3.; 4.; 5.; 6. ]) 2 3
      in
      let rmat =
        Matrices.matrix (Array.of_list [ 7.; 8.; 9.; 10.; 11.; 12. ]) 3 2
      in
      let expected =
        Matrices.matrix (Array.of_list [ 58.; 64.; 139.; 154. ]) 2 2
      in
      assert_equal expected
        (Matrices.multiply lmat rmat)
        ~printer:Matrices.string_of_t );
    ( "Matrix multiplication failure due to incompatible dimensions" >:: fun _ ->
      let lmat =
        Matrices.matrix (Array.of_list [ 1.; 2.; 3.; 4.; 5.; 6. ]) 2 3
      in
      let rmat = Matrices.matrix (Array.of_list [ 7.; 8.; 9.; 10. ]) 2 2 in
      assert_raises
        (Failure "Multiplication cannot be performed on these matrices")
        (fun () -> Matrices.multiply lmat rmat) );
    ( "Matrix inversion test with a valid square matrix" >:: fun _ ->
      let mat = Matrices.matrix (Array.of_list [ 4.; 7.; 2.; 6. ]) 2 2 in
      let expected = [| [| 0.6; -0.7 |]; [| -0.2; 0.4 |] |] in
      let actual = Matrices.inverse mat in
      let epsilon = 1e-6 in
      Array.iteri
        (fun i row ->
          Array.iteri
            (fun j value ->
              let diff = abs_float (value -. expected.(i).(j)) in
              assert_bool
                (Printf.sprintf
                   "Value mismatch at (%d, %d): expected %.6f but got %.6f" i j
                   expected.(i).(j)
                   value)
                (diff < epsilon))
            row)
        actual );
    ( "Matrix inversion failure for non-square matrix" >:: fun _ ->
      let mat =
        Matrices.matrix (Array.of_list [ 1.; 2.; 3.; 4.; 5.; 6. ]) 2 3
      in
      assert_raises (Failure "Matrix must be square to compute its inverse")
        (fun () -> Matrices.inverse mat) );
    ( "Matrix inversion failure for singular matrix" >:: fun _ ->
      let mat = Matrices.matrix (Array.of_list [ 1.; 2.; 2.; 4. ]) 2 2 in
      assert_raises (Failure "Matrix is singular and cannot be inverted")
        (fun () -> Matrices.inverse mat) );
  ]

let test_cases =
  List.flatten
    [
      vector_tests;
      value_tests;
      vector_float_tests;
      vector_bool_tests;
      vector_rec_tests;
      typecheck_tests;
      processlines_tests;
      matrix_tests;
      additional_matrix_tests;
    ]

let test_suite = "RCaml" >::: test_cases
let _ = run_test_tt_main test_suite
