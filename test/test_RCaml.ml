open OUnit
open RCaml
open RCaml.Vector

let string_of_string_list lst = "[" ^ String.concat "; " lst ^ "]"

(** [make_simple_test input output] is the assert_equal OUnit test for comparing
    an [input] and [output] where input is the R code. *)
let make_simple_test input (output : string list) =
  "" >:: fun _ ->
  assert_equal output
    (List.map Interp.Main.parse input |> EvalAst.process_input)
    ~printer:string_of_string_list

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
