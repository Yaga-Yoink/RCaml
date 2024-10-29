open OUnit
open RCaml
open RCaml.Vector

let string_of_string_list lst = "[" ^ String.concat "; " lst ^ "]"

let vector_tests =
  "vector test suite"
  >::: [
         (********** VECTOR TESTS **********)
         ( "" >:: fun _ ->
           assert_equal "()"
             (NumericVector.empty |> NumericVector.string_of_vec)
             ~printer:(fun x -> x) );
         ( "" >:: fun _ ->
           assert_equal "(2., 3.)"
             (NumericVector.init_vec "c(2,3)" |> NumericVector.string_of_vec)
             ~printer:(fun x -> x) );
         ( "" >:: fun _ ->
           assert_equal "(7., 5.)"
             (NumericVector.init_vec "c(2,3)"
             |> NumericVector.add (NumericVector.init_vec "c(5,2)")
             |> NumericVector.string_of_vec)
             ~printer:(fun x -> x) );
         ( "" >:: fun _ ->
           assert_equal "(10., 6.)"
             (NumericVector.init_vec "c(2,3)"
             |> NumericVector.mult (NumericVector.init_vec "c(5,2)")
             |> NumericVector.string_of_vec)
             ~printer:(fun x -> x) );
         ( "" >:: fun _ ->
           assert_equal "(3., 6.)"
             (NumericVector.eval_vec [ "c(1,3)"; "+"; "c(2,3)" ]
             |> NumericVector.string_of_vec)
             ~printer:(fun x -> x) );
         ( "" >:: fun _ ->
           assert_equal "(2., 9.)"
             (NumericVector.eval_vec [ "c(1,3)"; "*"; "c(2,3)" ]
             |> NumericVector.string_of_vec)
             ~printer:(fun x -> x) );
         ( "" >:: fun _ ->
           assert_equal "(2., 9.)"
             (NumericVector.eval_vec [ "VARIABLE_NAME"; "<-"; "c(2,9)" ]
             |> NumericVector.string_of_vec)
             ~printer:(fun x -> x) );
         (********** VALUE TESTS **********)
         ( "" >:: fun _ ->
           assert_equal "1."
             (Value.to_string (Value.eval_val [ "0"; "+"; "1" ])) );
         ( "" >:: fun _ ->
           assert_equal "1.5"
             (Value.to_string (Value.eval_val [ "0.5"; "+"; "1" ]))
             ~printer:Fun.id );
         ( "" >:: fun _ ->
           assert_equal "1."
             (Value.to_string (Value.eval_val [ "2"; "-"; "1" ])) );
         ( "" >:: fun _ ->
           assert_equal "1.5"
             (Value.to_string (Value.eval_val [ "2.5"; "-"; "1" ])) );
         ( "" >:: fun _ ->
           assert_equal "2."
             (Value.to_string (Value.eval_val [ "1"; "*"; "2" ])) );
         ( "" >:: fun _ ->
           assert_equal "1.5"
             (Value.to_string (Value.eval_val [ "0.5"; "*"; "3" ])) );
         ( "" >:: fun _ ->
           assert_equal "2."
             (Value.to_string (Value.eval_val [ "2"; "/"; "1" ])) );
         ( "" >:: fun _ ->
           assert_equal "3."
             (Value.to_string (Value.eval_val [ "4.5"; "/"; "1.5" ])) );
         (********** PROCESSLINES TESTS **********)
         ( "" >:: fun _ ->
           assert_equal [ "7."; "NA"; "6." ]
             (ProcessLines.process_input
                [
                  [ "3"; "+"; "4" ];
                  [ "x"; "<-"; "5"; "+"; "3" ];
                  [ "x"; "-"; "2" ];
                ])
             ~printer:string_of_string_list );
         ( "" >:: fun _ ->
           assert_equal
             [ "(3.,5.)"; "NA"; "d(7., 9.)" ]
             (ProcessLines.process_input
                [
                  [ "c(1,2)"; "+"; "c(2,3)" ];
                  [ "x"; "<-"; "c(5,7)" ];
                  [ "x"; "+"; "c(2,2)" ];
                ])
             ~printer:string_of_string_list );
       ]

let _ = run_test_tt_main vector_tests
