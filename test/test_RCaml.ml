open OUnit
open RCaml.Vector

let vector_tests =
  "vector test suite"
  >::: [
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
       ]

let _ = run_test_tt_main vector_tests
