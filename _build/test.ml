open OUnit2
open BasicOp

let basic_op_test name expected_output f input_list acc =
  name >:: fun info ->
  assert_equal expected_output (f input_list acc)
    ~printer:string_of_float

let basic_op_test_exception name exception_raised f input_list acc =
  name >:: fun info ->
  assert_raises exception_raised (fun () -> f input_list acc)

let basic_op_tests =
  [
    basic_op_test "the sum of the list [2.;3.;4.] is 9." 9. add_tr
      [ 2.; 3.; 4. ] 0.;
    basic_op_test "the sum of the list [-2.;-3.;4.] is -1." (-1.) add_tr
      [ -2.; -3.; 4. ] 0.;
    basic_op_test
      "the result of multiplying all floats in the list [2.;3.;4.] is \
       24."
      24. multiply_tr [ 2.; 3.; 4. ] 1.;
    basic_op_test "the result of dividing 6 and 3 is 2." 2. divide_tr
      [ 6.; 3. ] 0.;
    basic_op_test "the result of dividing (3/2)/3 is .5" 0.5 divide_tr
      [ 3.; 2.; 3. ] 0.;
    basic_op_test "the result of dividing (3/2) is 1.5." 1.5 divide_tr
      [ 3.; 2. ] 0.;
    basic_op_test_exception
      "Trying to divide by zero results in a Divide_by_zero exception"
      Division_by_zero divide_tr [ 3.; 2.; 0. ] 0.;
    basic_op_test_exception
      "Trying to divide by zero results in a Divide_by_zero exception"
      Division_by_zero divide_tr [ 0.; 3.; 2.; 0. ] 0.;
    basic_op_test "Dividing 0. by any integer is always 0" 0. divide_tr
      [ 0.; 2.; 3. ] 0.;
    basic_op_test
      "Subtracting the list [60.;90.;40.] equals 60. -. 90. -. 40. = \
       -70. ;"
      (-70.) subtract_tr [ 60.; 90.; 40. ] 0.;
    basic_op_test
      "Subtracting the list [-60.;-90.;40.] equals 60. -. 90. -. 40. = \
       -70. ;"
      70. subtract_tr [ -60.; -90.; 40. ] 0.;
  ]

let suite =
  "test suite for operations" >::: List.flatten [ basic_op_tests ]

let _ = run_test_tt_main suite
