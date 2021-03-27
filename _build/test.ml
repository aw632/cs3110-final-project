open OUnit2
open BasicOp

let basic_op_test name expected_output f input_list =
  name >:: fun info ->
  assert_equal expected_output (f input_list 0.)
    ~printer:string_of_float

let basic_op_tests =
  [
    basic_op_test "the sum of the list [2.;3.;4.] is 9." 9.
      BasicOp.add_tr [ 2.; 3.; 4. ];
  ]

let suite =
  "test suite for operations" >::: List.flatten [ basic_op_tests ]

let _ = run_test_tt_main suite
