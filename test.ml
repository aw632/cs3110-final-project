open OUnit2
open BasicOp
open Commands
open EuclideanAlg
open FrontEnd

let basic_op_test name expected_output f input_list =
  name >:: fun info ->
  assert_equal expected_output (f input_list) ~printer:string_of_float

let factorial_test name expected_output (num : int) (acc : int) =
  name >:: fun info ->
  assert_equal expected_output (factorial_tr num acc)
    ~printer:string_of_int

let fast_exp_test
    name
    expected_output
    (m : int)
    (n : int)
    (bin_list : int list) =
  name >:: fun info ->
  assert_equal expected_output
    (fast_exp m n bin_list 1)
    ~printer:string_of_int

let basic_op_test_exception name exception_raised f input_list =
  name >:: fun info ->
  assert_raises exception_raised (fun () -> f input_list)

let factorial_test_exception name exception_raised num acc =
  name >:: fun info ->
  assert_raises exception_raised (fun () -> factorial_tr num acc)

let basic_op_tests =
  [
    (* add_tr function*)
    basic_op_test "the sum of the list [2.;3.;4.] is 9." 9. add_tr
      [ 2.; 3.; 4. ];
    basic_op_test "the sum of the list [-2.;-3.;4.] is -1." (-1.) add_tr
      [ -2.; -3.; 4. ];
    (* multiply_tr function*)
    basic_op_test
      "the result of multiplying all floats in the list [2.;3.;4.] is \
       24."
      24. multiply_tr [ 2.; 3.; 4. ];
    (* divide_tr function*)
    basic_op_test "the result of dividing 6 and 3 is 2." 2. divide_tr
      [ 6.; 3. ];
    basic_op_test "the result of dividing (3/2)/3 is .5" 0.5 divide_tr
      [ 3.; 2.; 3. ];
    basic_op_test "the result of dividing (3/2) is 1.5." 1.5 divide_tr
      [ 3.; 2. ];
    basic_op_test_exception
      "Trying to divide by zero results in a Divide_by_zero exception"
      Division_by_zero divide_tr [ 3.; 2.; 0. ];
    basic_op_test_exception
      "Trying to divide by zero results in a Divide_by_zero exception"
      Division_by_zero divide_tr [ 0.; 3.; 2.; 0. ];
    basic_op_test "Dividing 0. by any integer is always 0" 0. divide_tr
      [ 0.; 2.; 3. ];
    (* subtract_tr function*)
    basic_op_test
      "Subtracting the list [60.;90.;40.] equals 60. -. 90. -. 40. = \
       -70."
      (-70.) subtract_tr [ 60.; 90.; 40. ];
    basic_op_test
      "Subtracting the list [-60.;-90.;40.] equals -60. +. 90. -. 40. \
       = -70."
      (-10.) subtract_tr [ -60.; -90.; 40. ];
    (* factorial_tr function*)
    factorial_test "Factorial of 7 equals 5040" 5040 7 1;
    factorial_test "Factorial of 0 equals 1" 1 0 1;
    factorial_test_exception
      "Factorial of negative numbers raises an Undefined_Input \
       exception"
      BasicOp.Undefined_Input (-4) 1;
    factorial_test_exception
      "Factorial of 23 raises an Integer_Overflow exception"
      Integer_Overflow 23 1;
    fast_exp_test "480272 ^ 293 mod 487001 is 2024" 2024 480272 487001
      [ 1; 0; 0; 1; 0; 0; 1; 0; 1 ];
    fast_exp_test "7 ^ 64 mod 2399 is 763" 763 7 2399
      [ 1; 0; 0; 0; 0; 0; 0 ];
  ]

let print_cmd_args lst =
  " [" ^ String.concat ", " (lst |> List.map string_of_float) ^ " ]"

let print_int_lst lst =
  " [" ^ String.concat ", " (lst |> List.map string_of_int) ^ " ]"

let print_command cmd =
  match cmd with
  | Add t -> "Add" ^ print_cmd_args t
  | Multiply t -> "Multiply" ^ print_cmd_args t
  | Subtract t -> "Subtract" ^ print_cmd_args t
  | Divide t -> "Divide" ^ print_cmd_args t
  | Factorial t -> "Factorial" ^ string_of_int t
  | FastExp (m, n, bin_list) ->
      "FastExp " ^ string_of_int m ^ " " ^ string_of_int n ^ " "
      ^ print_int_lst bin_list
  | Exit -> "Exit"

let parse_test name expected_output input =
  name >:: fun info ->
  assert_equal expected_output (Commands.parse input)
    ~printer:print_command

let function_parse_test n i s = n >:: fun _ -> assert_equal i (interp s)

let function_parse_tests = []

let command_tests =
  [
    parse_test "the command add 3 4 parses to the command Add [3.;4.]"
      (Add [ 3.; 4. ])
      "add 3 4";
    parse_test "the command add 3 4. parses to the command Add [3.;4.]"
      (Add [ 3.; 4. ])
      "add 3 4.";
    parse_test
      "the command divide 3 4 4.5 parses to the command Divide \
       [3.;4.;4.5]"
      (Divide [ 3.; 4.; 4.5 ])
      "divide 3 4 4.5";
    parse_test
      "the command subtract 3 4 4.5 parses to the command Divide \
       [3.;4.;4.5]"
      (Subtract [ 3.; 4.; 4.5 ])
      "subtract 3 4 4.5";
    parse_test
      "The command fastexp 21 2 1101 parses to FastExp (21,2, \
       [1;1;0;1]) "
      (FastExp (21, 2, [ 1; 1; 0; 1 ]))
      "fastexp 21 2 1101";
  ]

let suite =
  "test suite for operations"
  >::: List.flatten [ basic_op_tests; command_tests ]

let _ = run_test_tt_main suite
