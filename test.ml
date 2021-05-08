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

let summation_test name expected_output first last f =
  name >:: fun info ->
  assert_equal expected_output (summation_tr first last f)

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
    (*summation function*)
    summation_test "summation (y = 2*x) 0 to 10 is 110" 110. 0. 10.
      (fun x -> 2. *. x);
    summation_test "summation (f = x) 0 to 1 million" 500000500000. 0.
      1_000_000. (fun x -> x);
    summation_test "summation (y = x) 3. to 0. is 0." 0. 3. 0. (fun x ->
        x /. 4.);
    summation_test "summation (y = x^2) -1 to 9. is 286." 286. (-1.) 9.
      (fun x -> x *. x);
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
  | GCD (m, n) -> "GCD " ^ string_of_int m ^ " " ^ string_of_int n
  | Mean t -> "Mean" ^ print_cmd_args t
  | Median t -> "Median" ^ print_cmd_args t
  | Standard_Dev t -> "Standard Deviation" ^ print_cmd_args t
  | Lin_Reg -> "Linear Regression"
  | Poly -> "Poly"
  | Sigma -> "Sigma"
  | Menu -> "Menu"
  | Derivative -> "This is a derivative"
  | Help t -> "Help" ^ t
  | Exit -> "Exit"

let stat_op_test = failwith "TODO"

let stat_op_tests = []

let parse_test name expected_output input =
  name >:: fun info ->
  assert_equal expected_output (Commands.parse input)
    ~printer:print_command

let function_parse_test name expected_output str num =
  name >:: fun _ ->
  assert_equal expected_output
    ((str |> parse |> make_polynomial |> get_fun) num)
    ~printer:string_of_float

let function_parse_tests =
  [
    function_parse_test
      "the string '5x^2 + 3x + 6+ 7x + 34x^3' where x = 1 results in 55"
      55. "5x^2 + 3x + 6+ 7x + 34x^3" 1.;
    function_parse_test "the string '6' where x = 100000 results in 6"
      6. "6" 100000.;
    function_parse_test
      "the string 'x+6' where x = 100000 results in 100006" 100006.
      "x+6" 100000.;
    function_parse_test
      "the string 'x+x+13-x^5+7x' where x = 0 results in 13" 13.
      "x+x+13-x^5+7x" 0.;
    function_parse_test
      "the string 'x+x+13-x^5+7x' where x = 2 results in -1" (-1.)
      "x+x+13-x^5+7x" 2.;
    function_parse_test
      "the string 'x+x+13-x^5+7x' where x = -3 results in 229" 229.
      "x+x+13-x^5+7x" (-3.);
  ]

let binop_parse_test name expected_output str =
  name >:: fun _ ->
  assert_equal expected_output
    (str |> parse |> reduce_bin_op |> get_float)
    ~printer:string_of_float

let binop_test_exception name exception_raised str =
  name >:: fun info ->
  assert_raises exception_raised (fun () ->
      str |> parse |> reduce_bin_op |> get_float)

let binop_parse_tests =
  [
    binop_parse_test "5*(7+3)*5 is 250" 250. "5*(7+3)*5";
    binop_parse_test "5*7+3 *  5 is 50" 50. "5*7+3 *  5";
    binop_parse_test "5/2+2 *  5 is 12.5" 12.5 "5/2+2 *  5";
    binop_parse_test " (5/2) + 2 ^ 5 is 34.5" 34.5 "(5 / 2) + 2 ^ 5";
    binop_test_exception
      "5/0+2 *  5 raises Invalid_Calculation exception"
      Invalid_Calculation "5/0+2 *  5";
    binop_test_exception "5++++ raises Undefined_Parse exception"
      Undefined_Parse "5++++";
  ]

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
  >::: List.flatten
         [
           basic_op_tests;
           command_tests;
           function_parse_tests;
           binop_parse_tests;
         ]

let _ = run_test_tt_main suite
