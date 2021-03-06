open OUnit2
open BasicOp
open Commands
open EuclideanAlg
open FrontEnd
open StatOp
open Trig

(** ====================================

    TEST SUITE AND TESTING METHODOLOGY

    ====================================

    We tested all user-accessible operations in our calculator. This
    means we excluded explicitly testing any helper functions, therefore
    we use BlackBox testing. Test cases were hence developed with this
    mind, and were mostly developed manually. Randomized test cases were
    not used.

    The OUnit below tests the following modules directly or indirectly:
    BasicOp Commands EuclideanAlg Dual Derviative StatOp (partially)
    MatrixDual Trig (partially)

    The OUnit does not test the following modules: Author Main Help

    The following modules were tested manually: Main HDerivative StatOp
    (partially) Trig (partially)

    We manually tested the REPL, as it is quite difficult to test it
    with OUnit. However, the Main module (which is the REPL) is mainly a
    hub which refers to other modules with logic within them. Therefore,
    as long as we test the correctness of the operations accessible from
    Main, we can demonstrate the correctness of Main.

    We did not explicitly test anything having to do with Dual Numbers
    (when in Tuple Form), as the facts and properties about Dual Numbers
    are mathematically proven to be correct. We only tested that the AST
    was able to parse a derivative and evaluate it - using dual numbers
    \- successfully. Dual Numbers in matrix form were tested because of
    the additional complexity of the double array data structure.

    Higher order derivatives were tested manually due to some
    difficulties scaling the Taylor Expansion in test cases. In
    addition, any function that required prompting and not direct input
    were not tested manually instead of through OUnit. In addition, any
    function (such as those StatOp and Trig noted above) that required
    prompting and not direct input were not tested manually instead of
    through OUnit. *)

module VariableMap = Map.Make (String)

let list_test name expected_output f input_list =
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
    list_test "the sum of the list [2.;3.;4.] is 9." 9. add_tr
      [ 2.; 3.; 4. ];
    list_test "the sum of the list [-2.;-3.;4.] is -1." (-1.) add_tr
      [ -2.; -3.; 4. ];
    (* multiply_tr function*)
    list_test
      "the result of multiplying all floats in the list [2.;3.;4.] is \
       24."
      24. multiply_tr [ 2.; 3.; 4. ];
    (* divide_tr function*)
    list_test "the result of dividing 6 and 3 is 2." 2. divide_tr
      [ 6.; 3. ];
    list_test "the result of dividing (3/2)/3 is .5" 0.5 divide_tr
      [ 3.; 2.; 3. ];
    list_test "the result of dividing (3/2) is 1.5." 1.5 divide_tr
      [ 3.; 2. ];
    basic_op_test_exception
      "Trying to divide by zero results in a Divide_by_zero exception"
      Division_by_zero divide_tr [ 3.; 2.; 0. ];
    basic_op_test_exception
      "Trying to divide by zero results in a Divide_by_zero exception"
      Division_by_zero divide_tr [ 0.; 3.; 2.; 0. ];
    list_test "Dividing 0. by any integer is always 0" 0. divide_tr
      [ 0.; 2.; 3. ];
    (* subtract_tr function*)
    list_test
      "Subtracting the list [60.;90.;40.] equals 60. -. 90. -. 40. = \
       -70."
      (-70.) subtract_tr [ 60.; 90.; 40. ];
    list_test
      "Subtracting the list [-60.;-90.;40.] equals -60. +. 90. -. 40. \
       = -70."
      (-10.) subtract_tr [ -60.; -90.; 40. ];
    (* factorial_tr function*)
    factorial_test "Factorial of 7 equals 5040" 5040 7 1;
    factorial_test "Factorial of 0 equals 1" 1 0 1;
    factorial_test_exception
      "Factorial of negative numbers raises an Undefined_Input \
       exception"
      BasicOp.Undefined_input (-4) 1;
    factorial_test_exception
      "Factorial of 23 raises an Integer_Overflow exception"
      Integer_overflow 23 1;
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
  | StandardDev t -> "Standard Deviation" ^ print_cmd_args t
  | LinReg -> "Linear Regression"
  | MultiVar -> "Multivariable Function"
  | Poly -> "Poly"
  | Sigma -> "Sigma"
  | Menu -> "Menu"
  | Ans -> "Ans"
  | Derivative -> "This is a derivative"
  | HDerivative -> "This is a derivative to a higher order"
  | Help t -> "Help" ^ t
  | Exit -> "Exit"
  | Pythag -> "Pythagorean"
  | Sin x -> "Sine"
  | Cos x -> "Cosine"
  | Tan x -> "Tangent"

let stat_op_tests =
  [
    list_test "Mean of float list is 3." 3. mean [ 2.; 3.; 4. ];
    list_test "Mean of another float list is 4." 4. mean [ 3.5; 4.5 ];
    list_test "Mean of length one list" 10. mean [ 10. ];
    list_test "Median of odd length list is 5." 5. median
      [ -1.; 5.; 6. ];
    list_test "Median of even length list is 5.5" 5.5 median
      [ -1.; 5.; 6.; 7.5 ];
    list_test "Median of unarranged even length list is 5.5" 5.5 median
      [ 5.; -1.; 6.; 7.5 ];
    list_test "Median of length one list" 10. median [ 10. ];
    list_test "Standard deviation test for list with same numbers" 0.
      standard_deviation [ 1.; 1. ];
    list_test "Standard deviation test for regular list" 1.
      standard_deviation [ 1.; 2.; 3. ];
    list_test "Standard deviation test for list of length 1" 0.
      standard_deviation [ -1. ];
  ]

let trig_test name expected_output f input =
  name >:: fun info ->
  assert_equal expected_output (f input) ~printer:string_of_float

let trig_tests =
  [
    trig_test "sine at 0 degrees is 0." 0. sin 0.;
    trig_test "sine at 90 degrees is 1." 1. sin 90.;
    trig_test "cosine at 0 degrees is 1." 1. cos 0.;
    trig_test "cosine at 180 degrees is -1." (-1.) cos 180.;
    trig_test "tangent at 0 degrees is 0." 0. tan 0.;
  ]

let function_parse_test name expected_output str num =
  name >:: fun _ ->
  assert_equal expected_output
    ((str |> parse |> make_polynomial (ref "0") |> get_fun) num)
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
      Invalid_calculation "5/0+2 *  5";
    binop_test_exception "5++++ raises Undefined_Parse exception"
      Undefined_parse "5++++";
  ]

let parser_ast_test name expected_output input =
  name >:: fun info ->
  assert_equal expected_output (FrontEnd.parse input)

let parser_ast_tests =
  [
    parser_ast_test
      {|The string "x+y" parses to BinOp ((Add), Var (x), Var (y))|}
      (Binop (Add, Var "x", Var "y"))
      "x+y";
    parser_ast_test
      {|The string "x^2+y" parses to BinOp ((Add), Poly (1.,x,2.), Var (y))|}
      (Binop (Add, Poly (1., "x", 2.), Var "y"))
      "x^2+y";
    parser_ast_test
      {|The string "x^2*81" parses to BinOp ((Binop (Mult, Poly (1., "x", 2.), Float 81.))|}
      (Binop (Mult, Poly (1., "x", 2.), Float 81.))
      "x^2 * 81";
    parser_ast_test {|The string "x" parses to  Var ("x")|} (Var "x")
      "x";
    parser_ast_test {|The string "21" parses to  Float ("21")|}
      (Float 21.) "21";
  ]

let parse_test name expected_output input =
  name >:: fun info ->
  assert_equal expected_output
    (Commands.parse input (ref "0"))
    ~printer:print_command

let multivar_test name expected_output input arg_map =
  name >:: fun info ->
  assert_equal expected_output
    ((make_multivar (FrontEnd.parse input) [] |> get_multi_fun) arg_map)

let arg_map1 =
  VariableMap.add "y" 2. (VariableMap.add "x" 3. VariableMap.empty)

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
    multivar_test "multivar test adding x y" 5. "x+y" arg_map1;
    multivar_test "multivar test multiply x y" 6. "x*y" arg_map1;
    multivar_test "multivar test multiply 3x y" 18. "3x*y" arg_map1;
  ]

let deriv_test name expected_output input value =
  name >:: fun info ->
  let polyFunDerivative =
    input |> FrontEnd.parse |> FrontEnd.make_derivative
    |> FrontEnd.get_fun
  in
  assert_equal expected_output
    (value |> polyFunDerivative |> string_of_float)

let derivative_tests =
  [
    deriv_test "Derivative of x^2 at 1.5 is 3" "3." "x^2" 1.5;
    deriv_test "Derivative of x^2 at 0 is 0" "0." "x^2" 0.;
    deriv_test "Derivative of a multiple term polynomial" "70."
      "x^2 + 66x + 3" 2.;
    deriv_test "derivative of constant" "0." "396644" 55.;
    deriv_test "derivative of single variable" "396644." "396644x"
      1234444.;
    deriv_test "derivative of cubic" "53." "x^3 + 5x + 9" 4.;
    deriv_test "derivative of fourthic" "131811524."
      "555x^4 + 123344x + 1245559980" 39.;
    deriv_test "derivative of quintic" "23134410." "2x^5 + 0x^3 + 2" 39.;
    deriv_test "derivative of a lot of zeroes" "0." "0x^3+0x^6" 3.;
    deriv_test "derivative of x^2+5x+193" "9." "x^2+5x+193" 2.;
    deriv_test "derivative of 6x+5" "6." "6x+5" 5.;
    deriv_test "derivative of 9x+2" "9." "9x+2" 0.;
    deriv_test "derivative of 111x^5" "555." "111x^5" 1.;
    deriv_test "derivative of big coeffcient" "499990." "99998x^5" 1.;
    deriv_test "derivative of 8th degree" "8." "x^8" 1.;
    deriv_test "derivative of 5th  + 9th degree" "1232." "x^5+0.5x^9" 2.;
    deriv_test "derivative of simple function" "900." "900x" 3.;
  ]

let make_matrix_test name expected_output dim num =
  name >:: fun info ->
  assert_equal expected_output
    (MatrixDual.MatrixDual.make_matrix dim num)

let matrix_op_test name expected_output t1 t2 func =
  name >:: fun info ->
  let res =
    match func with
    | "add" -> MatrixDual.MatrixDual.matrix_add t1 t2
    | "sub" -> MatrixDual.MatrixDual.matrix_sub t1 t2
    | "mult" -> MatrixDual.MatrixDual.matrix_mult t1 t2
    | "div" -> MatrixDual.MatrixDual.matrix_div t1 t2
    | _ -> failwith "None others"
  in
  assert_equal expected_output res

let matrix_pow_test name expected_output t num =
  name >:: fun info ->
  assert_equal expected_output
    (MatrixDual.MatrixDual.matrix_power t t num)

let m3103 = [| [| 3.; 1. |]; [| 0.; 3. |] |]

let m9609 = [| [| 9.; 6. |]; [| 0.; 9. |] |]

let m2727027 = [| [| 27.; 27. |]; [| 0.; 27. |] |]

let m0123 = [| [| 0.; 1. |]; [| 2.; 3. |] |]

let m1234 = [| [| 1.; 2. |]; [| 3.; 4. |] |]

let scalar_third = MatrixDual.MatrixDual.make_scalar 2 (1. /. 3.)

let m1243 = [| [| 1.; 2. |]; [| 4.; 3. |] |]

let m0123plus1243 = [| [| 1.; 3. |]; [| 6.; 6. |] |]

let m0123sub1243 = [| [| -1.; -1. |]; [| -2.; 0. |] |]

let m0123mult1243 = [| [| 4.; 3. |]; [| 14.; 13. |] |]

let scaled_third_m1234 =
  [| [| 1. /. 3.; 2. /. 3. |]; [| 1.; 4. /. 3. |] |]

let matrix_tests =
  [
    make_matrix_test "Replacement of a 2x2 matrix with 3" m3103 2 3.;
    matrix_op_test "Addition of two simple matrices" m0123plus1243 m0123
      m1243 "add";
    matrix_op_test "Subtraction of two simple matrices" m0123sub1243
      m0123 m1243 "sub";
    matrix_op_test "Product of two simple matrices" m0123mult1243 m0123
      m1243 "mult";
    matrix_pow_test "Cubing a matrix" m2727027 m3103 3;
    matrix_pow_test "Squaring a matrix" m9609 m3103 2;
    matrix_op_test "Scaling by 1/3" scaled_third_m1234 m1234
      scalar_third "div";
  ]

let suite =
  "test suite for operations"
  >::: List.flatten
         [
           basic_op_tests;
           command_tests;
           function_parse_tests;
           stat_op_tests;
           trig_tests;
           parser_ast_tests;
           binop_parse_tests;
           derivative_tests;
           matrix_tests;
         ]

let _ = run_test_tt_main suite
