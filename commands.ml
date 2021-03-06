type basic_arguments = float list

type command =
  | Ans
  | Add of basic_arguments
  | Multiply of basic_arguments
  | Subtract of basic_arguments
  | Divide of basic_arguments
  | Factorial of int
  | FastExp of (int * int * int list)
  | GCD of (int * int)
  | Mean of basic_arguments
  | Median of basic_arguments
  | StandardDev of basic_arguments
  | Sin of float
  | Cos of float
  | Tan of float
  | Pythag
  | LinReg
  | Poly
  | MultiVar
  | Derivative
  | HDerivative
  | Sigma
  | Menu
  | Help of string
  | Exit

exception Empty

exception Malformed

exception Undefined_input

let supported_ops =
  [
    "add";
    "divide";
    "multiply";
    "subtract";
    "factorial";
    "fast_exp";
    "gcd";
    "mean";
    "median";
    "standarddev";
    "linreg";
    "poly";
    "multi";
  ]

(** [check_supported str] checks if the input is a supported operation*)
let check_supported str =
  List.exists (fun element -> str = element) supported_ops

(** Matches the input with the correct command *)
let check_string_input str args =
  let str = String.lowercase_ascii str in
  if str = "add" then Add args
  else if str = "divide" then Divide args
  else if str = "multiply" then Multiply args
  else if str = "subtract" then Subtract args
  else if str = "mean" then Mean args
  else if str = "median" then Median args
  else if str = "stddev" then StandardDev args
  else raise Malformed

let check_fact str arg =
  let str = String.lowercase_ascii str in
  if str = "factorial" && arg > 0 then Factorial arg
  else if arg < 0 then raise Undefined_input
  else raise Malformed

let check_linear_regression lst1 lst2 =
  let lst1_length = List.length lst1 in
  let lst2_length = List.length lst2 in
  if
    lst1_length < 2 || lst2_length < 2 || not (lst1_length = lst2_length)
  then raise Undefined_input

let bin_to_list str =
  let rec to_list num acc =
    if num >= 1 then to_list (num / 10) ((num mod 10) :: acc) else acc
  in
  let exp = int_of_string str in
  to_list exp []

let check_fast_exp arg_list =
  match arg_list with
  | [ arg1; arg2; arg3 ] ->
      FastExp (int_of_string arg1, int_of_string arg2, bin_to_list arg3)
  | _ -> raise Malformed

let check_gcd = function
  | [ arg1; arg2 ] -> GCD (int_of_string arg1, int_of_string arg2)
  | _ -> raise Malformed

(**[replace_ans str_lst] replaces all instances of "ANS" in the list
   with the string bound to the ans_ref*)
let replace_ans str_lst ans_ref =
  List.map (fun x -> if x = "ANS" then !ans_ref else x) str_lst

(**[str_to_arg_lst str and_ref] takes a single string input and converts
   it to a list of strings. List elements are substrings of str split by
   spaces. All instances of the string "ANS" are replaced by the string
   in the ref ans_ref *)
let str_to_arg_lst str ans_ref =
  replace_ans
    (List.filter
       (function "" -> false | _ -> true)
       (String.split_on_char ' ' str))
    ans_ref

let parse_noinput_helper h =
  let str = String.lowercase_ascii h in
  if str = "exit" then Exit
  else if str = "menu" then Menu
  else if str = "linreg" then LinReg
  else if str = "poly" then Poly
  else if str = "multivar" then MultiVar
  else if str = "derivative" then Derivative
  else if str = "hderivative" then HDerivative
  else if str = "sigma" then Sigma
  else if str = "pythag" then Pythag
  else if not (check_supported h) then raise Malformed
  else raise Undefined_input

let parse_trig_helper h t =
  let str = String.lowercase_ascii h in
  if str = "factorial" then
    check_fact h
      (try match int_of_string t with i -> i
       with Failure s -> raise Undefined_input)
  else if str = "sin" then Sin (float_of_string t)
  else if str = "cos" then Cos (float_of_string t)
  else if str = "tan" then Tan (float_of_string t)
  else if str = "help" then Help t
  else if str = "mean" then Mean [ float_of_string t ]
  else if str = "median" then Median [ float_of_string t ]
  else if str = "stddev" then StandardDev [ float_of_string t ]
  else raise Undefined_input

let parse str ans_ref =
  let str_list = str_to_arg_lst str ans_ref in
  match str_list with
  | [ h ] -> if str = "ANS" then Ans else parse_noinput_helper h
  | [ h; t ] -> parse_trig_helper h t
  | h :: t -> (
      let str = String.lowercase_ascii h in
      if str = "fastexp" then
        try check_fast_exp t with Failure s -> raise Undefined_input
      else if str = "gcd" then
        try check_gcd t with Failure s -> raise Undefined_input
      else
        try check_string_input h (t |> List.map float_of_string)
        with Failure s -> raise Undefined_input)
  | [] -> raise Empty

(** [parse_list str] will try to create a list of floats from a string *)
let parse_list str =
  let str_list =
    List.filter
      (function "" -> false | _ -> true)
      (String.split_on_char ' ' str)
  in
  try match str_list |> List.map float_of_string with list -> list
  with Failure s -> raise Undefined_input
