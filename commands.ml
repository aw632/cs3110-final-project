type basic_arguments = float list

type command =
  | Add of basic_arguments
  | Multiply of basic_arguments
  | Subtract of basic_arguments
  | Divide of basic_arguments
  | Factorial of int
  | FastExp of (int * int * int list)
  | Lin_Reg
  | Exit

exception Empty

exception Malformed

exception Undefined_Input

let supported_ops =
  [
    "add";
    "divide";
    "multiply";
    "subtract";
    "factorial";
    "fast_exp";
    "lin_reg";
  ]

(** [check_supported str] checks if the input is a supported operation*)
let check_supported str =
  List.exists (fun element -> str = element) supported_ops

(** Matches the input with the correct command *)
let check_basic_op str args =
  let str = String.lowercase_ascii str in
  if str = "add" then Add args
  else if str = "divide" then Divide args
  else if str = "multiply" then Multiply args
  else if str = "subtract" then Subtract args
  else raise Malformed

let check_fact str arg =
  let str = String.lowercase_ascii str in
  if str = "factorial" && arg > 0 then Factorial arg
  else if arg < 0 then raise Undefined_Input
  else raise Malformed

let check_linear_regression lst1 lst2 =
  let lst1_length = List.length lst1 in
  let lst2_length = List.length lst2 in
  if
    lst1_length < 2 || lst2_length < 2 || not (lst1_length = lst2_length)
  then raise Undefined_Input

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
  | [] | [ _ ] | _ :: _ :: _ :: _ :: _ | [ _; _ ] -> raise Malformed

let parse str =
  let str_list =
    List.filter
      (function "" -> false | _ -> true)
      (String.split_on_char ' ' str)
  in
  match str_list with
  | [ h ] ->
      let str = String.lowercase_ascii h in
      if str = "exit" then Exit
      else if str = "linreg" then Lin_Reg
      else if not (check_supported h) then raise Malformed
      else raise Malformed
  | [ h; t ] ->
      check_fact h
        (try match int_of_string t with i -> i
         with Failure s -> raise Undefined_Input)
  | h :: t -> (
      let str = String.lowercase_ascii h in
      if str = "fastexp" then
        try check_fast_exp t with Failure s -> raise Undefined_Input
      else
        try check_basic_op h (t |> List.map float_of_string)
        with Failure s -> raise Undefined_Input)
  | [] -> raise Empty

let parse_list str =
  let str_list =
    List.filter
      (function "" -> false | _ -> true)
      (String.split_on_char ' ' str)
  in
  str_list |> List.map float_of_string
