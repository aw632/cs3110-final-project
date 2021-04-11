type basic_arguments = float list

type command =
  | Add of basic_arguments
  | Multiply of basic_arguments
  | Subtract of basic_arguments
  | Divide of basic_arguments
  | Factorial of int
  | FastExp of (int * int * int list)
  | Exit

exception Empty

exception Malformed

exception Undefined_Input

let supported_ops =
  [ "add"; "divide"; "multiply"; "subtract"; "factorial"; "fast_exp" ]

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
      if h |> String.lowercase_ascii = "exit" then Exit
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
