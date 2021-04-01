type basic_arguments = float list

type command =
  | Add of basic_arguments
  | Multiply of basic_arguments
  | Subtract of basic_arguments
  | Divide of basic_arguments
  | Factorial of int
  | Exit

exception Empty

exception Malformed

exception Undefined_Input

let supported_ops =
  [ "add"; "divide"; "multiply"; "subtract"; "factorial" ]

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
  if str = "factorial" then Factorial arg else raise Malformed

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
        ( try match int_of_string t with i -> i
          with Failure s -> raise Undefined_Input )
  | h :: t -> (
      try check_basic_op h (t |> List.map float_of_string)
      with Failure s -> raise Undefined_Input )
  | [] -> raise Empty
