type arguments = float list

type command =
  | Add of arguments
  | Multiply of arguments
  | Subtract of arguments
  | Divide of arguments
  | Factorial of int
  | Exit

exception Empty

exception Malformed

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
        (* debugging statement: Note that in the below line, hardcoding
           add works... *)
        (* else if h = "Add" then Add [ 0.; 1. ] *)
      else if not (check_supported h) then raise Malformed
      else raise Malformed
  | [ h; t ] -> check_fact h (int_of_string t)
  | h :: t ->
      let args = t |> List.map float_of_string in
      check_basic_op h args
  | [] -> raise Empty
