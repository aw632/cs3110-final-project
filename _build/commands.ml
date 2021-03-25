type arguments = float list

type command =
  | Add of arguments
  | Multiply of arguments
  | Subtract of arguments
  | Divide of arguments
  | Exit

exception Empty

exception Malformed

(** [supported_ops] is a list of every operation a user can use. Each
    element corresponds to a specific command*)
let supported_ops = [ "add"; "divide"; "multiply"; "subtract" ]

(** [check_supported str] checks if the input is a supported operation*)
let check_supported str =
  List.exists (fun element -> str = element) supported_ops

(** Matches the input with the correct command *)
let check_op str args =
  if str = "add" then Add args
  else if str = "divide" then Divide args
  else if str = "multiply" then Multiply args
  else if str = "subtract" then Subtract args
  else raise Malformed

let parse str =
  let str_list =
    List.filter
      (function "" -> false | _ -> true)
      (String.split_on_char ' ' str)
  in
  match str_list with
  | [ h ] ->
      if h = "exit" then Exit
      else if not (check_supported h) then raise Malformed
      else raise Malformed
  | h :: t ->
      let args = t |> List.map int_of_string |> List.map float_of_int in
      check_op h args
  | [] -> raise Empty
