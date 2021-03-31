open BasicOp
open Commands

(** [new_command_query ()] prints the lines below to the console. *)
let rec new_command_query () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Do you want to enter a new operation? (Y/N) \n";
  print_string "> ";
  match read_line () with
  | "Y" -> ask_for_commands ()
  | "N" ->
      ANSITerminal.print_string [ ANSITerminal.green ] "Goodbye!\n";
      exit 0
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "You did not enter a valid value. Make sure you type Y or N \
         exactly. \n";
      new_command_query ()

(** [ask_for_commands x] performs a calcuation for an inputted command. *)
and ask_for_commands () =
  (* The arguments of ask_for_commands can be edited to support
     history/accumulation *)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Please enter an operation (or Exit), followed by a space, \
     followed by the numbers you want to operate on.\n\
     For example, the input 'Add 5 6 1' (without quotes) means 'Add 5 \
     and 6, then Add 1 to that sum.' \n";
  print_string "> ";
  try
    match read_line () |> parse with
    | Add arguments ->
        print_endline (add_tr arguments 0. |> Float.to_string);
        new_command_query ()
    | Multiply arguments ->
        print_endline (multiply_tr arguments 1. |> Float.to_string);
        new_command_query ()
    | Subtract arguments ->
        print_endline (subtract_tr arguments 0. |> Float.to_string);
        new_command_query ()
    | Divide arguments ->
        print_endline (divide_tr arguments 1. |> Float.to_string);
        new_command_query ()
    | Exit ->
        ANSITerminal.print_string [ ANSITerminal.green ] "Goodbye!\n";
        exit 0
  with Malformed ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Did not recognize the command given! Please try again.\n";
    ask_for_commands ()

(** [start_calc x] starts the calculator with initial command [x]. *)
let start_calc x =
  (* Fill in this function later; its intention is to be used as an
     intermediate loading screen when more complex operations are added. *)
  failwith "unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to the Caml Calculator.\n";
  print_endline "Type any key to start, or type Exit to quit.";
  print_string "> ";
  match read_line () with
  | "Exit" ->
      print_endline "Goodbye!";
      exit 0
  | x -> ask_for_commands ()

(** Execute the game engine. *)
let () = main ()
