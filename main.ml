open BasicOp
open Commands
open EuclideanAlg
open StatOp

(** [new_command_query ()] prints the lines below to the console. *)
let rec new_command_query () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\n Do you want to enter a new operation? (Y/N) \n\n ";
  print_string "> ";
  match read_line () with
  | "Y" ->
      print_string "\n ";
      ask_for_commands ()
  | "N" ->
      ANSITerminal.print_string [ ANSITerminal.green ] "\n\n Goodbye!\n";
      exit 0
  | "Exit" ->
      print_endline "\n\n Goodbye!";
      exit 0
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n\n\
        \ You did not enter a valid value. Make sure you type Y or N \
         exactly. \n";
      new_command_query ()

(** [ask_for_commands x] performs a calcuation for an inputted command. *)
and ask_for_commands () =
  (* The arguments of ask_for_commands can be edited to support
     history/accumulation *)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\
    \ \n\
    \ Please enter an operation, followed by a space, followed by \n\
    \ the numbers you want to operate on.\n\
    \   Functions available:\n\
    \   Add (takes in multiple inputs, returns float) \n\
    \   Subtract (takes in multiple inputs, returns float) \n\
    \   Divide (takes in multiple inputs, returns float)\n\
    \   Multiply (takes in multiple inputs, returns float)\n\
    \   Factorial (takes in one input, returns integer)\n\
    \   FastExp (takes in three inputs, returns integer)\n\
    \   Mean (takes in multiple input, returns float)\n\
    \   Median (takes in multiple input, returns float)\n\
    \   StdDev (takes in multiple input, returns float)\n\
    \   LinReg (takes in two lists, returns linear regression)\n\
    \ Enter Exit at any time to exit from the program\n\
    \ ";

  print_string "\n > ";
  try
    match read_line () |> parse with
    | Add arguments ->
        print_endline ("\n" ^ (add_tr arguments |> Float.to_string));
        new_command_query ()
    | Multiply arguments ->
        print_endline ("\n" ^ (multiply_tr arguments |> Float.to_string));
        new_command_query ()
    | Subtract arguments ->
        print_endline ("\n" ^ (subtract_tr arguments |> Float.to_string));
        new_command_query ()
    | Divide arguments ->
        print_endline ("\n" ^ (divide_tr arguments |> Float.to_string));
        new_command_query ()
    | Factorial arguments ->
        print_endline
          ("\n" ^ (factorial_tr arguments 1 |> string_of_int));
        new_command_query ()
    | FastExp (m, n, bin_list) ->
        print_endline ("\n" ^ (fast_exp m n bin_list 1 |> string_of_int));
        new_command_query ()
    | Mean arguments ->
        print_endline ("\n" ^ (mean arguments |> Float.to_string));
        new_command_query ()
    | Median arguments ->
        print_endline ("\n" ^ (median arguments |> Float.to_string));
        new_command_query ()
    | Standard_Dev arguments ->
        print_endline
          ("\n" ^ (standard_deviation arguments |> Float.to_string));
        new_command_query ()
    | Lin_Reg ->
        print_string "First list:";
        print_string "> ";
        let input1 = read_line () in
        let list1 = Commands.parse_list input1 in
        print_string "Second list:";
        print_string "> ";
        let input2 = read_line () in
        let list2 = Commands.parse_list input2 in
        let tuple = linear_regression list1 list2 in
        print_endline
          ("\n In the form y=ax+b, a = "
          ^ string_of_float (fst tuple)
          ^ " and b = "
          ^ string_of_float (snd tuple));
        new_command_query ()
    | Exit ->
        ANSITerminal.print_string [ ANSITerminal.green ] "\nGoodbye!\n";
        exit 0
  with
  | Malformed ->
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\n Did not recognize the command given! Please try again!\n";
      ask_for_commands ()
  | Undefined_Input ->
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\n Input is undefined for this operation! Please try again!\n";
      ask_for_commands ()
  | Integer_Overflow ->
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\n\
         The output for this operation is too large. Please try again!\n";
      ask_for_commands ()
  | Division_by_zero ->
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\n You cannot divide by zero. Please try again!\n";
      ask_for_commands ()
  | Empty ->
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\n This command is empty. Please try again!\n";
      ask_for_commands ()

(** [start_calc x] starts the calculator with initial command [x]. *)
let start_calc x =
  (* Fill in this function later; its intention is to be used as an
     intermediate loading screen when more complex operations are added. *)
  failwith "unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string
    [ ANSITerminal.green; ANSITerminal.Bold ]
    "\n\n Welcome to the Caml Calculator.\n";
  print_endline "\n\n Type any key to start, or type Exit to quit.";
  print_string "\n\n > ";
  match read_line () with
  | "Exit" ->
      print_endline "Goodbye!";
      exit 0
  | x -> ask_for_commands ()

(** Execute the game engine. *)
let () = main ()
