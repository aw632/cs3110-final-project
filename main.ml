open BasicOp
open Commands

(** [ask_for_commands x] performs a calcuation for an inputted command.. *)
let rec ask_for_commands () =
  (* The arguments of ask_for_commands can be edited to support
     history/accumulation *)
  print_endline "Please enter a command.\n";
  print_string "> ";
  (* TODO: finish this part*)
  try match Commands.parse (read_line ()) with _ -> assert false
  with Malformed ->
    print_endline
      "Did not recognize the command given! Please try again.\n";
    print_string "> ";
    ask_for_commands ()

(** [start_calc x] starts the calculator with initial command [x]. *)
let start_calc x = failwith "unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to the Caml Calculator.\n";
  print_endline
    "Please enter a command to start, or enter Exit to quit.";
  print_string "> ";
  match read_line () with x -> start_calc x

(* Execute the game engine. *)
let () = main ()
