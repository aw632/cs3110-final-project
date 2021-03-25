open Basic_op
open Commands

(** [ask_for_commands x] starts the calculation with initial command
    [x]. *)
let rec ask_for_commands x =
  print_endline "Please enter a command.\n";
  print_string "> ";
  (* TODO: finish this part*)
  let c = Commands.parse (read_line ()) in
  try match c with _ -> assert false
  with Malformed ->
    print_endline
      "Did not recognize the command given! Please try again.\n";
    print_string "> ";
    ask_for_commands x

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Caml Calculator.\n";
  print_endline "Please enter an operation name.\n";
  print_string "> ";
  match read_line () with x -> ask_for_commands x

(* Execute the game engine. *)
let () = main ()
