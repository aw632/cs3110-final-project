open BasicOp
open Commands
open Notty
open Notty_unix
open EuclideanAlg
open StatOp
open Help
open Notty
open Notty_unix

let uchars_maker arr =
  I.uchars A.(fg lightred) (Array.map Uchar.of_int arr)

let time_converter utim =
  let record = utim |> Unix.localtime in
  string_of_int record.tm_hour
  ^ ":"
  ^ string_of_int record.tm_min
  ^ ":"
  ^ string_of_int record.tm_sec

let make_command_string () =
  let time =
    I.string A.(fg lightyellow) (Unix.time () |> time_converter)
  in
  let small_line =
    [| 0x2500; 0x2500; 0x2500; 0x3010 |] |> uchars_maker
  in
  let middle_line =
    [| 0x3011; 0x2500; 0x2500; 0x2500 |] |> uchars_maker
  in
  I.(small_line <|> time <|> middle_line)

(** This menu message will be printed when the user types 'menu' into
    the terminal*)
let menu_msg =
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
  \   Poly (takes in a function and a value to evaluate the function \
   at the value)\n\
  \   Sigma evaluates the sigma from the first number (floor) to the \
   second number (ceiling) using the user-inputted polynomial\n\
  \   Derivative (takes in a function and a value to evaluate the \
   derivative at the value)\n\
  \ Enter Exit at any time to exit from the program\n\
  \ If you want more information, use command help followed by the \
   function you want to know more about\n\
  \ "

(** [read_float] takes a string input from the user and makes it a
    float.

    Raises Undefined_Input if the string cannot be made a float*)
let read_float () =
  try read_line () |> String.trim |> float_of_string
  with Failure s -> raise Undefined_Input

(** [ask_for_commands x] performs a calcuation for an inputted command. *)
let rec ask_for_commands () =
  (* The arguments of ask_for_commands can be edited to support
     history/accumulation *)
  make_command_string () |> output_image;

  print_string "\n > ";
  try
    match read_line () |> parse with
    | Add arguments ->
        print_endline ("\n" ^ (add_tr arguments |> Float.to_string));
        ask_for_commands ()
    | Multiply arguments ->
        print_endline ("\n" ^ (multiply_tr arguments |> Float.to_string));
        ask_for_commands ()
    | Subtract arguments ->
        print_endline ("\n" ^ (subtract_tr arguments |> Float.to_string));
        ask_for_commands ()
    | Divide arguments ->
        print_endline ("\n" ^ (divide_tr arguments |> Float.to_string));
        ask_for_commands ()
    | Factorial arguments ->
        print_endline
          ("\n" ^ (factorial_tr arguments 1 |> string_of_int));
        ask_for_commands ()
    | FastExp (m, n, bin_list) ->
        print_endline ("\n" ^ (fast_exp m n bin_list 1 |> string_of_int));
        ask_for_commands ()
    | GCD (m, n) ->
        print_endline ("\n" ^ (gcd m n |> string_of_int));
        ask_for_commands ()
    | Mean arguments ->
        print_endline ("\n" ^ (mean arguments |> Float.to_string));
        ask_for_commands ()
    | Median arguments ->
        print_endline ("\n" ^ (median arguments |> Float.to_string));
        ask_for_commands ()
    | StandardDev arguments ->
        print_endline
          ("\n" ^ (standard_deviation arguments |> Float.to_string));
        ask_for_commands ()
    | LinReg ->
        print_string " First list:";
        print_string " > ";
        let input1 = read_line () in
        let list1 = Commands.parse_list input1 in
        print_string " Second list:";
        print_string " > ";
        let input2 = read_line () in
        let list2 = Commands.parse_list input2 in
        let tuple = linear_regression list1 list2 in
        print_endline
          ( "\n In the form y=ax+b, a = "
          ^ string_of_float (fst tuple)
          ^ " and b = "
          ^ string_of_float (snd tuple) );
        ask_for_commands ()
    | Poly ->
        print_endline " Function: ";
        print_string " > ";
        let user_input = read_line () in
        let polyFun =
          user_input |> FrontEnd.parse |> FrontEnd.make_polynomial
          |> FrontEnd.get_fun
        in
        print_endline " Value to evaluate: ";
        print_string " > ";
        let value = read_float () in
        print_endline
          ("Answer: " ^ (value |> polyFun |> string_of_float));
        ask_for_commands ()
    | MultiVar -> failwith "not implemented"
    | Derivative ->
        print_endline " Function to differentiate: ";
        print_string " > ";
        let user_input = read_line () in
        let polyFunDerivative =
          user_input |> FrontEnd.parse |> FrontEnd.make_derivative
          |> FrontEnd.get_fun
        in
        print_endline " Value to evaluate: ";
        print_string " > ";
        let value = read_float () in
        print_endline
          ("Answer: " ^ (value |> polyFunDerivative |> string_of_float));
        ask_for_commands ()
    | Sigma ->
        print_endline " First: ";
        print_string " > ";
        let a = read_float () in
        print_endline " Second: ";
        print_string " > ";
        let b = read_float () in
        print_endline " Function: ";
        print_string " > ";
        let user_input = read_line () in
        let polyFun =
          user_input |> FrontEnd.parse |> FrontEnd.make_polynomial
          |> FrontEnd.get_fun
        in
        print_endline
          ("Answer: " ^ (summation_tr a b polyFun |> string_of_float));
        ask_for_commands ()
    | Menu ->
        ANSITerminal.print_string [ ANSITerminal.green ] menu_msg;
        ask_for_commands ()
    | Help str ->
        print_endline (help str);
        ask_for_commands ()
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
  | FrontEnd.Undefined_Parse ->
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\n\
        \ This is not a valid polynomial function or binary operation. \
         Try again. \n";
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

(* let square = "\xe2\x96\xaa"

   let rec sierp n = if n > 1 then let ss = sierp (pred n) in I.(ss <->
   (ss <|> ss)) else I.(string A.(fg magenta) square |> hpad 1 0)

   let rad n color = let a1 = A.fg color in let a2 = A.(st blink ++ a1)
   in I.( string a2 "Rad" |> hpad n 0 <-> (string a1 "(⌐■_■)" |> hpad (n
   + 7) 0)) *)

(* let colors = A.[ red; green; yellow; blue; magenta; cyan ] *)

(** [main ()] prompts for the game to play, then starts it. *)

let main () =
  (* ANSITerminal.print_string [ ANSITerminal.green; ANSITerminal.Bold ]
     "\n\n\ \ Welcome to the ClammyAlpha Calculator. This calculator was
     \ developed by the esteemed AHA corporation. The main developers \
     are AWang, HuyBear and Kangaroo. Please enjoy. \n"; *)
  let long_line = Array.make 100 0x2500 in
  uchars_maker long_line |> Notty_unix.output_image;

  let description =
    I.string
      A.(fg lightyellow ++ bg lightblack ++ st underline ++ st bold)
      "Welcome to the ClammyAlpha Calculator!"
  in
  I.(pad ~l:1 ~t:2 ~b:2 description) |> Notty_unix.output_image;
  let company =
    I.string
      A.(fg white)
      "Developed by the eestemed AHA Corporation with"
  in
  I.(pad ~l:1 ~b:2 company) |> Notty_unix.output_image;
  let devs =
    I.string
      A.(fg white)
      "Chief Developers AWang, HuyBear, and Kangaroo."
  in
  I.(pad ~l:1 ~b:2 devs) |> Notty_unix.output_image;
  I.uchars A.(fg lightred) (Array.map Uchar.of_int long_line)
  |> Notty_unix.eol |> Notty_unix.output_image;
  print_endline "\n\n Type any key to start, or type Exit to quit.\n";
  make_command_string () |> output_image;
  print_string "\n\n > ";
  match read_line () with
  | "Exit" ->
      print_endline "Goodbye!";
      exit 0
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.green ] menu_msg;
      ask_for_commands ()

(** Execute the game engine. *)
let () = main ()
