open BasicOp
open Commands
open Notty
open Notty_unix
open EuclideanAlg
open HelpMessage
open StatOp
open Trig
open Help
open Notty
open Notty_unix
module VariableMap = Map.Make (String)

let ans = ref "0"

let uchars_maker arr =
  I.uchars A.(fg (rgb 1 2 5)) (Array.map Uchar.of_int arr)

let time_converter utim =
  let record = utim |> Unix.localtime in
  "【"
  ^ (if record.tm_hour mod 12 < 10 then
     "0" ^ string_of_int (record.tm_hour mod 12)
    else string_of_int (record.tm_hour mod 12))
  ^ ":"
  ^ (if record.tm_min < 10 then "0" ^ string_of_int record.tm_min
    else string_of_int record.tm_min)
  ^ ":"
  ^ (if record.tm_sec < 10 then "0" ^ string_of_int record.tm_sec
    else string_of_int record.tm_sec)
  ^ (if record.tm_hour > 12 then " PM" else " AM")
  ^ " 】"

let make_command_string () =
  let time =
    I.string A.(fg (rgb 2 3 5)) (Unix.time () |> time_converter)
  in
  let small_line =
    I.uchars
      A.(fg (rgb 1 2 4))
      (Array.map Uchar.of_int (Array.make 4 0x2500))
  in
  let middle_line =
    I.uchars
      A.(fg (rgb 1 2 4))
      (Array.map Uchar.of_int (Array.make 4 0x2500))
  in
  I.(small_line <|> time <|> middle_line)

(** [read_float] takes a string input from the user and makes it a
    float.

    Raises Undefined_Input if the string cannot be made a float*)
let read_float () =
  let user_input = read_line () in
  if user_input = "ANS" then !ans |> float_of_string
  else
    try user_input |> String.trim |> float_of_string
    with Failure s -> raise Undefined_Input

(**[prompt_variable_input lst] will prompt the user to input a value for
   every variable in the list. It returns a map where the keys are the
   variables in the list and the values are the floats inputted from the
   terminal bound bound to the specific variable*)
let rec prompt_variable_input lst (var_map : float VariableMap.t) =
  match lst with
  | var :: t ->
      print_endline ("The value of " ^ var ^ ": ");
      print_string " > ";
      let user_input = read_float () in
      let updated_var_map = VariableMap.add var user_input var_map in
      prompt_variable_input t updated_var_map
  | [] -> var_map

(** [check_degree i] checks if [i] is greater than or equal to 1. Raises
    [Undefined_Input] otherwise. *)
let check_degree i = if i >= 1 then i else raise Undefined_Input

let print_linreg_result prompt =
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
    ("\n In the form y=ax+b, a = "
    ^ string_of_float (fst tuple)
    ^ " and b = "
    ^ string_of_float (snd tuple));
  prompt

let print_poly_result prompt =
  print_endline " Function: ";
  print_string " > ";
  let user_input = read_line () in
  let polyFun =
    user_input |> FrontEnd.parse
    |> FrontEnd.make_polynomial ans
    |> FrontEnd.get_fun
  in
  print_endline " Value to evaluate: ";
  print_string " > ";
  let value = read_float () in
  let result = value |> polyFun |> string_of_float in
  ans := result;
  print_endline ("Answer: " ^ result);
  prompt

let print_multivar_result prompt =
  print_endline " Function: ";
  print_string " > ";
  let user_input = read_line () in
  let multivar =
    FrontEnd.make_multivar (user_input |> FrontEnd.parse) []
  in
  let var_lst =
    multivar |> FrontEnd.get_var_lst |> List.sort_uniq compare
  in
  let multi_fun = multivar |> FrontEnd.get_multi_fun in
  let var_map = prompt_variable_input var_lst VariableMap.empty in
  let result = var_map |> multi_fun |> string_of_float in
  ans := result;
  print_endline ("Answer: " ^ result);
  prompt

let print_derivative_result prompt =
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
  let result = value |> polyFunDerivative |> string_of_float in
  ans := result;
  print_endline ("Answer: " ^ result);
  prompt

let print_hderivative_result prompt =
  print_endline " Function to differentiate: ";
  print_string " > ";
  let user_input = read_line () in
  print_endline " Degree of Differentiation: ";
  print_string " > ";
  let degree = read_int () |> check_degree in
  let polyFunDerivative =
    user_input |> FrontEnd.parse
    |> FrontEnd.make_hderivative degree
    |> FrontEnd.get_fun
  in
  print_endline " Value to evaluate: ";
  print_string " > ";
  let value = read_float () in
  let result =
    (value |> polyFunDerivative)
    *. float_of_int (BasicOp.factorial_tr degree 1)
    |> string_of_float
  in
  ans := result;
  print_endline ("Answer: " ^ result);
  prompt

let print_pythag_result prompt =
  print_endline
    " Side you are looking for (type \"hypotenuse\" or \"leg\"): ";
  print_string " > ";
  let user_input = read_line () in
  print_endline "Length of remaining side 1 (hypotenuse or leg)";
  print_string " > ";
  let user_input1 = float_of_string (read_line ()) in
  print_endline "Length of remaining side 2 (leg)";
  print_string " > ";
  let user_input2 = float_of_string (read_line ()) in
  let result =
    pythag user_input user_input1 user_input2 |> string_of_float
  in
  ans := result;
  print_endline ("\n" ^ result);
  prompt

let print_sigma_result prompt =
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
    user_input |> FrontEnd.parse
    |> FrontEnd.make_polynomial ans
    |> FrontEnd.get_fun
  in
  let result = summation_tr a b polyFun |> string_of_float in
  ans := result;
  print_endline ("Answer: " ^ result);
  prompt

(** [ask_for_commands x] performs a calcuation for an inputted command. *)
let rec ask_for_commands () =
  let print_result computation_str =
    let result = computation_str in
    ans := result;
    print_endline ("\n" ^ result);
    ask_for_commands ()
  in
  (* The arguments of ask_for_commands can be edited to support
     history/accumulation *)
  make_command_string () |> output_image;
  print_string "\n > ";
  try
    match parse (read_line ()) ans with
    | Ans ->
        print_endline !ans;
        ask_for_commands ()
    | Add arguments -> print_result (add_tr arguments |> Float.to_string)
    | Multiply arguments ->
        print_result (multiply_tr arguments |> Float.to_string)
    | Subtract arguments ->
        print_result (subtract_tr arguments |> Float.to_string)
    | Divide arguments ->
        print_result (divide_tr arguments |> Float.to_string)
    | Factorial arguments ->
        print_result (factorial_tr arguments 1 |> string_of_int)
    | FastExp (m, n, bin_list) ->
        print_result (fast_exp m n bin_list 1 |> string_of_int)
    | GCD (m, n) -> print_result (gcd m n |> string_of_int)
    | Mean arguments -> print_result (mean arguments |> Float.to_string)
    | Median arguments ->
        print_result (median arguments |> Float.to_string)
    | StandardDev arguments ->
        print_result (standard_deviation arguments |> Float.to_string)
    | LinReg -> print_linreg_result ask_for_commands ()
    | Poly -> print_poly_result ask_for_commands ()
    | MultiVar -> print_multivar_result ask_for_commands ()
    | Sin t -> print_result (sin t |> string_of_float)
    | Cos t -> print_result (cos t |> string_of_float)
    | Tan t -> print_result (tan t |> string_of_float)
    | Pythag -> print_pythag_result ask_for_commands ()
    | Derivative -> print_derivative_result ask_for_commands ()
    | HDerivative -> print_hderivative_result ask_for_commands ()
    | Sigma -> print_sigma_result ask_for_commands ()
    | Menu ->
        menu_msg ();
        ask_for_commands ()
    | Help str ->
        print_endline (help str);
        ask_for_commands ()
    | Exit ->
        ANSITerminal.print_string [ ANSITerminal.green ] "\nGoodbye!\n";
        exit 0
  with
  | Failure _ ->
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\n Did not recognize the command given! Please try again!\n";
      ask_for_commands ()
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
         Please try again! \n";
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
  | TriangleDNE ->
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\n\
        \ These two sides do not form a plausible triangle. Please try \
         again! \n";
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
     are AWang, HuyBear and Kangaloo. Please enjoy. \n"; *)
  let long_line = Array.make 100 0x2600 in
  I.uchars A.(fg (rgb 1 2 4)) (Array.map Uchar.of_int long_line)
  |> Notty_unix.eol |> Notty_unix.output_image;

  let description =
    I.string
      A.(fg (rgb 1 2 4) ++ st bold)
      "Welcome to the ClammyAlpha Calculator!"
  in
  I.(pad ~l:1 ~t:2 ~b:2 description) |> Notty_unix.output_image;
  let company =
    I.string
      A.(fg white)
      "Developed by the esteemed AHA Corporation using OCaml."
  in
  I.(pad ~l:1 ~b:1 company) |> Notty_unix.output_image;

  let devs =
    I.string
      A.(fg white)
      "Chief Developers: AWang, HuyBear, and Kangaroo."
  in
  I.(pad ~l:1 ~b:1 devs) |> Notty_unix.output_image;

  let thanks =
    I.string
      A.(fg white)
      "Special thanks to support from the CS 3110 staff! "
  in
  I.(pad ~l:1 ~b:2 thanks) |> Notty_unix.output_image;

  I.uchars A.(fg (rgb 1 2 4)) (Array.map Uchar.of_int long_line)
  |> Notty_unix.eol |> Notty_unix.output_image;

  let instruction =
    I.string
      A.(fg white)
      "Type any key to start, or type Exit to quit. "
  in
  I.(pad ~l:1 ~t:1 ~b:2 instruction) |> Notty_unix.output_image;

  make_command_string () |> output_image;

  print_string "\n\n > ";

  match read_line () with
  | "Exit" ->
      print_endline "Goodbye!";
      exit 0
  | _ ->
      menu_msg ();
      ask_for_commands ()

(** Execute the game engine. *)
let () = main ()
