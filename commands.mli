(** The type [arguments] represents the arguments passed to each
    operation that are a part of the user's input. Each element in the
    list represents a number. Elements are dilineated in the user input
    by spaces. The list is in the same order as the original user input. *)

type basic_arguments = float list

(** The type [command] represents a user input consisting of a
    mathematical operation and the arguments passed it *)
type command =
  | Add of basic_arguments
  | Multiply of basic_arguments
  | Subtract of basic_arguments
  | Divide of basic_arguments
  | Factorial of int
  | FastExp of (int * int * int list)
  | Mean of basic_arguments
  | Median of basic_arguments
  | Standard_Dev of basic_arguments
  | Lin_Reg
  | Poly
  | Sigma
  | Help
  | Exit

(** [supported_ops] is a list of every operation a user can use. Each
    element corresponds to a specific command*)
val supported_ops : string list

(** raised when the command is empty*)
exception Empty

(** raised when a command is malformed*)
exception Malformed

(** raised when input is not well-defined *)
exception Undefined_Input

val check_linear_regression : float list -> float list -> unit

(** [parse str] takes the user input and makes it a command. The first
    word (the first consecutive sequence of non-space characters)
    indicates the mathematical operation to be performed. The rest of
    the input becomes the arguments passed to the mathematical
    operation. Any integers passed as agruments are automatically
    converted to floats.

    Raises: [Empty] if [str] is the empty string or contains only
    spaces.

    Raises: [Malformed] if the command is malformed. A command is
    malformed if the mathematical operation is not supported, or if the
    input is "exit" and there are arguments following it, or if the
    input is a mathematical operation with no arguments.*)
val parse : string -> command

(** [parse_list str] takes the user input and makes it a float list. Any
    integers passed as agruments are automatically converted to floats.

    Raises: [Malformed] if the command is malformed. A command is
    malformed if the mathematical operation is not supported, or if the
    input is "exit" and there are arguments following it, or if the
    input is a mathematical operation with no arguments.*)
val parse_list : string -> float list
