(** The type [arguments] represents the arguments passed to each
    operation that are a part of the user's input. Each element in the
    list represents a number. Elements are dilineated in the user input
    by spaces. The list is in the same order as the original user input. *)

type basic_arguments = float list

(** The type [command] represents a user input consisting of a
    mathematical operation and the arguments passed it *)
type command =
<<<<<<< HEAD
  | Add of basic_arguments
  | Multiply of basic_arguments
  | Subtract of basic_arguments
  | Divide of basic_arguments
=======
  | Add of arguments
  | Multiply of arguments
  | Subtract of arguments
  | Divide of arguments
  | Factorial of int
>>>>>>> 95bfd643cfa9b6d10824ae2d00562357aba010a6
  | Exit

(** [supported_ops] is a list of every operation a user can use. Each
    element corresponds to a specific command*)
val supported_ops : string list

(** raised when the command is empty*)
exception Empty

(** raised when a command is malformed*)
exception Malformed

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
