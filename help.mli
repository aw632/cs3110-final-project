(** [help str] will take in a function name [str] and return a
    corresponding in-depth explanation of that function.

    Raises [Malformed] if [str] is not found to be one of the available
    function. *)
exception Malformed

val help : string -> string
