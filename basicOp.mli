(** This module consists of basic operations for the calculator *)

(** raised when a command input is not in domain of function*)
exception Undefined_Input

(** raised when a command ouput is too large*)
exception Integer_Overflow

(** [add_tr numlist] sums all elements in [numlist].

    Ex: add [1.0;4.0;5.0] = 1.0 +. 4.0 +. 5.0 = 10.0. *)
val add_tr : float list -> float

(** [subtract_tr numlist] substracts each element from its previous
    elements in [numlist].

    Ex: substract [3.4; 6.0; 2.2] = 3.4 -. 6.0 -. 2.2 = -4.8.*)
val subtract_tr : float list -> float

(** [divide_tr numlist] divides each element from its previous elements
    in [numlist]. Function is left associative.

    Raises [Division_by_zero] if any element except for the first is 0.

    Ex: divide_tr [3.0; 2.0; 3.0] = (3.0 /. 2.0) /. 3.0 = 0.5.*)
val divide_tr : float list -> float

(** [multiply_tr numlist] multiplies all elements in [numlist].

    Ex: multiply_tr [3.0; 2.0; 4.0; 2.0] = 3.0 *. 2.0 *. 4.0 *. 2.0 =
    48.0. *)
val multiply_tr : float list -> float

(** [factorial_tr num acc] returns the factorial of [num].

    Raises: [Undefined_Input] if [num] is negative. [Integer_Overflow]
    if output is out of range

    Ex: factorial_tr 3 1 = 6. *)
val factorial_tr : int -> int -> int
