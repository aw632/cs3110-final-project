(** This module consists of basic operations for the calculator *)

(** [add_tr acc numlist] sums all elements in [numlist]. Ex: add 0.
    [1.0;4.0;5.0] = 1.0 +. 4.0 +. 5.0 = 10.0. *)
val add_tr : float list -> float -> float

(** [subtract_tr acc numlist] substracts each element from its previous
    elements in [numlist]. Ex: substract 0. [3.4; 6.0; 2.2] = 3.4 -. 6.0
    \-. 2.2 = -4.8.*)
val subtract_tr : float list -> float -> float

(** [divide_tr acc numlist] divides each element from its previous
    elements in [numlist]. Function is left associative.

    Raises [Division_by_zero] if any element except for the first is 0.

    Ex: divide_tr 1.0 [3.0; 2.0; 3.0] = (3.0 /. 2.0) /. 3.0 = 0.5.*)
val divide_tr : float list -> float -> float

(** [multiply_tr acc numlist] multiplies all elements in [numlist]. Ex:
    multiply_tr 1.0 [3.0; 2.0; 4.0; 2.0] = 3.0 *. 2.0 *. 4.0 *. 2.0 =
    48.0. *)
val multiply_tr : float list -> float -> float
