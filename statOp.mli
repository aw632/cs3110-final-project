(** This module consists of statistical operations *)

(** [median list] returns the median of [list].

    Ex: median [3.0; 2.0; 3.0;1.0] = 2.5.*)
val median : float list -> float

(** [mean list] returns the mean of [list].

    Ex: mean [3.0; 2.0; 3.0;1.0] = 2.25.*)
val mean : float list -> float

(** [standard_deviation list] returns the standard_deviation of [list].

    Ex: mean [1.0; 2.0; 3.0] = 1.0.*)
val standard_deviation : float list -> float

(** [linear_regression list1 list2] return the linear regression of the
    two lists in the form of a tuple (a,b) where a and b are from the
    equation y = ax+b.

    Raises [Undefined_Input] if two lists have less than two entries or
    have different number of entries.*)
val linear_regression : float list -> float list -> float * float
