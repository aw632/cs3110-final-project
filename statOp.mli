(** This module consists of statistical operations *)

(** [linear_regression list1 list2] return the linear regression of the
    two lists in the form of a tuple (a,b) where a and b are from the
    equation y = ax+b.

    Raises [Undefined_Input] if two lists have less than two entries or
    have different number of entries.*)
val linear_regression : float list -> float list -> float * float
