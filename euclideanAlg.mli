(** [fast_exp m n bin_list acc] gives the result of m ^ e mod n.
    [bin_list] is the binary representation going left to right of e

    Requires e,m,n are positve*)
val fast_exp : int -> int -> int list -> int -> int

(** [gcd x y] returns the greatest common denominator of x and y. If x
    and y are relatively prime, gcd x y returns 1*)

(* val gcd : int -> int -> int *)
