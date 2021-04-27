(** [fast_exp m n bin_list acc] gives the result of m ^ e mod n.
    [bin_list] is the binary representation going left to right of e.

    Requires e,m and n are positve. *)
val fast_exp : int -> int -> int list -> int -> int

(** [gcd m n] returns the greatest common denominator of m and n. If m
    and n are relatively prime, gcd m n returns 1.

    Requires m and n are positive.

    Ex: gcd 10 5 = 5 *)

val gcd : int -> int -> int
