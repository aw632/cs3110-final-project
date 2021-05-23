module Dual : sig
  (** [t] is the representation of the dual number a + b(epsilon). See
      dualnumbers explanatory PDF.*)
  type t

  (** [make_t x y] forms the dual number a+b(epsilon) *)
  val make_t : float -> float -> t

  (** [get_real t] returns the non-dual component of a dual number t. *)
  val get_real : t -> float

  (** [get_dual t] returns the dual component of a dual number t. *)
  val get_dual : t -> float

  (** [mult t1 t2] is the product of two dual numbers. Example: (a + be)
      times (c + de) should be ac + (bc +ad)e. A full description can be
      found in the dual numbers explanatory document. *)
  val mult : t -> t -> t

  (** [add t1 t2] is the sum after component-wise addition of two dual
      numbers. Example: (a + be) plus (c + de) should be (a + c) + (b +
      d)e *)
  val add : t -> t -> t

  (** [sub t1 t2] is the subtraction of [t1] and [t2]. *)
  val sub : t -> t -> t

  (** [div t1 t2] is the division of [t1] and [t2]. (a+be)/(c+de) = a/c
      \+ ((bc-ad)/c)^2e Requires: c is nonzero.*)
  val div : t -> t -> t

  (** [exp t a] is the exponentiation of [t] to the power of [a]. *)
  val exp : t -> t -> t

  module InfixOp : sig
    (** [( + )] is the infix version of dual addition. *)
    val ( + ) : t -> t -> t

    (** [( - )] is the infix version of dual addition. *)
    val ( - ) : t -> t -> t

    (** [( * )] is the infix version of dual addition. *)
    val ( * ) : t -> t -> t

    (** [( / )] is the infix version of dual addition. *)
    val ( / ) : t -> t -> t

    (** [( ** )] is the infix version of dual addition. *)
    val ( ** ) : t -> t -> t
  end
end
