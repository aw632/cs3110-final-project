module Dual : sig
  (** [t] is the representation of the dual number a + b(epsilon). See
      dualnumbers explanatory PDF. *)
  type t

  (** [make_t x y] forms the dual number a+b(epsilon) *)
  val make_t : float -> float -> t

  (** [get_real t] returns the real component of a dual number t. *)
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

  (** Infix version of [mult t1 t2]. *)
  val ( $* ) : t -> t -> t

  (** Infix version of [add t1 t2]. *)
  val ( $+ ) : t -> t -> t
end
