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
end
