module D : sig
  (** [t] is the representation of Dual Numbers. *)
  type t = Dual.Dual.t

  (** [make_constant x] returns the dual number (x + 0e). *)
  val make_constant : float -> t

  (** [make_variable x] returns the dual number (x + 1e), which
      represents the first derivative. *)
  val make_variable : float -> t

  (** [eval_at_f f x] returns the value of function [f] at the point
      [v].*)
  val eval_at_f : (t -> t) -> float -> float

  (** [eval_at_f f x] returns the value of the derivative of function
      [f] at the point [v].*)
  val eval_deriv : (t -> t) -> float -> float

  module InfixOp : sig
    val ( + ) : t -> t -> t

    val ( - ) : t -> t -> t

    val ( * ) : t -> t -> t

    val ( / ) : t -> t -> t

    val ( ** ) : t -> t -> t
  end
end
