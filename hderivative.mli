module H : sig
  (** [t] is the representation of Dual Numbers. *)
  type t = MatrixDual.MatrixDual.t

  (** [make_constant x] returns dual representations of constants or
      coefficients. In Matrix Form, these are scalar matrices. *)
  val make_constant : float -> t

  (** [make_variable x] returns dual representation of variables. *)
  val make_variable : float -> t

  (** [eval_at_f f x] returns the value of the derivative of function
      [f] at the point [v].*)
  val eval_deriv : (t -> t) -> float -> float
end
