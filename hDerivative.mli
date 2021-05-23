module H : sig
  (** [t] is the representation of Dual Numbers. *)
  type t = MatrixDual.MatrixDual.t

  (** [make_constant x y] returns dual representations of constants or
      coefficients as a [y] by [y] matrix. In Matrix Form, these are
      scalar matrices. *)
  val make_constant : float -> int -> t

  (** [make_variable x y] returns dual representation of variables as a
      [y] by [y] matrix. *)
  val make_variable : float -> int -> t

  (** [eval_at_f f x n] returns the value of the [n]th derivative of
      function [f] at the point [x].*)
  val eval_deriv : (t -> t) -> float -> int -> float
end
