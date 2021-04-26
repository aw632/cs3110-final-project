open Dual

module type Derivative = sig
  module F : Field

  (** [t] is the representation of Dual Numbers. *)
  type t

  (** [eval_at_f v f] returns the value of function [f] at the point
      [v]. Requires: [f] knows how to take in Duals of the
      representation specified in DualMaker. *)
  val eval_at_f : F.t -> (t -> t) -> F.t

  (** [eval_at_f v f] returns the value of the derivative of function
      [f] at the point [v]. Requires: [f] knows how to take in Duals of
      the representation specified in DualMaker. *)
  val eval_deriv : F.t -> (t -> t) -> F.t
end

module Make : functor (DM : DualMaker) (F : Field) ->
  Derivative with module F = F
