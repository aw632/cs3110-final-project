open Dual

module type Derivative = sig
  module F : Field

  type elt

  (** [t] is the representation of Dual Numbers. *)
  type t

  val make_var : elt -> t

  val make_constant : elt -> t

  (** [eval_at_f v f] returns the value of function [f] at the point
      [v]. Requires: [f] knows how to take in Duals of the
      representation specified in DualMaker. *)
  val eval_at_f : elt -> (t -> t) -> elt

  (** [eval_at_f v f] returns the value of the derivative of function
      [f] at the point [v]. Requires: [f] knows how to take in Duals of
      the representation specified in DualMaker. *)
  val eval_deriv : elt -> (t -> t) -> elt
end

module Make : functor (DM : DualMaker) (F : Field) ->
  Derivative with type elt = F.t
