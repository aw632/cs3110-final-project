open Dual

module type Derivative = sig
  module F : Field

  type elt

  type t

  val make_var : elt -> t

  val make_constant : elt -> t

  val eval_at_f : elt -> (t -> t) -> elt

  val eval_deriv : elt -> (t -> t) -> elt
end

module Make =
functor
  (DM : DualMaker)
  (F : Field)
  ->
  struct
    module F = F
    module DType = DM (F)

    type elt = F.t

    type t = DType.t

    let make_var x = DType.make_t x F.one

    let make_constant x = DType.make_t x F.zero

    let eval_at_f v f = make_var v |> f |> DType.get_real

    let eval_deriv v f = make_var v |> f |> DType.get_dual
  end
