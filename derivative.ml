open Dual

module type Derivative = sig
  module F : Field

  type t

  val eval_at_f : F.t -> (t -> t) -> F.t

  val eval_deriv : F.t -> (t -> t) -> F.t
end

module Make =
functor
  (DM : DualMaker)
  (F : Field)
  ->
  struct
    module F = F
    module DType = DM (F)

    type t = DType.t

    let make_var x = DType.make_t x F.one

    let make_constant x = DType.make_t x F.zero

    let eval_at_f v f = make_var v |> f |> DType.get_real

    let eval_deriv v f = make_var v |> f |> DType.get_dual
  end
