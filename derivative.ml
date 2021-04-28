open Dual

module D = struct
  type t = Dual.t

  let make_constant x = Dual.make_t x 0.

  let make_variable x = Dual.make_t x 1.

  let eval_at_f f x = make_variable x |> f |> Dual.get_real

  let eval_deriv f x = make_variable x |> f |> Dual.get_dual

  module InfixOp = struct
    include Dual.InfixOp
  end
end
