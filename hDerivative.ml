open MatrixDual

module H = struct
  type t = MatrixDual.t

  let make_constant x y = MatrixDual.make_scalar y x

  let make_variable x y = MatrixDual.make_matrix y x

  let eval_deriv f x n =
    let small_res =
      make_variable x (n - 1) |> f |> MatrixDual.get_last_dual
    in
    small_res *. float_of_int (BasicOp.factorial_tr n 1)
end
