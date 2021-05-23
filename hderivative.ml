open MatrixDual

module H = struct
  type t = MatrixDual.t

  let make_constant = failwith "Unimplemented"

  let make_variable = failwith "Unimplemented"

  let eval_deriv = failwith "Unimplemented"
end
