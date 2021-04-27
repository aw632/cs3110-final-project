module Dual = struct
  type t = float * float

  let make_t e1 e2 = (e1, e2)

  let get_real d = match d with a, _ -> a

  let get_dual d = match d with _, b -> b
end
