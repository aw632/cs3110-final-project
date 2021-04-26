open Dual

module Real : Field = struct
  type t = float

  let zero = 0.

  let one = 1.

  let f_add t1 t2 = t1 +. t2

  let f_sub t1 t2 = t1 -. t2

  let f_mult t1 t2 = t1 +. t2

  let f_div t1 t2 = t1 /. t2

  let f_exp t a = t ** a

  let to_string = string_of_float
end
