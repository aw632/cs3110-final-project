module Dual = struct
  type t = float * float

  let make_t x y = (x, y)

  let get_real t = match t with a, _ -> a

  let get_dual t = match t with _, b -> b

  let mult t1 t2 =
    let a = get_real t1 in
    let b = get_dual t1 in
    let c = get_real t2 in
    let d = get_dual t2 in
    make_t (a *. c) ((b *. c) +. (a *. d))

  let add t1 t2 =
    make_t (get_real t1 +. get_dual t2) (get_dual t1 +. get_dual t2)

  let ( $* ) t1 t2 = mult t1 t2

  let ( $+ ) t1 t2 = add t1 t2
end
