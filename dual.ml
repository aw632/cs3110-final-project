module Dual = struct
  type t = float * float

  let make_t e1 e2 = (e1, e2)

  let get_real d = match d with a, _ -> a

  let get_dual d = match d with _, b -> b

  let mult d1 d2 =
    let a = get_real d1 in
    let b = get_dual d1 in
    let c = get_real d2 in
    let d = get_dual d2 in
    make_t (a *. c) ((b *. c) +. (a *. d))

  let add d1 d2 =
    let a = get_real d1 in
    let b = get_dual d1 in
    let c = get_real d2 in
    let d = get_dual d2 in
    make_t (a +. c) (b +. d)

  let sub d1 d2 =
    let a = get_real d1 in
    let b = get_dual d1 in
    let c = get_real d2 in
    let d = get_dual d2 in
    make_t (a -. c) (b -. d)

  let div d1 d2 =
    let a = get_real d1 in
    let b = get_dual d1 in
    let c = get_real d2 in
    let d = get_dual d2 in
    let bc = b *. c in
    let ad = a *. d in
    make_t (a /. c) ((bc -. ad) /. (c ** 2.))

  let exp a b =
    let b = get_real b in
    make_t
      (get_real a ** b)
      (b *. get_dual a *. (get_real a ** (b -. 1.)))

  module InfixOp = struct
    let ( + ) d1 d2 = add d1 d2

    let ( - ) d1 d2 = sub d1 d2

    let ( * ) d1 d2 = mult d1 d2

    let ( / ) d1 d2 = div d1 d2

    let ( ** ) d1 d2 = exp d1 d2
  end
end
