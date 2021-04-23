open Dual

module Make : DualMaker =
functor
  (F : Field)
  ->
  struct
    open F

    type elem = F.t

    type t = elem * elem

    let make_t e1 e2 = (e1, e2)

    let get_real d = match d with a, _ -> a

    let get_dual d = match d with _, b -> b

    let mult d1 d2 =
      let a = get_real d1 in
      let b = get_dual d1 in
      let c = get_real d2 in
      let d = get_dual d2 in
      make_t (f_mult a c) (f_add (f_mult b c) (f_mult a d))

    let add d1 d2 =
      let a = get_real d1 in
      let b = get_dual d1 in
      let c = get_real d2 in
      let d = get_dual d2 in
      make_t (f_add a c) (f_add b d)

    let ( $* ) d1 d2 = mult d1 d2

    let ( $+ ) d1 d2 = add d1 d2
  end
