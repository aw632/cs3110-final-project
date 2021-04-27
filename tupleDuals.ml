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

    let sub d1 d2 =
      let a = get_real d1 in
      let b = get_dual d1 in
      let c = get_real d2 in
      let d = get_dual d2 in
      make_t (f_sub a c) (f_sub b d)

    let div d1 d2 =
      let a = get_real d1 in
      let b = get_dual d1 in
      let c = get_real d2 in
      let d = get_dual d2 in
      let bc = f_mult b c in
      let ad = f_mult a d in
      make_t (f_div a c)
        (f_div (f_sub bc ad) (f_exp c (f_add F.one F.one)))

    let exp t a =
      let a = get_real a in
      make_t
        (f_exp (get_real t) a)
        (f_mult
           (f_mult a (get_real t))
           (f_exp (get_real t) (f_sub a F.one)))

    let ( $* ) d1 d2 = mult d1 d2

    let ( $+ ) d1 d2 = add d1 d2
  end
