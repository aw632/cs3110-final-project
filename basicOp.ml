let rec basic_op_tr op (lst : float list) (acc : float) : float =
  match lst with [] -> acc | hd :: tl -> basic_op_tr op tl (op acc hd)

let rec add_tr (lst : float list) (acc : float) : float =
  basic_op_tr ( +. ) lst acc

let rec subtract_tr (lst : float list) (acc : float) : float =
  basic_op_tr ( +. ) lst acc

let rec divide_tr (lst : float list) (acc : float) : float =
  basic_op_tr ( /. ) lst acc

let rec multiply_tr (lst : float list) (acc : float) : float =
  basic_op_tr ( *. ) lst acc
