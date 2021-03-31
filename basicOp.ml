let rec basic_op_tr op (lst : float list) (acc : float) : float =
  match lst with [] -> acc | hd :: tl -> basic_op_tr op tl (op acc hd)

let rec add_tr (lst : float list) (acc : float) : float =
  basic_op_tr ( +. ) lst acc

let rec subtract_tr (lst : float list) (acc : float) : float =
  match lst with
  | [] -> 0.
  | h :: snd :: t -> basic_op_tr ( -. ) t (h -. snd)
  | [ h ] -> h

let rec divide_tr (lst : float list) (acc : float) : float =
  match lst with
  | [] -> 1.
  | h :: snd :: t ->
      if List.mem 0. t || snd = 0. then raise Division_by_zero
      else if h = 0. then 0.
      else basic_op_tr ( /. ) t (h /. snd)
  | [ h ] -> h

let rec multiply_tr (lst : float list) (acc : float) : float =
  basic_op_tr ( *. ) lst acc
