let rec basic_op_tr op (lst : float list) (acc : float) : float =
  match lst with [] -> acc | hd :: tl -> basic_op_tr op tl (op acc hd)

<<<<<<< HEAD
let add_tr (lst : float list) : float = basic_op_tr ( +. ) lst 0.

let subtract_tr (lst : float list) : float =
  basic_op_tr ( -. ) (List.tl lst) (List.hd lst)

let divide_tr (lst : float list) : float =
=======
let add_tr (lst : float list) (acc : float) : float =
  basic_op_tr ( +. ) lst acc

let subtract_tr (lst : float list) (acc : float) : float =
  match lst with
  | [] -> 0.
  | h :: snd :: t -> basic_op_tr ( -. ) t (h -. snd)
  | [ h ] -> h

let divide_tr (lst : float list) (acc : float) : float =
>>>>>>> 95bfd643cfa9b6d10824ae2d00562357aba010a6
  match lst with
  | [] -> 1.
  | h :: snd :: t ->
      if List.mem 0. t || snd = 0. then raise Division_by_zero
      else if h = 0. then 0.
      else basic_op_tr ( /. ) (List.tl lst) (List.hd lst)
  | [ h ] -> h

<<<<<<< HEAD
let multiply_tr (lst : float list) : float = basic_op_tr ( *. ) lst 1.
=======
let multiply_tr (lst : float list) (acc : float) : float =
  basic_op_tr ( *. ) lst acc
>>>>>>> 95bfd643cfa9b6d10824ae2d00562357aba010a6
