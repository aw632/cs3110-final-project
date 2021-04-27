exception Undefined_Input

exception Integer_Overflow

let rec basic_op_tr op (lst : float list) (acc : float) : float =
  match lst with [] -> acc | hd :: tl -> basic_op_tr op tl (op acc hd)

let add_tr (lst : float list) : float = basic_op_tr ( +. ) lst 0.

let subtract_tr (lst : float list) : float =
  basic_op_tr ( -. ) (List.tl lst) (List.hd lst)

let divide_tr (lst : float list) : float =
  match lst with
  | [] -> 1.
  | h :: snd :: t ->
      if List.mem 0. t || snd = 0. then raise Division_by_zero
      else if h = 0. then 0.
      else basic_op_tr ( /. ) (List.tl lst) (List.hd lst)
  | [ h ] -> h

let multiply_tr (lst : float list) : float = basic_op_tr ( *. ) lst 1.

let rec factorial_tr (num : int) (acc : int) : int =
  if num < 0 then raise Undefined_Input
  else if num >= 20 then raise Integer_Overflow
  else if num = 0 then acc
  else factorial_tr (num - 1) acc * num

let rec summation_tr floor ceiling f =
  let rec sum a b f acc =
    if a <= b then sum (a +. 1.) b f (f a +. acc) else acc
  in
  sum floor ceiling f 0.
