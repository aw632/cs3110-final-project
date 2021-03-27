let rec add_tr (lst : float list) (acc : float) : float =
  match lst with [] -> acc | hd :: tl -> add_tr tl (acc +. hd)

let rec subtract_tr (lst : float list) (acc : float) : float =
  match lst with [] -> acc | hd :: tl -> add_tr tl (acc -. hd)

let rec divide_tr (lst : float list) (acc : float) : float =
  match lst with [] -> acc | hd :: tl -> add_tr tl (acc /. hd)

let rec multiply_tr (lst : float list) (acc : float) : float =
  match lst with [] -> acc | hd :: tl -> add_tr tl (acc *. hd)
