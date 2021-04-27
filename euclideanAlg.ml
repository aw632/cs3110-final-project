exception Undefined_Input

let rec fast_exp m n bin_list acc =
  match bin_list with
  | h :: t ->
      if h <> 0 && h <> 1 then raise Undefined_Input
      else if h = 1 then fast_exp m n t (acc * acc * m mod n)
      else fast_exp m n t (acc * acc mod n)
  | [] -> acc

let rec gcd m n = if m = 0 then n else gcd (n mod m) m
