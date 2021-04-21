let rec fast_exp m n bin_list acc =
  match bin_list with
  | h :: t ->
      if h = 1 then fast_exp m n t (acc * acc * m mod n)
      else fast_exp m n t (acc * acc mod n)
  | [] -> acc
