open BasicOp

let linear_regression list1 list2 =
  let lst1_length = List.length list1 in
  let lst2_length = List.length list2 in
  if
    lst1_length < 2 || lst2_length < 2 || not (lst1_length = lst2_length)
  then raise Undefined_Input
  else
    let sum_x = BasicOp.add_tr list1 in
    let sum_y = BasicOp.add_tr list2 in
    let sum_x_sq = BasicOp.add_tr (List.map (fun x -> x *. x) list1) in
    let sum_xy =
      BasicOp.add_tr (List.map2 (fun x y -> x *. y) list1 list2)
    in
    let n = float_of_int (List.length list1) in
    let b =
      ((sum_y *. sum_x_sq) -. (sum_x *. sum_xy))
      /. ((n *. sum_x_sq) -. (sum_x *. sum_x))
    in
    let a =
      ((n *. sum_xy) -. (sum_x *. sum_y))
      /. ((n *. sum_x_sq) -. (sum_x *. sum_x))
    in
    (a, b)
