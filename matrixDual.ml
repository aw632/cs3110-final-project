module MatrixDual = struct
  type t = float Array.t Array.t

  (** [replace_elems base_array num dim counter] replaces the diagonal
      with [num] and the number immediately to the right of the diagonal
      with [1]. This is intended to represent dual numbers in [dim]
      dimensions. *)
  let rec replace_elems base_array num dim counter =
    match counter with
    | 0 ->
        base_array.(0).(0) <- num;
        base_array.(0).(1) <- 1.;
        base_array
    | x ->
        if counter + 1 = dim then (
          base_array.(counter).(counter) <- num;
          replace_elems base_array num dim (counter - 1) )
        else (
          base_array.(x).(x) <- num;
          base_array.(x).(x + 1) <- 1.;
          replace_elems base_array num dim (counter - 1) )

  let make_matrix dim (num : float) =
    let base_array = Array.make_matrix dim dim 0. in
    replace_elems base_array num dim (dim - 1)

  let make_scalar dim s = Array.make_matrix dim dim s

  let matrix_add t1 t2 =
    Array.map2 (fun x y -> Array.map2 (fun x1 y1 -> x1 +. y1) x y) t1 t2

  let matrix_sub t1 t2 =
    Array.map2 (fun x y -> Array.map2 (fun x1 y1 -> x1 -. y1) x y) t1 t2

  let matrix_mult t1 t2 =
    let x0 = Array.length t1 and y0 = Array.length t2 in
    let y1 = if y0 = 0 then 0 else Array.length t1.(0) in
    let z = Array.make_matrix x0 y1 0. in
    for i = 0 to x0 - 1 do
      for j = 0 to y1 - 1 do
        for k = 0 to y0 - 1 do
          z.(i).(j) <- z.(i).(j) +. (t1.(i).(k) *. t2.(k).(j))
        done
      done
    done;
    z

  let matrix_div f t =
    let dim = Array.length t in
    matrix_mult (make_scalar dim f) t

  let rec matrix_power orig t n =
    match n with
    | 1 -> t
    | x -> matrix_power orig (matrix_mult orig t) (n - 1)
end
