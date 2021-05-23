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

  let matrix_mult t1 t2 = failwith "TODO"

  let matrix_div t1 t2 = failwith "TODO"

  let matrix_power t1 t2 = failwith "TODO"
end
