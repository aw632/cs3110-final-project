exception TriangleDNE

let sin arg = sin (arg *. Float.pi /. 180.)

let tan arg = tan (arg *. Float.pi /. 180.)

let cos arg = cos (arg *. Float.pi /. 180.)

let pythag part length1 length2 =
  match part with
  | "hypotenuse" -> sqrt ((length1 *. length1) +. (length2 *. length2))
  | "leg" ->
      if (length1 *. length1) -. (length2 *. length2) > 0. then
        sqrt ((length1 *. length1) -. (length2 *. length2))
      else raise TriangleDNE
  | _ -> failwith "Will not be reached"
