open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let is_poly = function PolyFun _ -> true | _ -> false

let get_bop = function
  | Add -> ( +. )
  | Sub -> ( -. )
  | _ -> failwith "not add or sub"

let poly_linear_combo (exp1, exp2) bop =
  match (exp1, exp2) with
  | PolyFun f1, PolyFun f2 ->
      fun variable -> (get_bop bop) (f1 variable) (f2 variable)
  | _ -> failwith "not valid"

let rec make_polynomial poly_node =
  match poly_node with
  | Poly (coeff, variable, exp) ->
      PolyFun (fun variable -> coeff *. (variable ** exp))
  | Float constant -> PolyFun (fun variable -> constant)
  | Binop (bop, exp1, exp2) ->
      if not (is_poly exp1) then
        make_polynomial (Binop (bop, make_polynomial exp1, exp2))
      else if not (is_poly exp2) then
        make_polynomial (Binop (bop, exp1, make_polynomial exp2))
      else PolyFun (poly_linear_combo (exp1, exp2) bop)
  | _ -> failwith "precondition violated"

let get_fun = function
  | PolyFun f -> f
  | _ -> failwith "precondition violated"

let fun1 = parse "5x^2 +3x +6+7x+34x^3"

let get_fun1 = fun1 |> make_polynomial |> get_fun
