open Ast
open Dual
open Derivative

exception Undefined_Parse

exception Invalid_Calculation

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast =
    try Parser.prog Lexer.read lexbuf with
    | Lexer.SyntaxError msg -> raise Undefined_Parse
    | Parser.Error -> raise Undefined_Parse
  in
  ast

let is_float = function Float _ -> true | _ -> false

let get_bop = function
  | Add -> Dual.add
  | Sub -> Dual.sub
  | Mult -> Dual.mult
  | Divide -> Dual.div
  | Exp -> Dual.exp

let get_bop2 = function
  | Add -> ( +. )
  | Sub -> ( -. )
  | Mult -> ( *. )
  | Divide -> ( /. )
  | Exp -> ( ** )

(** [poly_linear_combo (exp1,exp2) bop] is the linear combination (using
    bop) of the two polynomial functions of exp1 and exp2.

    Requires: exp1 and exp2 are PolyFun. *)
let poly_linear_combo (exp1, exp2) bop =
  match (exp1, exp2) with
  | PolyFun f1, PolyFun f2 ->
      fun variable -> (get_bop2 bop) (f1 variable) (f2 variable)
  | _ -> failwith "precondition violated"

(** [make_polynomial poly_node] create s an OCaml Anonymous function
    representing dual operations.

    Example: Input of [3x^2] would return the function
    [(fun s -> Dual.mult (3 + 0e) (Dual.exp (s + 1e) (2 + 0e)))], where
    [s] is some float.

    Requires: poly_node is a Var, Float, or Poly(_,_,_)*)
let rec make_polynomial poly_node =
  match poly_node with
  | Poly (coeff, _, exp) ->
      let coeff = D.make_constant coeff in
      let exp = D.make_constant exp in
      PolyFun
        (fun s ->
          Dual.mult coeff (Dual.exp (D.make_variable s) exp)
          |> Dual.get_dual)
  | Float constant ->
      PolyFun
        (fun variable -> D.make_constant constant |> Dual.get_dual)
  | Var variable ->
      PolyFun
        (fun variable -> D.make_variable variable |> Dual.get_dual)
  | Binop (bop, exp1, exp2) ->
      if is_float exp1 then
        make_polynomial (Binop (bop, exp1, make_polynomial exp2))
      else if is_float exp2 then
        make_polynomial (Binop (bop, make_polynomial exp1, exp2))
      else PolyFun (poly_linear_combo (exp1, exp2) bop)
  | _ -> raise Undefined_Parse

let get_fun = function
  | PolyFun f -> f
  | _ -> failwith "precondition violated"
