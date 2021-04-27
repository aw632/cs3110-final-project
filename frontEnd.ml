open Ast

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

let is_poly = function PolyFun _ -> true | _ -> false

(** TODO: factor out duplicated code *)
let get_bop = function
  | Add -> ( +. )
  | Sub -> ( -. )
  | Mult -> ( *. )
  | Divide -> ( /. )
  | Exp -> ( ** )

(** [poly_linear_combo (exp1,exp2) bop] is the linear combination (using
    bop) of the two polynomial functions of exp1 and exp2

    Example: exp1 = PolyFun (fun x-> 3. *. x) and exp2 = PolyFun (fun
    x-> x**2.) and bop = Add then the combined polynomial is PolyFun
    (fun x -> (+.) ((fun x-> 3. *. x) x) ((fun x -> x ** 2.) x)
    Abstractly, the linaer combination of 3x and x^2 is 3x + x^2

    Requires: exp1 and exp2 are PolyFun variants i.e represent some
    polynomial function*)
let poly_linear_combo (exp1, exp2) bop =
  match (exp1, exp2) with
  | PolyFun f1, PolyFun f2 ->
      fun variable -> (get_bop bop) (f1 variable) (f2 variable)
  | _ -> failwith "precondition violated"

(** [make_polynomial poly_node] creates an ocaml function to represent
    the polynomial expressed by poly_node

    Example: Poly (3.,x,5.) returns PolyFun (fun x-> 3. *. x ** 5.)
    which is equivalent to 3x^5

    Requires: poly_node is a Var, Float, or Poly(_,_,_)*)
let rec make_polynomial poly_node =
  match poly_node with
  | Poly (coeff, variable, exp) ->
      PolyFun (fun variable -> coeff *. (variable ** exp))
  | Float constant -> PolyFun (fun variable -> constant)
  | Var variable -> PolyFun (fun variable -> variable)
  | Binop (bop, exp1, exp2) ->
      if not (is_poly exp1) then
        make_polynomial (Binop (bop, make_polynomial exp1, exp2))
      else if not (is_poly exp2) then
        make_polynomial (Binop (bop, exp1, make_polynomial exp2))
      else PolyFun (poly_linear_combo (exp1, exp2) bop)
  | _ -> raise Undefined_Parse

let get_fun = function
  | PolyFun f -> f
  | _ -> failwith "precondition violated"

(** [is_reduced] tells the calculator when to stop 'stepping.' An
    expression is fully reduced when it is a float*)
let is_reduced = function Float _ -> true | _ -> false

let get_float = function
  | Float num -> num
  | _ -> failwith "precondition violated"

(** [perform_binop] performs the binary operation of the two expressions
    Requires exp1 and exp2 are Float variants*)
let perform_binop (exp1, exp2) bop =
  match (exp1, exp2) with
  | Float float1, Float float2 ->
      let result = (get_bop bop) float1 float2 in
      if result = infinity then raise Invalid_Calculation else result
  | _ -> failwith "precondition violated"

let rec reduce_bin_op binop =
  match binop with
  | Binop (bop, exp1, exp2) ->
      if not (is_reduced exp1) then
        reduce_bin_op (Binop (bop, reduce_bin_op exp1, exp2))
      else if not (is_reduced exp2) then
        reduce_bin_op (Binop (bop, exp1, reduce_bin_op exp2))
      else Float (perform_binop (exp1, exp2) bop)
  | _ -> raise Invalid_Calculation
