open Ast
open Dual
open Derivative
module VariableMap = Map.Make (String)

type vars = float VariableMap.t

let empty_variable_map = VariableMap.empty

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

let is_function = function
  | PolyFun _ -> true
  | MultiFun _ -> true
  | _ -> false

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

    Requires: poly_node is a Var, Float, Binop or Poly(_,_,_)*)
let rec make_polynomial poly_node =
  match poly_node with
  | Poly (coeff, variable, exp) ->
      PolyFun (fun variable -> coeff *. (variable ** exp))
  | Float constant -> PolyFun (fun variable -> constant)
  | Var variable -> PolyFun (fun variable -> variable)
  | Binop (bop, exp1, exp2) ->
      if not (is_function exp1) then
        make_polynomial (Binop (bop, make_polynomial exp1, exp2))
      else if not (is_function exp2) then
        make_polynomial (Binop (bop, exp1, make_polynomial exp2))
      else PolyFun (poly_linear_combo (exp1, exp2) bop)
  | _ -> raise Undefined_Parse

(** [make_derivative poly_node] create s an OCaml Anonymous function
    representing dual operations.

    Example: Input of [3x^2] would return the function
    [(fun s -> Dual.mult (3 + 0e) (Dual.exp (s + 1e) (2 + 0e)))], where
    [s] is some float.

    Requires: poly_node is a Var, Float, Binop or Poly(_,_,_)*)
let rec make_derivative poly_node =
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
      (* if [is_function exp1] is false, i.e. exp1 is not a function,
         then recurse on it*)
      if not (is_function exp1) then
        make_derivative (Binop (bop, make_derivative exp1, exp2))
      else if not (is_function exp2) then
        make_derivative (Binop (bop, exp1, make_derivative exp2))
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

(** [multi_linear_combo (exp1,exp2) bop] is the linear combination
    (using bop) of the two multivariable functions of exp1 and exp2

    Example: exp1 = PolyFun (fun input_map-> 3. *. (VariableMap.find "x"
    input_map)) and exp2 = PolyFun (fun input_map-> (VariableMap.find
    "y" input_map)**2.) and bop = Add then the combined polynomial is
    PolyFun (fun input_map -> (+.) ((fun input_map-> 3. *.
    (VariableMap.find "x" input_map)) x) ((fun input_map ->
    (VariableMap.find "x" input_map) ** 2.) input_map) Abstractly, the
    linaer combination of 3x and y^2 is 3x + y^2

    Requires: exp1 and exp2 are MultiFun variants i.e represent some
    multivariable function*)
let multivar_linear_combo (exp1, exp2) bop =
  match (exp1, exp2) with
  | MultiFun (f1, _), MultiFun (f2, _) ->
      fun input_map -> (get_bop bop) (f1 input_map) (f2 input_map)
  | _ -> failwith "precondition violated"

let get_multi_fun = function
  | MultiFun (f, _) -> f
  | _ -> failwith "not multifun"

let get_var_lst = function
  | MultiFun (_, l) -> l
  | _ -> failwith "not multifun"

(** [make_multivar multi_node] creates an ocaml function to represent
    the polynomial expressed by node. The input to the function is a
    map. The value of the variable is retrieved from the map and used in
    a calculation to return as the value of the function.

    Example: Poly (3.,x,5.) returns MultiFun (fun input-> 3. *.
    (VariableMap.find x input) ** 5.) which is equivalent to 3x^5 where
    the value of x is input into the map input.

    Requires: node is a Var, Float, Binop or Poly(_,_,_) The map input
    into each function always has a binding for the variable the
    function represents*)
let rec make_multivar multi_node (var_lst : string list) =
  match multi_node with
  | Poly (coeff, variable, exp) ->
      let new_var_lst = variable :: var_lst in
      MultiFun
        ( (fun (variable_values : vars) ->
            coeff *. (VariableMap.find variable variable_values ** exp)),
          new_var_lst )
  | Float constant ->
      MultiFun ((fun variable_values -> constant), var_lst)
  | Var variable ->
      let new_var_lst = variable :: var_lst in
      MultiFun
        ( (fun variable_values ->
            VariableMap.find variable variable_values),
          new_var_lst )
  | Binop (bop, exp1, exp2) ->
      if not (is_function exp1) then
        let multifun1 = make_multivar exp1 var_lst in
        make_multivar
          (Binop (bop, multifun1, exp2))
          (multifun1 |> get_var_lst)
      else if not (is_function exp2) then
        let multifun2 = make_multivar exp2 var_lst in
        make_multivar
          (Binop (bop, exp1, multifun2))
          (multifun2 |> get_var_lst)
      else MultiFun (multivar_linear_combo (exp1, exp2) bop, var_lst)
  | _ -> raise Undefined_Parse
