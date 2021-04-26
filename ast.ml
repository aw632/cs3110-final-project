(** The type of binary operators. *)
type bop =
  | Add
  | Mult
  | Sub
  | Divide
  | Exp

(** The type of the abstract syntax tree (AST). *)
type expr =
  | Float of float
  | Int of int
  | Binop of bop * expr * expr
  | Poly of float * string * float
  | Var of string
  | PolyFun of (float -> float)
