module MatrixDual : sig
  (** [t] is dual numbers represented as a matrix. For example, the dual
      number a + be is represented as \[\[a, b\]\[0, a\]\]. *)
  type t = float Array.t Array.t

  (** [make_matrix i input] forms an upper-triangular Toeplitz matrix of
      size [i+1] given the input number [input]. *)
  val make_matrix : int -> float -> t

  (** [make_scalar dim s t] makes a scalar [s] of dimension [dim]. *)
  val make_scalar : int -> float -> t

  (** [matrix_add t1 t2] is the summation of two matrices. *)
  val matrix_add : t -> t -> t

  (** [matrix_sub t1 t2] is the subtraction of two matrices. *)
  val matrix_sub : t -> t -> t

  (** [matrix_div t1 t2] is the division of two matrices.*)
  val matrix_div : t -> t -> t

  (** [matrix_mult t1 t2] is the multiplication of two matrices. *)
  val matrix_mult : t -> t -> t

  (** [matrix_mult t n] is the exponentiation of matrix [t] to the power
      [n]. *)
  val matrix_power : t -> int -> t
end
