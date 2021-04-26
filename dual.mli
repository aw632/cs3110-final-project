module type Field = sig
  (** [t] is the type representing elements of the set Z; it should be
      defined on binary operators [+] and [.] to form the field (Z, +,
      .) *)
  type t

  (** [zero] is the additive identity element in Z. *)
  val zero : t

  (** [one] is the multiplicative identity element in Z. *)
  val one : t

  (** [f_add t1 t2] defines the binary operation called "addition" in
      the field Z. *)
  val f_add : t -> t -> t

  (** [f_add t1 t2] defines the binary operation called "subtraction" in
      the field Z. *)
  val f_sub : t -> t -> t

  (** [f_mult t1 t2] defines the binary operation called "addition" in
      the field Z. *)
  val f_mult : t -> t -> t

  (** [f_div t1 t2] defines the multiplicative inverses on the field Z.*)
  val f_div : t -> t -> t

  (** [f_exp t a] is the exponentiation of [t] to the power of [a]. *)
  val f_exp : t -> t -> t

  (** [to_string t] returns the string representation of t. *)
  val to_string : t -> string
end

module type Dual = sig
  type elem

  (** [t] is the representation of the dual number a + b(epsilon). See
      dualnumbers explanatory PDF.*)
  type t

  (** [make_t x y] forms the dual number a+b(epsilon) *)
  val make_t : elem -> elem -> t

  (** [get_real t] returns the non-dual component of a dual number t. *)
  val get_real : t -> elem

  (** [get_dual t] returns the dual component of a dual number t. *)
  val get_dual : t -> elem

  (** [mult t1 t2] is the product of two dual numbers. Example: (a + be)
      times (c + de) should be ac + (bc +ad)e. A full description can be
      found in the dual numbers explanatory document. *)
  val mult : t -> t -> t

  (** [add t1 t2] is the sum after component-wise addition of two dual
      numbers. Example: (a + be) plus (c + de) should be (a + c) + (b +
      d)e *)
  val add : t -> t -> t

  (** [exp t a] is the exponentiation of [t] to the power of [a]. *)
  val exp : t -> elem -> t

  (** Infix version of [mult t1 t2]. *)
  val ( $* ) : t -> t -> t

  (** Infix version of [add t1 t2]. *)
  val ( $+ ) : t -> t -> t
end

module type DualMaker = functor (F : Field) -> Dual with type elem = F.t
