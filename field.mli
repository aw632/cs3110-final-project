module Field : sig
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

  (** [f_mult t1 t2] defines the binary operation called "addition" in
      the field Z. *)
  val f_mult : t -> t -> t

  (** [f_div t1 t2] defines the multiplicative inverses on the field Z.*)
  val f_div : t -> t -> t

  (** [to_string t] returns the string representation of t. *)
  val to_string : t -> string
end
