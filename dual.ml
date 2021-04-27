module type Field = sig
  type t

  val zero : t

  val one : t

  val f_add : t -> t -> t

  val f_sub : t -> t -> t

  val f_mult : t -> t -> t

  val f_div : t -> t -> t

  val f_exp : t -> t -> t

  val to_string : t -> string
end

module type Dual = sig
  type elem

  type t

  val make_t : elem -> elem -> t

  val get_real : t -> elem

  val get_dual : t -> elem

  val mult : t -> t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val div : t -> t -> t

  val exp : t -> t -> t

  val ( $* ) : t -> t -> t

  val ( $+ ) : t -> t -> t
end

module type DualMaker = functor (F : Field) -> Dual with type elem = F.t
