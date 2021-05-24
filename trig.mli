(** This module consists of trig operations for the calculator *)

(** raised when a input creates an impossible triangle*)
exception Triangle_DNE

(** [sin arg] returns the sine function at degree [arg].

    Ex: sin 90 = 1. *)
val sin : float -> float

(** [cos arg] returns the cosine function at degree [arg].

    Ex: cos 90 = 0. *)
val cos : float -> float

(** [tan arg] returns the tangent function at degree [arg].

    Ex: tan 0 = 0. *)
val tan : float -> float

(** [pythag part length1 length2] returns the third side of a right
    angle triangle, depending on if [part] is a leg or hypotenuse.

    Raises: [Triangle_DNE] if inputs create an impossible triangle.

    Ex: pythag "hypotenuse" 3. 4. = 5. *)
val pythag : string -> float -> float -> float
