type t

val of_int : int -> (t, string) result
val to_int : t -> int
val one : t
val pp : Format.formatter -> t -> unit
