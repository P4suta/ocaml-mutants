type t

val zero : t
val of_seconds : float -> (t, string) result
val to_seconds : t -> float
val add : t -> t -> t
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
