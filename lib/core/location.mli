type t

val make : line:int -> column:int -> (t, string) result
val line : t -> int
val column : t -> int
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
