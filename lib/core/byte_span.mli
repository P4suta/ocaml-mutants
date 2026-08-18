type t

val make : start_byte:int -> end_byte:int -> (t, string) result
val start_byte : t -> int
val end_byte : t -> int
val length : t -> int
val compare : t -> t -> int
val equal : t -> t -> bool
val contains : outer:t -> inner:t -> bool
val strictly_contains : outer:t -> inner:t -> bool
val overlaps : t -> t -> bool
val pp : Format.formatter -> t -> unit
