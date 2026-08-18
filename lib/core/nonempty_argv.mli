type t

val of_list : string list -> (t, string) result
val to_list : t -> string list
val program : t -> string
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
