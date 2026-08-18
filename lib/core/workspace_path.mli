type t

val of_string : string -> (t, string) result
val to_string : t -> string
val append : t -> string -> (string, string) result
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
