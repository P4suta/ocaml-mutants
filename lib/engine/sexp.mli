type t = Atom of string | List of t list

val parse : string -> (t, string) result
val atoms : t -> string list
val pp : Format.formatter -> t -> unit
