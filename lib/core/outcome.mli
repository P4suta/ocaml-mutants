type t =
  | Killed
  | Survived
  | Timeout
  | Inconclusive of string
  | Error of string

val name : t -> string
val of_string : string -> (t, string) result
val pp : Format.formatter -> t -> unit
