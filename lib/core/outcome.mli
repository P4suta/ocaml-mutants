type t =
  | Killed
  | Survived
  | Timeout
  | Inconclusive of string
  | Error of string

val name : t -> string
val is_detected : t -> bool
val is_error : t -> bool
val of_string : string -> (t, string) result
val pp : Format.formatter -> t -> unit
