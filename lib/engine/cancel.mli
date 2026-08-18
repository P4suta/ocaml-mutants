type t

val create : unit -> t
val request : t -> unit
val is_requested : t -> bool
