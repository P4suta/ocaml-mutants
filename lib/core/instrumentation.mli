type error = Interval_forest.error

val instrument : source:Source.t -> Mutant.t list -> (string, error) result
val pp_error : Format.formatter -> error -> unit
