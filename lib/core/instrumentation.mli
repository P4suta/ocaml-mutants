type error = Interval_forest.error

val hit_owner_environment : string

val instrument :
  hit_owner:string -> source:Source.t -> Mutant.t list -> (string, error) result

val pp_error : Format.formatter -> error -> unit
