type partial
type complete
type 'state t

type error =
  | Unknown_mutant of string
  | Duplicate_result of string
  | Missing_results of Mutant.t list

val create : Catalog.t -> partial t
val record : partial t -> Mutant.t -> Outcome.t -> (partial t, error) result
val finish : partial t -> (complete t, error) result

val of_complete_list :
  Catalog.t -> (Mutant.t * Outcome.t) list -> (complete t, error) result

val to_list : 'state t -> (Mutant.t * Outcome.t) list
val not_run : partial t -> Mutant.t list
val pp_error : Format.formatter -> error -> unit
