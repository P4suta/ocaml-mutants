type node
type t

type error =
  | Invalid_range of Mutant.t
  | Source_mismatch of Mutant.t
  | Crossing_ranges of Mutant.t * Mutant.t

val create : source:Source.t -> Mutant.t list -> (t, error) result
val roots : t -> node list
val range : node -> Source_range.t
val mutants : node -> Mutant.t list
val children : node -> node list
val pp_error : Format.formatter -> error -> unit
