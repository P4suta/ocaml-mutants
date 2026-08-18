type t

type error =
  | Hash_collision of { id : string; left : Mutant.t; right : Mutant.t }

val empty : t
val of_list : Mutant.t list -> (t, error) result
val to_list : t -> Mutant.t list
val length : t -> int
val exact_duplicates : t -> int
val filter : (Mutant.t -> bool) -> t -> t
val find : string -> t -> Mutant.t option
val pp_error : Format.formatter -> error -> unit

module For_testing : sig
  val of_list_with_short_id :
    (Mutant.t -> string) -> Mutant.t list -> (t, error) result
end
