type t
type error = Span_out_of_bounds of Source_range.t

val of_string : string -> t
val to_string : t -> string
val length : t -> int
val digest : t -> string
val location_at_byte : t -> byte:int -> Location.t option
val slice : t -> Source_range.t -> (string, error) result
val pp_error : Format.formatter -> error -> unit
