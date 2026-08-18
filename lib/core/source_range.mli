type t

val make :
  start_byte:int ->
  end_byte:int ->
  start_line:int ->
  start_column:int ->
  end_line:int ->
  end_column:int ->
  (t, string) result

val start_byte : t -> int
val end_byte : t -> int
val span : t -> Byte_span.t
val start_location : t -> Location.t
val end_location : t -> Location.t
val start_line : t -> int
val start_column : t -> int
val end_line : t -> int
val end_column : t -> int
val byte_length : t -> int
val compare : t -> t -> int
val equal : t -> t -> bool
val contains : outer:t -> inner:t -> bool
val strictly_contains : outer:t -> inner:t -> bool
val overlaps : t -> t -> bool
val pp : Format.formatter -> t -> unit
