type t

val create : started_at:string -> nonce:string -> (t, string) result
val of_string : string -> (t, string) result
val to_string : t -> string
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
