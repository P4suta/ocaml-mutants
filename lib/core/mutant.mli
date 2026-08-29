type unchecked
type validated
type 'phase mutant
type t = validated mutant

module Id : sig
  type t

  val full : t -> string
  val short : t -> string
  val is_valid_prefix : string -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
end

type validation_error =
  | Invalid_path of string
  | Source_error of Source.error
  | Original_mismatch of { expected : string; actual : string }
  | Source_digest_mismatch of { expected : string; actual : string }
  | Id_mismatch of { expected : string; actual : string }
  | Empty_replacement

val unchecked :
  path:string ->
  range:Source_range.t ->
  rule:Operator.Rule.t ->
  replacement:string ->
  (unchecked mutant, validation_error) result

val decoded :
  path:string ->
  range:Source_range.t ->
  rule:Operator.Rule.t ->
  original:string ->
  replacement:string ->
  source_digest:string ->
  full_id:string ->
  (unchecked mutant, validation_error) result

val validate :
  source:Source.t -> unchecked mutant -> (t, validation_error) result

val restore :
  path:string ->
  range:Source_range.t ->
  rule:Operator.Rule.t ->
  original:string ->
  replacement:string ->
  source_digest:string ->
  full_id:string ->
  (t, validation_error) result

val id : 'phase mutant -> Id.t
val path : 'phase mutant -> string
val range : 'phase mutant -> Source_range.t
val rule : 'phase mutant -> Operator.Rule.t
val family : 'phase mutant -> Operator.Family.t
val original : validated mutant -> string
val replacement : 'phase mutant -> string
val source_digest : validated mutant -> string

(* A structural, non-authoritative identity for history display and manual
   rebind suggestions. It deliberately excludes byte offsets and the complete
   source digest and must never be used for cache or policy evidence. *)
val lineage_id : validated mutant -> string
val equal_identity : t -> t -> bool
val normalize_path : string -> string
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
val pp_validation_error : Format.formatter -> validation_error -> unit
