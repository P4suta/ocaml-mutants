type t

(** Snapshot-root allocation is one direct exclusive directory creation; it no
    longer passes through a temporary file name. The subsequent source walk,
    destination materialization, and recursive cleanup are still path-based.
    They do not prove safety against a post-creation namespace or reparse-point
    replacement. Production capability wiring remains blocked until a live
    directory capability covers those operations and conditional cleanup. *)

type 'a bracket_outcome =
  | Acquisition_failed of Error.t
  | Action_returned of 'a * (unit, Error.t) result
  | Action_raised of exn * Printexc.raw_backtrace * (unit, Error.t) result

val bracket : string -> (t -> 'a) -> 'a bracket_outcome

val with_snapshot :
  string -> (t -> ('a, Error.t) result) -> ('a, Error.t) result

val source_root : t -> string
val root : t -> string
val manifest_digest : t -> string
val default_skip : string -> bool

module For_testing : sig
  module type TEMP_DIRECTORY = sig
    val create_exclusive :
      temp_dir:string ->
      permissions:int ->
      prefix:string ->
      suffix:string ->
      (string, string) result
  end

  val allocate_temporary_root :
    (module TEMP_DIRECTORY) -> temp_dir:string -> (string, string) result
  (** The injected interface deliberately exposes one direct, exclusive
      directory-creation operation. Snapshot-root allocation has no
      placeholder-file or unlink phase. *)

  val default_skip_for_platform : case_sensitive:bool -> string -> bool
end
