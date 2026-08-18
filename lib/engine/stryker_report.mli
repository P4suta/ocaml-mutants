module Core = Ocaml_mutants_core

(** A [Run_store.run] projected onto the interoperable Mutation Testing Report
    Schema v2 surface. The projection intentionally remains a view: callers must
    retain the native run report for complete modeled engine state, such as
    inconclusive results and expectations. Process output in that report remains
    an intentionally bounded capture. *)

type thresholds

val thresholds : high:int -> low:int -> (thresholds, string) result
(** Creates the score thresholds required by the v2 schema. Both values are
    percentages and [low] must not exceed [high]. They are explicit inputs so
    the engine does not silently impose dashboard policy. *)

type endpoint = Start | End

type error =
  | Source_unavailable of { path : string; reason : string }
  | Source_digest_mismatch of {
      path : string;
      expected : string;
      actual : string;
    }
  | Source_span_invalid of { path : string; mutant_id : string }
  | Source_location_mismatch of {
      path : string;
      mutant_id : string;
      endpoint : endpoint;
      recorded_line : int;
      recorded_column : int;
      derived_line : int;
      derived_column : int;
    }
  | Source_original_mismatch of {
      path : string;
      mutant_id : string;
      expected : string;
      actual : string;
    }
  | Duplicate_mutant_id of {
      mutant_id : string;
      first_path : string;
      duplicate_path : string;
    }

val pp_error : Format.formatter -> error -> unit

val to_yojson :
  thresholds:thresholds ->
  read_source:(path:string -> (string, string) result) ->
  Run_store.run ->
  (Yojson.Safe.t, error) result
(** [to_yojson ~read_source run] produces a Mutation Testing Report Schema v2
    document. [read_source] receives normalized workspace-relative paths. Every
    source is read exactly once and checked against the digest and source slices
    recorded by the native report before it is emitted. Recorded line and
    byte-column coordinates must also equal the positions derived from the byte
    offsets in that source. *)

val to_string :
  thresholds:thresholds ->
  read_source:(path:string -> (string, string) result) ->
  Run_store.run ->
  (string, error) result

module For_testing : sig
  val reject_duplicate_identities :
    (string * string) list -> (unit, error) result
  (** The global full-ID uniqueness check used by the projection, exposed only
      to exercise cross-file collision behavior without forging invalid
      [Core.Mutant.t] values. *)
end
