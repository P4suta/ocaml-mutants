type t = {
  contents : string;
  encoding_errors : int;
  retained_raw_sha256 : string;
}

val normalize : string -> t
(** [normalize raw] preserves every well-formed UTF-8 byte and replaces each
    byte that cannot participate in a well-formed scalar encoding with [?].
    The replacement is deliberately one byte wide, so byte-oriented capture
    limits and offsets remain truthful. [retained_raw_sha256] identifies the
    exact bytes supplied by the process before normalization. *)

val valid_sha256 : string -> bool
(** [valid_sha256 value] accepts canonical lowercase SHA-256 hex only. *)
