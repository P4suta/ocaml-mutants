module Core = Ocaml_mutants_core

type captured = { contents : string; truncated : bool; total_bytes : int }

type stage_result = {
  name : string;
  status : string;
  duration : Core.Duration.t;
}

type baseline_stage = {
  name : string;
  command : Core.Nonempty_argv.t;
  runs : Core.Duration.t list;
  slowest : Core.Duration.t;
}

type warning = { code : string; message : string }
type skip_summary = { reason : string; count : int; examples : string list }
type hit_map_entry = { test : string; mutant_ids : string list }

type retry_attempt = {
  outcome : Core.Outcome.t;
  duration : Core.Duration.t;
  stages : stage_result list;
  stdout : captured;
  stderr : captured;
}

type timeout_retry = {
  initial_timeout : retry_attempt;
  serial_retry : retry_attempt;
}

type evidence_origin =
  | Execution
  | Checkpoint_resume
  | Checkpoint_estimated
  | Historical_exact
  | Historical_estimated
  | Fast_estimated

type expectation_status =
  | Expectation_fulfilled
  | Expectation_unfulfilled_killed
  | Expectation_unfulfilled_confirmed_timeout
  | Expectation_inconclusive of string
  | Expectation_error of string
  | Expectation_stale
  | Expectation_not_evaluated

type expectation_evaluation = {
  mutant_id : string;
  reason : string;
  status : expectation_status;
}

type mutant_result = {
  mutant : Core.Mutant.t;
  outcome : Core.Outcome.t;
  duration : Core.Duration.t;
  cached : bool;
  evidence_origin : evidence_origin;
  stages : stage_result list;
  timeout_confirmed : bool;
  timeout_retry : timeout_retry option;
  expected_reason : string option;
  stdout : captured;
  stderr : captured;
}

type metadata = {
  id : Core.Run_id.t;
  started_at : string;
  finished_at : string;
  workspace_digest : string;
  toolchain : string;
  profile : Core.Operator.Profile.t;
  selection : string;
  test_command : Core.Nonempty_argv.t;
  baseline_duration : Core.Duration.t option;
  baseline_stages : baseline_stage list;
  hit_map : hit_map_entry list;
  timeout : Core.Duration.t option;
  cache_mode : string;
  execution_mode : string;
  historical_reuse : string;
  cache_key : string;
  resolved_config : Yojson.Safe.t;
  input_fingerprint : string;
  config_digest : string;
}

type run_status = Completed | Interrupted | Failed of Error.t

val status_name : run_status -> string

type completeness = Complete | Partial of Core.Mutant.t list

type run = {
  metadata : metadata;
  status : run_status;
  results : mutant_result list;
  checkpointed : int;
  completeness : completeness;
  expectations : expectation_evaluation list;
  skipped : skip_summary list;
  warnings : warning list;
}

type summary = {
  kind : string;
  total : int;
  executed : int;
  not_run : int;
  killed : int;
  survived : int;
  timeout : int;
  unconfirmed_timeouts : int;
  inconclusive : int;
  error : int;
  expected_survivors : int;
  unexpected_survivors : int;
  unfulfilled_expectations : int;
  detected : int;
  score : float option;
}

type evidence_level = Executed | Exact_cache | Estimated

val summary : run -> summary
val result_evidence_level : mutant_result -> evidence_level
val evidence_level_name : evidence_level -> string
val evidence_origin_name : evidence_origin -> string
val run_evidence_level : run -> evidence_level
val result_coverage_from_hit_map : hit_map_entry list -> mutant_result -> string
val result_coverage : run -> mutant_result -> string
val not_run : run -> Core.Mutant.t list

type t
type reservation
type journal
type staged_run
type publish_capability

type finalization = {
  publication : publish_capability;
  cleanup_errors : Error.t list;
}

type published_run = { path : string; run : run; advisories : Error.t list }

type fault_point =
  | Pending_report_write
  | Report_publish
  | Latest_index_update
  | Reservation_marker_remove
  | Lease_unlock
  | Lease_close
  | Lease_root_close
  | Publication_lease_unlock
  | Publication_lease_close
  | Publication_lease_root_close

val create :
  ?workspace:string -> ?directory:string -> unit -> (t, Error.t) result
(** Cache bootstrap, run reservation, and maintenance leases are acquired
    root-relative through the native directory-capability backend; there is no
    pathname creation, ownership-marker, or [lockf] fallback. A missing root is
    accepted only with exact native creation evidence. An existing unmarked root
    is adopted only after a bounded capability-relative enumeration proves it
    empty, then the exact v2 marker is created without replacement. When
    [workspace] is absent (cache maintenance commands), the captured current
    directory is the forbidden separation boundary. A backend unable to prove
    separation, materialization, or lock ownership returns a structured error.
*)

val directory : t -> string
val captured : ?truncated:bool -> ?total_bytes:int -> string -> captured
val expectation_status_of_result : mutant_result -> expectation_status
val expectation_status_name : expectation_status -> string
val expectation_status_is_failure : expectation_status -> bool
val reserve : t -> started_at:string -> (reservation, Error.t) result
val reservation_id : reservation -> Core.Run_id.t
val abandon_reservation : t -> reservation -> (unit, Error.t) result

val open_journal :
  t -> reservation -> key:string -> fresh:bool -> (journal, Error.t) result
(** Opens the always-on crash-recovery journal for an exact input fingerprint. A
    previous session is resumed only while its state is explicitly [open].
    [fresh] starts a new session even when an unfinished one exists. *)

val journal_resumed : journal -> bool

val load_checkpoint :
  journal ->
  source:Core.Source.t ->
  expected:Core.Mutant.t ->
  (mutant_result option, Error.t) result
(** Reads one immutable, checksummed settled result. Invalid, truncated, or
    identity-mismatched records fail closed as a cache miss. *)

val checkpoint_mutant : journal -> mutant_result -> (unit, Error.t) result
(** Durably creates one immutable checksummed checkpoint. *)

val complete_journal : journal -> (unit, Error.t) result
(** Atomically closes the journal session so later ordinary runs cannot treat a
    completed run as crash-recovery input. *)

val stage_run : t -> reservation -> (staged_run, Error.t) result
(** [stage_run] validates process and store ownership, then consumes the active
    reservation as a pure in-memory state transition. It performs no descendant
    filesystem I/O: reservation-marker and publication-path existence are not
    inspected here. Publication conflicts are checked fail-closed by
    [publish_run]. *)

val finalize_run : staged_run -> (finalization, Error.t) result
(** [finalize_run] conditionally deletes the reservation marker through its
    retained parent/file capabilities, never by re-resolving a pathname, while
    transferring the exact live root lease into the returned one-shot
    publication capability without an unlock/reacquire gap. Marker cleanup
    failures and live retry authority are retained in acquisition order so
    callers can encode the errors in the authoritative report before publishing
    it. The transferred lease remains continuously held until [publish_run]
    consumes the capability or [abandon_reservation] releases it. *)

val publish_run : publish_capability -> run -> (published_run, Error.t) result
(** [publish_run] creates [<id>.json.pending] relative to the continuously held
    cache-root capability and atomically renames the returned immutable handle
    to the authoritative [<id>.json]. The native create contract writes and
    verifies the exact encoded bytes before returning that live handle. The
    pending artifact is never loadable. Updating [latest] happens only after
    publication; failures are non-fatal structured [advisories] and never change
    the already-published run or its exit policy. Pending and final paths are
    checked only at this boundary and one native same-directory no-replace
    operation decides the conflict. [latest] replacement is staged beside its
    target under the same retained root. A failed commit retains the pending
    artifact for audit/GC and reports the commit failure as primary with ordered
    cleanup evidence suppressed beneath it. A capability permits exactly one
    publication attempt. The publication capability continuously owns both the
    reservation's native root and lock across the finalize-to-publish boundary;
    maintenance cannot run in between. Lease teardown after a committed report
    is advisory. *)

val run_key : string list -> string

val load_mutant :
  t ->
  key:string ->
  source:Core.Source.t ->
  expected:Core.Mutant.t ->
  mutant_result option

val cacheable_result : mutant_result -> bool
(** [cacheable_result result] holds when the outcome is admissible evidence for
    the shared cache: kills, survivors, and serially confirmed timeouts.
    Inconclusive results, errors, and unconfirmed timeouts are never stored. *)

val checkpointable_result : mutant_result -> bool
(** Whether a settled result can be persisted in the crash-recovery journal.
    Unlike historical cache entries, inconclusive and error results remain
    useful checkpoints; only an unconfirmed timeout is excluded. *)

val save_mutant : t -> key:string -> mutant_result -> (unit, Error.t) result
val load_run : t -> string -> (run, Error.t) result
val list_runs : t -> (run list, Error.t) result

(* Returns immutable reports in newest-first run-ID order. *)
val list_runs_best_effort : t -> run list * (string * Error.t) list
(** Returns valid immutable reports in newest-first order and separately reports
    every rejected run ID. This is for interactive history only: policy and
    report commands must use the strict [list_runs]/[load_run] interfaces. *)

val run_to_yojson : run -> Yojson.Safe.t
val run_to_string : run -> string
val run_of_json : Yojson.Safe.t -> (run, string) result
val range_to_json : Core.Source_range.t -> Yojson.Safe.t
val stats : t -> int * int64

val gc : t -> older_than_days:int -> (int, Error.t) result
(** [gc store ~older_than_days] rejects negative ages. Zero is allowed and is
    still protected by the root maintenance lease. *)

val clean : t -> (unit, Error.t) result

module For_testing : sig
  val create :
    ?workspace:string ->
    ?directory:string ->
    ?next_reservation_sequence:(unit -> (int64, string) result) ->
    fail:(fault_point -> string option) ->
    unit ->
    (t, Error.t) result
  (** Constructs a normal store whose typed publication/finalization boundaries
      can fail deterministically. [next_reservation_sequence] is a narrow seam
      for allocator collision contracts. Production operations remain otherwise
      inaccessible. Lease teardown faults are observed only after the real
      native release attempt and retain their public unlock/close ordering. *)
end
