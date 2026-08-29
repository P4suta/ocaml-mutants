type output = Application_request.output =
  | Terminal of { quiet : bool; color : bool }
  | Json
  | Stryker_json of Stryker_report.thresholds

type selection = Application_request.selection =
  | All
  | Changed
  | Changed_from of string
  | Mutants of string list
  | Shard of Application_request.shard_selection
  | Rerun of { parent_run_id : string; mutant_id : string }

module Workspace : sig
  type snapshot

  type 'a bracket_outcome =
    | Acquisition_failed of Error.t
    | Action_returned of 'a * (unit, Error.t) result
    | Action_raised of exn * Printexc.raw_backtrace * (unit, Error.t) result

  val bracket : string -> (snapshot -> 'a) -> 'a bracket_outcome
end

type draft

val prepare_in_snapshot :
  cancel:Cancel.t ->
  store:Run_store.t ->
  reservation:Run_store.reservation ->
  started_at:string ->
  root:string ->
  config:Config.t ->
  fresh:bool ->
  selection:selection ->
  output:output ->
  snapshot:Workspace.snapshot ->
  draft Application.preparation

val prepare_failure :
  cancel:Cancel.t ->
  store:Run_store.t ->
  reservation:Run_store.reservation ->
  started_at:string ->
  root:string ->
  config:Config.t ->
  fresh:bool ->
  selection:selection ->
  output:output ->
  Error.t ->
  draft Application.preparation

val commit_reserved :
  store:Run_store.t ->
  reservation:Run_store.reservation ->
  finished_at:string ->
  resolution:Application.resolution ->
  draft ->
  (Application.resolution, Error.t) result

val list_mutants :
  cancel:Cancel.t ->
  root:string ->
  config:Config.t ->
  selection:selection ->
  output:output ->
  (int, Error.t) result

val create_shard_plan :
  cancel:Cancel.t ->
  root:string ->
  config:Config.t ->
  selection:selection ->
  shard_count:int ->
  durations:(string * float) list ->
  (Shard_plan.t, Error.t) result

type deep_diagnostic = {
  mutants : int;
  tests : int;
  baseline_seconds : float;
  timeout_seconds : float;
}

val doctor_deep :
  cancel:Cancel.t ->
  root:string ->
  config:Config.t ->
  (deep_diagnostic, Error.t) result
(** Runs snapshot acquisition, analysis, test inventory, baseline, mutation
    instrumentation, and readiness proof, but executes no mutant. *)

val toolchain : cancel:Cancel.t -> root:string -> (string, Error.t) result

module For_testing : sig
  val redact : string list -> string -> string
  (** Replaces every configured literal with an equal-length mask. *)

  val selected_stages :
    Config.t ->
    Run_store.hit_map_entry list ->
    Ocaml_mutants_core.Mutant.t ->
    string list * bool
  (** Returns the ordered stage names and whether fast-mode evidence omits any
      configured stage. *)

  val resolved_test_plan :
    Config.t ->
    Dune_adapter.described_test list ->
    ((Config.t * Config.t), Error.t) result
  (** Effective execution config and its baseline-only config. Dune drivers use
      individual [runtest-NAME] aliases plus one exhaustive [@runtest]
      remainder; the baseline executes that exhaustive alias once. *)

  val classify_exhaustive_hits :
    Config.t -> Run_store.hit_map_entry list -> Run_store.hit_map_entry list
  (** Removes hits already attributed to an individual Dune test from the
      exhaustive fallback entry, leaving only unclassified test-rule hits. *)

  val settlement_ready : Run_store.mutant_result -> bool
  (** False only for an initial timeout that still requires its serial retry. *)

  val mutant_environment :
    root:string ->
    Ocaml_mutants_core.Mutant.t ->
    (string * string option) list
  (** Explicit attempt environment. In particular, inherited readiness hit
      files are removed before executing a mutant. *)

  val emit_after_publish :
    write:(string -> unit) -> flush:(unit -> unit) -> string -> Error.t list
  (** Exercises the non-authoritative output boundary used only after the
      immutable native report has been published. Both write and flush are
      attempted; failures are returned as ordered advisories. *)
end
