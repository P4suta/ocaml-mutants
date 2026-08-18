type output = Application_request.output =
  | Terminal of { quiet : bool; color : bool }
  | Json
  | Stryker_json of Stryker_report.thresholds

type selection = Application_request.selection =
  | All
  | Changed
  | Changed_from of string
  | Mutants of string list

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

val toolchain : cancel:Cancel.t -> root:string -> (string, Error.t) result

module For_testing : sig
  val emit_after_publish :
    write:(string -> unit) -> flush:(unit -> unit) -> string -> Error.t list
  (** Exercises the non-authoritative output boundary used only after the
      immutable native report has been published. Both write and flush are
      attempted; failures are returned as ordered advisories. *)
end
