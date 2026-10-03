module type CLOCK = sig
  val now : unit -> string
end

module type STORE = sig
  type t
  type reservation

  val create :
    ?workspace:string -> ?directory:string -> unit -> (t, Error.t) result

  val reserve : t -> started_at:string -> (reservation, Error.t) result
  val abandon_reservation : t -> reservation -> (unit, Error.t) result
end

module type WORKSPACE = sig
  type snapshot

  type 'a bracket_outcome =
    | Acquisition_failed of Error.t
    | Action_returned of 'a * (unit, Error.t) result
    | Action_raised of exn * Printexc.raw_backtrace * (unit, Error.t) result

  val bracket : string -> (snapshot -> 'a) -> 'a bracket_outcome
  (** Acquires the workspace witness, runs the action, and attempts cleanup
      before returning. The abstract [snapshot] value can only be supplied by
      this interpreter. *)
end

module type PROCESS_LIFETIME_CANCELLATION = Signal_service.S
(** A process-lifetime OS interrupt router with per-run subscription tokens.
    Initial router establishment may fail before work starts; once [install]
    returns, [restore] is a total in-memory deactivation. Application keeps
    subscriptions live through immutable report publication, eliminating both
    the old interrupt gap and unreportable OS-handler restoration failures.
    Cancellation observed immediately before [commit_reserved] becomes the
    authoritative interrupted resolution. That check is the decision
    linearization point; a later signal is safely claimed but cannot rewrite a
    report whose atomic commit may already have occurred. *)

type completed = All_detected | Unexpected_survivor | Contract_failure

type verdict =
  | Completed of completed
  | Failed of Error.t
  | Interrupted of Error.t

type report_status =
  | Report_completed
  | Report_interrupted
  | Report_failed of Error.t

type 'draft preparation = {
  draft : 'draft;
  verdict : verdict;
  cleanup_errors : Error.t list;
}

type resolution

val resolve : verdict -> cleanup_errors:Error.t list -> resolution
val with_failure : resolution -> Error.t -> resolution
val report_status : resolution -> report_status
val result : resolution -> (int, Error.t) result
val commit_failure : resolution -> Error.t -> Error.t

module type SERVICES = sig
  module Signals : PROCESS_LIFETIME_CANCELLATION
  module Clock : CLOCK
  module Store : STORE
  module Workspace : WORKSPACE

  type draft

  val prepare_in_snapshot :
    cancel:Cancel.t ->
    store:Store.t ->
    reservation:Store.reservation ->
    started_at:string ->
    root:string ->
    config:Config.t ->
    fresh:bool ->
    selection:Application_request.selection ->
    output:Application_request.output ->
    snapshot:Workspace.snapshot ->
    draft preparation

  val prepare_failure :
    cancel:Cancel.t ->
    store:Store.t ->
    reservation:Store.reservation ->
    started_at:string ->
    root:string ->
    config:Config.t ->
    fresh:bool ->
    selection:Application_request.selection ->
    output:Application_request.output ->
    Error.t ->
    draft preparation
  (** Builds the strongest reportable draft available when workspace acquisition
      or preparation fails before a normal draft is returned. *)

  val commit_reserved :
    store:Store.t ->
    reservation:Store.reservation ->
    finished_at:string ->
    resolution:resolution ->
    draft ->
    (resolution, Error.t) result

  val list_mutants :
    cancel:Cancel.t ->
    root:string ->
    config:Config.t ->
    selection:Application_request.selection ->
    output:Application_request.output ->
    (int, Error.t) result
end

module Make (Services : SERVICES) : sig
  val run_with_cancel :
    cancel:Cancel.t ->
    root:string ->
    config:Config.t ->
    fresh:bool ->
    selection:Application_request.selection ->
    output:Application_request.output ->
    (int, Error.t) result
  (** Runs the same reservation, snapshot, publication, and cleanup lifecycle as
      [run] with a caller-owned cancellation token. The caller is responsible
      for translating its input or process-lifetime events into
      [Cancel.request]. *)

  val run :
    root:string ->
    config:Config.t ->
    fresh:bool ->
    selection:Application_request.selection ->
    output:Application_request.output ->
    (int, Error.t) result

  val list_mutants :
    root:string ->
    config:Config.t ->
    selection:Application_request.selection ->
    output:Application_request.output ->
    (int, Error.t) result
end
