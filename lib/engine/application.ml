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
end

module type PROCESS_LIFETIME_CANCELLATION = Signal_service.S

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

type resolution = { verdict : verdict; cleanup_errors : Error.t list }

let suppress_all primary errors = List.fold_left Error.suppress primary errors

let resolved_error resolution =
  match resolution.verdict with
  | Failed error | Interrupted error ->
      Some (suppress_all error resolution.cleanup_errors)
  | Completed _ -> (
      match resolution.cleanup_errors with
      | [] -> None
      | primary :: rest -> Some (suppress_all primary rest))

let resolve verdict ~cleanup_errors = { verdict; cleanup_errors }

let with_failure resolution error =
  { resolution with cleanup_errors = resolution.cleanup_errors @ [ error ] }

let report_status resolution =
  match (resolution.verdict, resolved_error resolution) with
  | Completed _, None -> Report_completed
  | Interrupted _, Some _ when resolution.cleanup_errors = [] ->
      Report_interrupted
  | (Completed _ | Failed _ | Interrupted _), Some error -> Report_failed error
  | Failed _, None | Interrupted _, None -> assert false

let completed_exit_code = function
  | All_detected -> 0
  | Unexpected_survivor -> 0
  | Contract_failure -> 2

let result resolution =
  match resolved_error resolution with
  | Some error -> Error error
  | None -> (
      match resolution.verdict with
      | Completed completed -> Ok (completed_exit_code completed)
      | Failed _ | Interrupted _ -> assert false)

let commit_failure resolution reporting =
  match resolved_error resolution with
  | None -> reporting
  | Some primary -> Error.suppress primary reporting

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

module Make (Services : SERVICES) = struct
  type installed_signal = { signal : int; token : Services.Signals.token }

  type 'a action_outcome =
    | Returned of ('a, Error.t) result
    | Raised of exn * Printexc.raw_backtrace

  type run_outcome =
    | Early_error of Error.t
    | Prepared of
        Services.Store.t
        * Services.Store.reservation
        * Services.draft preparation
    | Preparation_blocked of
        Error.t * Services.Store.t * Services.Store.reservation

  let unexpected_exception_error ~phase ~operation ~report_state exception_
      backtrace =
    Error.create ~phase ~cause:Error.Invariant_violation
      ~context:
        [
          ("operation", operation);
          ("exception", Printexc.to_string exception_);
          ("backtrace", Printexc.raw_backtrace_to_string backtrace);
          ("authoritative_report_state", report_state);
        ]
      "unexpected exception escaped %s" operation

  let protect ~phase ~operation ~report_state action =
    try Ok (action ())
    with exception_ ->
      Error
        (unexpected_exception_error ~phase ~operation ~report_state exception_
           (Printexc.get_raw_backtrace ()))

  let signal_error ~phase ~operation signal exception_ backtrace =
    Error.create ~phase ~cause:Error.Io_failure
      ~context:
        [
          ("signal", string_of_int signal);
          ("exception", Printexc.to_string exception_);
          ("backtrace", Printexc.raw_backtrace_to_string backtrace);
        ]
      "could not %s signal handler" operation

  let restore_all installed =
    let rec restore errors = function
      | [] -> List.rev errors
      | { signal; token } :: rest -> (
          try
            Services.Signals.restore token;
            restore errors rest
          with exception_ ->
            restore
              (signal_error ~phase:Error.Cleanup ~operation:"restore" signal
                 exception_
                 (Printexc.get_raw_backtrace ())
              :: errors)
              rest)
    in
    restore [] installed

  let finish ~phase ~operation ~report_state outcome cleanup_errors =
    match outcome with
    | Raised (exception_, backtrace) ->
        Error
          (suppress_all
             (unexpected_exception_error ~phase ~operation ~report_state
                exception_ backtrace)
             cleanup_errors)
    | Returned (Error primary) -> Error (suppress_all primary cleanup_errors)
    | Returned (Ok value) -> (
        match cleanup_errors with
        | [] -> Ok value
        | primary :: rest -> Error (suppress_all primary rest))

  let install_signals cancel =
    let rec install installed = function
      | [] -> Ok installed
      | signal :: rest -> (
          match
            try
              Ok
                (Services.Signals.install signal (fun () ->
                     Cancel.request cancel))
            with exception_ ->
              Error (exception_, Printexc.get_raw_backtrace ())
          with
          | Ok token -> install ({ signal; token } :: installed) rest
          | Error (exception_, backtrace) ->
              let primary =
                signal_error ~phase:Error.Cli ~operation:"install" signal
                  exception_ backtrace
              in
              Error (suppress_all primary (restore_all installed)))
    in
    install [] [ Sys.sigint; Sys.sigterm ]

  let with_cancel action =
    let cancel = Cancel.create () in
    match install_signals cancel with
    | Error _ as error -> error
    | Ok installed ->
        let outcome =
          try Returned (action cancel)
          with exception_ -> Raised (exception_, Printexc.get_raw_backtrace ())
        in
        finish ~phase:Error.Cli ~operation:"list-mutants"
          ~report_state:"not-applicable" outcome (restore_all installed)

  let reservation_cleanup_error exception_ backtrace =
    Error.create ~phase:Error.Cleanup ~cause:Error.Io_failure
      ~context:
        [
          ("exception", Printexc.to_string exception_);
          ("backtrace", Printexc.raw_backtrace_to_string backtrace);
        ]
      "could not abandon run reservation"

  let publication_interruption () =
    Error.create ~phase:Error.Reporting ~cause:Error.Interrupted_by_user
      ~context:[ ("operation", "commit-reserved") ]
      "run interrupted before report publication"

  let abandon store reservation =
    match
      try Services.Store.abandon_reservation store reservation
      with exception_ ->
        Error
          (reservation_cleanup_error exception_ (Printexc.get_raw_backtrace ()))
    with
    | Ok () -> []
    | Error error -> [ error ]

  let prepare_reserved ~cancel ~store ~reservation ~started_at ~root ~config
      ~fresh ~selection ~output =
    let cleanup_errors = function Ok () -> [] | Error error -> [ error ] in
    let failure_preparation primary prior_cleanup =
      match
        protect ~phase:Error.Reporting ~operation:"prepare-failure-draft"
          ~report_state:"unavailable-no-draft" (fun () ->
            Services.prepare_failure ~cancel ~store ~reservation ~started_at
              ~root ~config ~fresh ~selection ~output primary)
      with
      | Ok preparation ->
          let verdict =
            match Error.cause primary with
            | Error.Interrupted_by_user -> Interrupted primary
            | _ -> Failed primary
          in
          Ok
            {
              preparation with
              verdict;
              cleanup_errors = prior_cleanup @ preparation.cleanup_errors;
            }
      | Error fallback ->
          Error (suppress_all primary (prior_cleanup @ [ fallback ]))
    in
    match
      protect ~phase:Error.Snapshot ~operation:"workspace-bracket"
        ~report_state:"unavailable-no-draft" (fun () ->
          Services.Workspace.bracket root (fun snapshot ->
              Services.prepare_in_snapshot ~cancel ~store ~reservation
                ~started_at ~root ~config ~fresh ~selection ~output ~snapshot))
    with
    | Error bracket -> failure_preparation bracket []
    | Ok (Services.Workspace.Acquisition_failed error) ->
        failure_preparation error []
    | Ok (Services.Workspace.Action_returned (preparation, cleanup)) ->
        Ok
          {
            preparation with
            cleanup_errors = preparation.cleanup_errors @ cleanup_errors cleanup;
          }
    | Ok (Services.Workspace.Action_raised (exception_, backtrace, cleanup)) ->
        let primary =
          unexpected_exception_error ~phase:Error.Analysis
            ~operation:"prepare-in-snapshot"
            ~report_state:"pending-partial-report" exception_ backtrace
        in
        failure_preparation primary (cleanup_errors cleanup)

  let run_controlled ~cancel ~finish_subscription ~root ~config ~fresh
      ~selection ~output =
    let configured_cache = config.Config.cache.directory in
    let acquired =
      match
        protect ~phase:Error.Cache ~operation:"store-create"
          ~report_state:"unavailable-before-store" (fun () ->
            Services.Store.create ~workspace:root ?directory:configured_cache ())
      with
      | Error error | Ok (Error error) -> Early_error error
      | Ok (Ok store) -> (
          match
            protect ~phase:Error.Reporting ~operation:"started-at-clock"
              ~report_state:"unavailable-before-reservation" (fun () ->
                Services.Clock.now ())
          with
          | Error error -> Early_error error
          | Ok started_at -> (
              match
                protect ~phase:Error.Reporting ~operation:"store-reserve"
                  ~report_state:"unavailable-reservation-handle" (fun () ->
                    Services.Store.reserve store ~started_at)
              with
              | Error error | Ok (Error error) -> Early_error error
              | Ok (Ok reservation) -> (
                  match
                    prepare_reserved ~cancel ~store ~reservation ~started_at
                      ~root ~config ~fresh ~selection ~output
                  with
                  | Ok preparation -> Prepared (store, reservation, preparation)
                  | Error error ->
                      Preparation_blocked (error, store, reservation))))
    in
    match acquired with
    | Early_error error -> Error (suppress_all error (finish_subscription ()))
    | Preparation_blocked (error, store, reservation) ->
        Error
          (suppress_all error
             (abandon store reservation @ finish_subscription ()))
    | Prepared (store, reservation, preparation) -> (
        let prepared_resolution =
          resolve preparation.verdict ~cleanup_errors:preparation.cleanup_errors
        in
        let resolution, committed =
          match
            protect ~phase:Error.Reporting ~operation:"finished-at-clock"
              ~report_state:"unavailable-metadata-incomplete" (fun () ->
                Services.Clock.now ())
          with
          | Error error -> (prepared_resolution, Error error)
          | Ok finished_at -> (
              let resolution =
                if Cancel.is_requested cancel then
                  match preparation.verdict with
                  | Interrupted _ -> prepared_resolution
                  | Completed _ | Failed _ ->
                      resolve
                        (Interrupted (publication_interruption ()))
                        ~cleanup_errors:preparation.cleanup_errors
                else prepared_resolution
              in
              match
                protect ~phase:Error.Reporting ~operation:"commit-reserved"
                  ~report_state:"indeterminate" (fun () ->
                    Services.commit_reserved ~store ~reservation ~finished_at
                      ~resolution preparation.draft)
              with
              | Error error -> (resolution, Error error)
              | Ok result -> (resolution, result))
        in
        (* A conforming process-lifetime signal service only deactivates
           in-memory subscriptions here, so this handoff is total and the OS
           router remains installed. Keeping the subscriptions live through
           [commit_reserved] closes the former interrupt gap. The cancellation
           check immediately before that call is the report decision's
           linearization point; later interrupts cannot rewrite a possibly
           committed immutable report. Interactive callers have no installed
           process subscription and supply a no-op finalizer. *)
        let subscription_errors = finish_subscription () in
        match committed with
        | Ok resolution -> (
            match subscription_errors with
            | [] -> result resolution
            | _ ->
                Error
                  (suppress_all
                     (Error.create ~phase:Error.Cleanup
                        ~cause:Error.Invariant_violation
                        ~context:
                          [ ("contract", "process-lifetime-unsubscribe-total") ]
                        "process-lifetime cancellation unsubscribe violated \
                         its totality contract")
                     subscription_errors))
        | Error reporting ->
            let primary = commit_failure resolution reporting in
            Error
              (suppress_all primary
                 (subscription_errors @ abandon store reservation)))

  let run_with_cancel ~cancel ~root ~config ~fresh ~selection ~output =
    run_controlled ~cancel
      ~finish_subscription:(fun () -> [])
      ~root ~config ~fresh ~selection ~output

  let run ~root ~config ~fresh ~selection ~output =
    let cancel = Cancel.create () in
    match install_signals cancel with
    | Error _ as error -> error
    | Ok installed ->
        run_controlled ~cancel
          ~finish_subscription:(fun () -> restore_all installed)
          ~root ~config ~fresh ~selection ~output

  let list_mutants ~root ~config ~selection ~output =
    with_cancel (fun cancel ->
        Services.list_mutants ~cancel ~root ~config ~selection ~output)
end
