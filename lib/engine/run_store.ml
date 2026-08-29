open Util
module Core = Ocaml_mutants_core

module Publication_deletion_authority = struct
  type dir = Dir_cap.System.dir
  type identity = Dir_cap.System.Identity.t
  type owner = Dir_cap.System.owner
  type t = unit

  let unsupported () =
    Error
      (Dir_cap.failure_of_error
         (Dir_cap.make_error ~operation:Dir_cap.Conditional_unlink
            ~class_:Dir_cap.Unsupported ~native_domain:Dir_cap.Contract
            ~native_code:"publication-adapter-has-no-deletion-authority" ()))

  let establish () ~root:_ ~identity:_ ~owner:_ = unsupported ()
  let validate_continuation () ~root:_ ~identity:_ ~owner:_ = unsupported ()
end

module Publication_cache =
  Cache_fs.Make (Dir_cap.System) (Publication_deletion_authority)

module Store_path = struct
  type t = Cache_fs.Relative.t

  let root = Cache_fs.Relative.root
  let equal = Cache_fs.Relative.equal

  let child path component =
    match Cache_fs.Name.of_string component with
    | Ok name -> Ok (Cache_fs.Relative.child path name)
    | Error reason ->
        Error
          (Format.asprintf "invalid internal store path component %S: %a"
             component Cache_fs.Name.pp_error reason)

  let child_internal path component =
    match child path component with
    | Ok path -> path
    | Error message -> invalid_arg message

  let of_internal_components components =
    List.fold_left child_internal root components

  let render ~root path =
    List.fold_left
      (fun parent component ->
        Filename.concat parent (Cache_fs.Name.to_string component))
      root
      (Cache_fs.Relative.components path)
end

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

let not_run run =
  match run.completeness with Complete -> [] | Partial mutants -> mutants

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

type native_root_lease = {
  root : Publication_cache.root;
  lock : Publication_cache.lock;
}

type cleanup_resource =
  | Cleanup_stage of Publication_cache.staged_file
  | Cleanup_marker of { stage : Publication_cache.staged_file; path : string }
  | Cleanup_retry of Publication_cache.cleanup_retry
  | Cleanup_listing of Publication_cache.listing
  | Cleanup_lock of Publication_cache.lock
  | Cleanup_root of Publication_cache.root

type cleanup_resources = cleanup_resource list

type lease_release = {
  errors : Error.t list;
  residual : cleanup_resources option;
}

type publication_failure = {
  error : Error.t;
  retained_resources : cleanup_resources;
}

type publication_success = {
  advisories : Error.t list;
  retained_resources : cleanup_resources;
}

type operations = {
  stage_pending_report :
    root:Publication_cache.root ->
    pending:Store_path.t ->
    display_path:string ->
    root_display:string ->
    contents:string ->
    (Publication_cache.staged_file, publication_failure) result;
  publish_report :
    stage:Publication_cache.staged_file ->
    pending_display:string ->
    final:Store_path.t ->
    final_display:string ->
    root_display:string ->
    (publication_success, publication_failure) result;
  update_latest :
    root:Publication_cache.root ->
    latest:Store_path.t ->
    display_path:string ->
    id:string ->
    root_display:string ->
    (publication_success, publication_failure) result;
  discard_marker :
    root:string -> path:string -> Publication_cache.staged_file -> lease_release;
  release_lease : root:string -> cleanup_resources -> lease_release;
  publication_release_lease : root:string -> cleanup_resources -> lease_release;
}

type topology = {
  scope : Store_path.t;
  runs : Store_path.t;
  outcomes : Store_path.t;
  journals : Store_path.t;
  latest : Store_path.t;
}

type t = {
  root : string;
  workspace : string option;
  lock_boundary : string;
  topology : topology;
  operations : operations;
  next_reservation_sequence : unit -> (int64, string) result;
}

type reservation_state = Active | Staged | Finalized

type shared_root_lease = {
  root_key : string;
  native : native_root_lease;
  mutable references : int;
  mutable retained_resources : cleanup_resources;
  mutable retained_error : Error.t option;
}

type reservation_lease = {
  shared : shared_root_lease;
  mutable operations : operations;
  mutable retained_resources : cleanup_resources;
  mutable retained_error : Error.t option;
  mutable released : bool;
}

type reservation = {
  id : Core.Run_id.t;
  owner_pid : int;
  scope : Store_path.t;
  marker_path : Store_path.t;
  mutable marker_stage : Publication_cache.staged_file option;
  lease : reservation_lease;
  mutex : Mutex.t;
  mutable state : reservation_state;
}

type journal = {
  store : t;
  reservation : reservation;
  key : string;
  session_id : Core.Run_id.t;
  directory : Store_path.t;
  state_path : Store_path.t;
  resumed : bool;
  mutable closed : bool;
}

type maintenance_lease = {
  root_key : string;
  native : native_root_lease;
  operations : operations;
  mutable retained_resources : cleanup_resources;
  mutable retained_error : Error.t option;
  mutable released : bool;
}

type staged_state = Awaiting_finalization | Finalization_issued

type staged_run = {
  store : t;
  reservation : reservation;
  pending_path : Store_path.t;
  final_path : Store_path.t;
  mutable staged_state : staged_state;
}

type publish_state = Ready_to_publish | Publish_attempted

type publish_capability = {
  staged : staged_run;
  lease : reservation_lease;
  mutable publish_state : publish_state;
}

type finalization = {
  publication : publish_capability;
  cleanup_errors : Error.t list;
}

type published_run = { path : string; run : run; advisories : Error.t list }

type local_root_lease =
  | Shared of shared_root_lease
  | Maintenance of maintenance_lease
  | Poisoned of { error : Error.t; residual : cleanup_resources }

let marker = ".ocaml-mutants-cache-v2"
let marker_contents = "owner=ocaml-mutants\nschema=2\n"
let maintenance_lock = ".ocaml-mutants-maintenance-v1.lock"
let marker_path = Store_path.of_internal_components [ marker ]

let maintenance_lock_path =
  Store_path.of_internal_components [ maintenance_lock ]

let legacy_outcome_path = Store_path.of_internal_components [ "o" ]

let legacy_outcomes_v2_path =
  Store_path.of_internal_components [ "outcomes-v2" ]

let legacy_runs_path = Store_path.of_internal_components [ "runs" ]
let workspace_scopes_path = Store_path.of_internal_components [ "w" ]
let root_latest_path = Store_path.of_internal_components [ "latest" ]

let topology scope =
  {
    scope;
    runs = Store_path.child_internal scope "runs";
    outcomes = Store_path.child_internal scope "o";
    journals = Store_path.child_internal scope "j";
    latest = Store_path.child_internal scope "latest";
  }

let store_path (store : t) relative =
  Store_path.render ~root:store.root relative

let scope_display (store : t) = store_path store store.topology.scope
let lease_registry_mutex = ref (Mutex.create ())
let lease_registry_pid = ref (Unix.getpid ())
let root_leases = Hashtbl.create 17

type reservation_sequence_state = Next_sequence of int64 | Sequence_exhausted

let reservation_sequence_mutex = ref (Mutex.create ())
let reservation_sequence_pid = ref (Unix.getpid ())
let reservation_sequence_state = ref (Next_sequence 0L)

let reset_reservation_sequence_after_fork () =
  let process = Unix.getpid () in
  if process <> !reservation_sequence_pid then (
    reservation_sequence_pid := process;
    reservation_sequence_state := Next_sequence 0L;
    reservation_sequence_mutex := Mutex.create ())

let next_process_reservation_sequence () =
  reset_reservation_sequence_after_fork ();
  let mutex = !reservation_sequence_mutex in
  Mutex.lock mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock mutex)
    (fun () ->
      match !reservation_sequence_state with
      | Sequence_exhausted -> Error "run reservation sequence is exhausted"
      | Next_sequence current ->
          reservation_sequence_state :=
            if Int64.equal current Int64.max_int then Sequence_exhausted
            else Next_sequence (Int64.succ current);
          Ok current)

let captured ?(truncated = false) ?total_bytes contents =
  {
    contents;
    truncated;
    total_bytes = Option.value total_bytes ~default:(String.length contents);
  }

let cache_root () =
  if Sys.win32 then
    Option.value
      (Sys.getenv_opt "LOCALAPPDATA")
      ~default:(Filename.get_temp_dir_name ())
  else
    match Sys.getenv_opt "XDG_CACHE_HOME" with
    | Some value -> value
    | None ->
        Filename.concat
          (Option.value (Sys.getenv_opt "HOME")
             ~default:(Filename.get_temp_dir_name ()))
          ".cache"

let default_directory () = Filename.concat (cache_root ()) "ocaml-mutants"

let normalize_for_compare path =
  let path = Core.Mutant.normalize_path path in
  if Sys.win32 then String.lowercase_ascii path else path

let reset_registry_after_fork () =
  let process = Unix.getpid () in
  if process <> !lease_registry_pid then (
    let release_locally =
      List.iter (function
        | Cleanup_stage stage -> ignore (Publication_cache.close_stage stage)
        | Cleanup_marker { stage; _ } ->
            ignore (Publication_cache.discard_stage stage)
        | Cleanup_retry retry -> ignore (Publication_cache.retry_cleanup retry)
        | Cleanup_listing listing ->
            ignore (Publication_cache.close_listing listing)
        | Cleanup_lock lock -> ignore (Publication_cache.release_lock lock)
        | Cleanup_root root -> ignore (Publication_cache.close_root root))
    in
    Hashtbl.iter
      (fun _ -> function
        | Shared shared ->
            release_locally
              (shared.retained_resources
              @ [
                  Cleanup_lock shared.native.lock;
                  Cleanup_root shared.native.root;
                ])
        | Maintenance lease ->
            release_locally
              (lease.retained_resources
              @ [
                  Cleanup_lock lease.native.lock; Cleanup_root lease.native.root;
                ])
        | Poisoned { residual; _ } -> release_locally residual)
      root_leases;
    Hashtbl.reset root_leases;
    lease_registry_pid := process;
    lease_registry_mutex := Mutex.create ())

let with_lease_registry action =
  reset_registry_after_fork ();
  let mutex = !lease_registry_mutex in
  Mutex.lock mutex;
  Fun.protect action ~finally:(fun () -> Mutex.unlock mutex)

let lease_error ~phase ~cause ?(context = []) format =
  Error.create ~phase ~cause ~context format

let maintenance_busy ?active_reservations root =
  let context =
    ("cache_root", root)
    :: Option.fold ~none:[]
         ~some:(fun count -> [ ("active_reservations", string_of_int count) ])
         active_reservations
  in
  lease_error ~phase:Error.Cache ~cause:Error.Resource_busy ~context
    "cache maintenance refused while an active run or maintenance process \
     holds the root lease"

let reservation_busy root =
  lease_error ~phase:Error.Reporting ~cause:Error.Resource_busy
    ~context:[ ("cache_root", root) ]
    "cannot reserve a run while cache maintenance holds the root lease"

let dir_cap_class_name = function
  | Dir_cap.Missing -> "missing"
  | Already_exists -> "already-exists"
  | Not_directory -> "not-directory"
  | Not_regular -> "not-regular"
  | Not_link -> "not-link"
  | Link_like -> "link-like"
  | Too_large -> "too-large"
  | Invalid_name -> "invalid-name"
  | Access_denied -> "access-denied"
  | Busy -> "busy"
  | Unsupported -> "unsupported"
  | Wrong_process -> "wrong-process"
  | Closed_capability -> "closed-capability"
  | Other -> "other"

let cache_class_name = function
  | Cache_fs.Missing -> "missing"
  | Already_exists -> "already-exists"
  | Busy -> "busy"
  | Link_like -> "link-like"
  | Not_directory -> "not-directory"
  | Not_regular -> "not-regular"
  | Too_large -> "too-large"
  | Budget_exhausted -> "budget-exhausted"
  | Access_denied -> "access-denied"
  | Unsupported -> "unsupported"
  | Invalid_name -> "invalid-name"
  | Unsafe_relationship -> "unsafe-relationship"
  | Identity_changed -> "identity-changed"
  | Closed_capability -> "closed-capability"
  | Wrong_process -> "wrong-process"
  | Backend_contract_violation -> "backend-contract-violation"
  | Other -> "other"

let dir_cap_domain_name = function
  | Dir_cap.Posix_errno -> "posix-errno"
  | Win32 -> "win32"
  | Ntstatus -> "ntstatus"
  | In_memory -> "in-memory"
  | Contract -> "contract"

let cache_domain_name = function
  | Cache_fs.Posix_errno -> "posix-errno"
  | Win32 -> "win32"
  | Ntstatus -> "ntstatus"
  | In_memory -> "in-memory"
  | Contract -> "contract"

let local_state_name = function
  | Dir_cap.Still_open -> "still-open"
  | Closed -> "closed"
  | Invalidated_unknown -> "invalidated-unknown"

let cache_progress_name = function
  | Cache_fs.Handle_closed -> "closed"
  | Handle_invalidated_unknown -> "invalidated-unknown"
  | Handle_still_open _ -> "still-open"

let native_publication_error ~path ~phase (error : Cache_fs.native_error) =
  let primitive =
    match error.primitive_operation with
    | None -> "none"
    | Some operation -> Dir_cap.operation_name operation
  in
  Error.create ~phase ~cause:Error.Io_failure
    ~context:
      ([
         ("path", path);
         ("operation", Cache_fs.operation_name error.operation);
         ("primitive_operation", primitive);
         ("class", cache_class_name error.class_);
         ("native_domain", cache_domain_name error.native_domain);
         ("native_code", error.native_code);
       ]
      @
      match error.component with
      | None -> []
      | Some component -> [ ("component", component) ])
    "native report publication operation failed"

let dir_cap_publication_error ~path ~phase (error : Dir_cap.error) =
  Error.create ~phase ~cause:Error.Io_failure
    ~context:
      ([
         ("path", path);
         ("primitive_operation", Dir_cap.operation_name error.operation);
         ("class", dir_cap_class_name error.class_);
         ("native_domain", dir_cap_domain_name error.native_domain);
         ("native_code", error.native_code);
       ]
      @
      match error.component with
      | None -> []
      | Some component -> [ ("component", component) ])
    "directory capability operation for report publication failed"

let suppress_all primary suppressed =
  List.fold_left Error.suppress primary suppressed

let dir_cap_cleanup_problem_error ~path (problem : Dir_cap.cleanup_problem) =
  dir_cap_publication_error ~path ~phase:Error.Cleanup problem.error
  |> Error.with_context "local_handle_state"
       (local_state_name problem.local_handle_state)
  |> Error.with_context "namespace_released"
       (string_of_bool problem.namespace_released)

let dir_cap_cleanup_failure_error ~path (failure : Dir_cap.cleanup_failure) =
  suppress_all
    (dir_cap_cleanup_problem_error ~path failure.primary)
    (List.map (dir_cap_cleanup_problem_error ~path) failure.suppressed)

let dir_cap_issue_error ~path = function
  | Dir_cap.Operation_error error ->
      dir_cap_publication_error ~path ~phase:Error.Reporting error
  | Cleanup_error failure -> dir_cap_cleanup_failure_error ~path failure

let dir_cap_failure_error ~path (failure : Dir_cap.failure) =
  suppress_all
    (dir_cap_issue_error ~path failure.primary)
    (List.map (dir_cap_issue_error ~path) failure.suppressed)

let cache_cleanup_problem_error ~path
    (problem : Publication_cache.cleanup_retry Cache_fs.cleanup_problem) =
  native_publication_error ~path ~phase:Error.Cleanup problem.error
  |> Error.with_context "local_handle_state"
       (cache_progress_name problem.local_handle)
  |> Error.with_context "namespace_released"
       (string_of_bool problem.namespace_released)

let cache_cleanup_failure_error ~path
    (failure : Publication_cache.cleanup_retry Cache_fs.cleanup_failure) =
  suppress_all
    (cache_cleanup_problem_error ~path failure.primary)
    (List.map (cache_cleanup_problem_error ~path) failure.suppressed)

let cache_issue_error ~path = function
  | Cache_fs.Operation_error error ->
      native_publication_error ~path ~phase:Error.Reporting error
  | Cleanup_error failure -> cache_cleanup_failure_error ~path failure

let cache_failure_error ~path (failure : Publication_cache.operation_failure) =
  suppress_all
    (cache_issue_error ~path failure.primary)
    (List.map (cache_issue_error ~path) failure.suppressed)

let cache_advisory_errors ~path advisories =
  List.map (cache_issue_error ~path) advisories

let native_lease_dir_error ~root ~phase (error : Dir_cap.error) =
  Error.create ~phase ~cause:Error.Io_failure
    ~context:
      ([
         ("cache_root", root);
         ("primitive_operation", Dir_cap.operation_name error.operation);
         ("class", dir_cap_class_name error.class_);
         ("native_domain", dir_cap_domain_name error.native_domain);
         ("native_code", error.native_code);
       ]
      @
      match error.component with
      | None -> []
      | Some component -> [ ("component", component) ])
    "native cache root lease operation failed"

let native_lease_dir_cleanup_problem ~root (problem : Dir_cap.cleanup_problem) =
  native_lease_dir_error ~root ~phase:Error.Cleanup problem.error
  |> Error.with_context "local_handle_state"
       (local_state_name problem.local_handle_state)
  |> Error.with_context "namespace_released"
       (string_of_bool problem.namespace_released)

let native_lease_dir_cleanup_failure ~root (failure : Dir_cap.cleanup_failure) =
  suppress_all
    (native_lease_dir_cleanup_problem ~root failure.primary)
    (List.map (native_lease_dir_cleanup_problem ~root) failure.suppressed)

let native_lease_dir_issue ~root ~phase = function
  | Dir_cap.Operation_error error -> native_lease_dir_error ~root ~phase error
  | Cleanup_error failure -> native_lease_dir_cleanup_failure ~root failure

let native_lease_dir_failure ~root ~phase (failure : Dir_cap.failure) =
  suppress_all
    (native_lease_dir_issue ~root ~phase failure.primary)
    (List.map (native_lease_dir_issue ~root ~phase) failure.suppressed)

let native_lease_cache_error ~root ~phase (error : Cache_fs.native_error) =
  let primitive =
    match error.primitive_operation with
    | None -> "none"
    | Some operation -> Dir_cap.operation_name operation
  in
  Error.create ~phase ~cause:Error.Io_failure
    ~context:
      ([
         ("cache_root", root);
         ("operation", Cache_fs.operation_name error.operation);
         ("primitive_operation", primitive);
         ("class", cache_class_name error.class_);
         ("native_domain", cache_domain_name error.native_domain);
         ("native_code", error.native_code);
       ]
      @
      match error.component with
      | None -> []
      | Some component -> [ ("component", component) ])
    "native cache root lease operation failed"

let native_lease_cache_cleanup_problem ~root
    (problem : Publication_cache.cleanup_retry Cache_fs.cleanup_problem) =
  native_lease_cache_error ~root ~phase:Error.Cleanup problem.error
  |> Error.with_context "local_handle_state"
       (cache_progress_name problem.local_handle)
  |> Error.with_context "namespace_released"
       (string_of_bool problem.namespace_released)

let native_lease_cache_cleanup_failure ~root
    (failure : Publication_cache.cleanup_retry Cache_fs.cleanup_failure) =
  suppress_all
    (native_lease_cache_cleanup_problem ~root failure.primary)
    (List.map (native_lease_cache_cleanup_problem ~root) failure.suppressed)

let native_lease_cache_issue ~root ~phase = function
  | Cache_fs.Operation_error error ->
      native_lease_cache_error ~root ~phase error
  | Cleanup_error failure -> native_lease_cache_cleanup_failure ~root failure

let native_lease_cache_failure ~root ~phase
    (failure : Publication_cache.operation_failure) =
  suppress_all
    (native_lease_cache_issue ~root ~phase failure.primary)
    (List.map (native_lease_cache_issue ~root ~phase) failure.suppressed)

let native_lease_cache_advisories ~root ~phase advisories =
  List.map (native_lease_cache_issue ~root ~phase) advisories

let cleanup_retries_of_problem
    (problem : Publication_cache.cleanup_retry Cache_fs.cleanup_problem) =
  match problem.local_handle with
  | Cache_fs.Handle_still_open retry -> [ retry ]
  | Handle_closed | Handle_invalidated_unknown -> []

let cleanup_retries_of_cleanup_failure
    (failure : Publication_cache.cleanup_retry Cache_fs.cleanup_failure) =
  List.concat_map cleanup_retries_of_problem
    (failure.primary :: failure.suppressed)

let cleanup_retries_of_issue = function
  | Cache_fs.Operation_error _ -> []
  | Cleanup_error failure -> cleanup_retries_of_cleanup_failure failure

let unique_cleanup_retries retries =
  List.fold_left
    (fun unique retry ->
      if List.exists (fun existing -> existing == retry) unique then unique
      else unique @ [ retry ])
    [] retries

let cleanup_retries_of_failure (failure : Publication_cache.operation_failure) =
  List.concat_map cleanup_retries_of_issue
    (failure.primary :: failure.suppressed)
  |> unique_cleanup_retries

let cleanup_retries_of_advisories advisories =
  List.concat_map cleanup_retries_of_issue advisories |> unique_cleanup_retries

let retry_resources_of_failure failure =
  List.map
    (fun retry -> Cleanup_retry retry)
    (cleanup_retries_of_failure failure)

let retry_resources_of_advisories advisories =
  List.map
    (fun retry -> Cleanup_retry retry)
    (cleanup_retries_of_advisories advisories)

let residual_observation_name = function
  | Publication_cache.Residual_creation_observed { name } ->
      "observed:" ^ Publication_cache.Native_name.encode name
  | Residual_creation_may_have_committed { name } ->
      "may-have-committed:" ^ Publication_cache.Native_name.encode name

let add_residual_context error residuals =
  match residuals with
  | [] -> error
  | _ ->
      Error.with_context "residuals"
        (String.concat "," (List.map residual_observation_name residuals))
        error

let cleanup_resources_are_empty = function [] -> true | _ :: _ -> false

let rec cleanup_native_resources ~root resources =
  let retain errors live wrap retained failure =
    ( errors @ [ native_lease_cache_failure ~root ~phase:Error.Cleanup failure ],
      live @ Option.to_list (Option.map wrap retained) )
  in
  let close (errors, live) = function
    | Cleanup_stage stage -> (
        match Publication_cache.close_stage stage with
        | Cache_fs.Teardown_complete | Teardown_local_only -> (errors, live)
        | Teardown_incomplete { live = retained; failure } ->
            retain errors live
              (fun stage -> Cleanup_stage stage)
              retained failure)
    | Cleanup_marker { stage; path } -> (
        match Publication_cache.discard_stage stage with
        | Publication_cache.Stage_discarded { advisories } ->
            let retried =
              cleanup_native_resources ~root
                (retry_resources_of_advisories advisories)
            in
            ( errors
              @ native_lease_cache_advisories ~root ~phase:Error.Cleanup
                  advisories
              @ retried.errors,
              live @ Option.value ~default:[] retried.residual )
        | Stage_discard_local_only { advisories } ->
            let primary =
              lease_error ~phase:Error.Cleanup ~cause:Error.Invariant_violation
                ~context:
                  [
                    ("cache_root", root);
                    ("path", path);
                    ("operation", "discard-reservation-marker");
                    ("namespace_released", "false");
                  ]
                "reservation marker cleanup released local handles without \
                 deleting its captured namespace entry"
            in
            let retried =
              cleanup_native_resources ~root
                (retry_resources_of_advisories advisories)
            in
            ( errors @ [ primary ]
              @ native_lease_cache_advisories ~root ~phase:Error.Cleanup
                  advisories
              @ retried.errors,
              live @ Option.value ~default:[] retried.residual )
        | Stage_discard_retained { live_stage; failure } ->
            ( errors
              @ [
                  native_lease_cache_failure ~root ~phase:Error.Cleanup failure
                  |> Error.with_context "path" path;
                ],
              live @ [ Cleanup_marker { stage = live_stage; path } ] )
        | Stage_discard_incomplete_audit_only { residual; failure } ->
            let primary =
              add_residual_context
                (native_lease_cache_failure ~root ~phase:Error.Cleanup failure)
                [ residual ]
              |> Error.with_context "path" path
            in
            let retried =
              cleanup_native_resources ~root
                (retry_resources_of_failure failure)
            in
            ( errors @ [ primary ] @ retried.errors,
              live @ Option.value ~default:[] retried.residual ))
    | Cleanup_retry retry -> (
        match Publication_cache.retry_cleanup retry with
        | Cache_fs.Teardown_complete | Teardown_local_only -> (errors, live)
        | Teardown_incomplete { live = retained; failure } ->
            retain errors live
              (fun retry -> Cleanup_retry retry)
              retained failure)
    | Cleanup_listing listing -> (
        match Publication_cache.close_listing listing with
        | Cache_fs.Teardown_complete | Teardown_local_only -> (errors, live)
        | Teardown_incomplete { live = retained; failure } ->
            retain errors live
              (fun listing -> Cleanup_listing listing)
              retained failure)
    | Cleanup_lock lock -> (
        match Publication_cache.release_lock lock with
        | Cache_fs.Teardown_complete | Teardown_local_only -> (errors, live)
        | Teardown_incomplete { live = retained; failure } ->
            retain errors live (fun lock -> Cleanup_lock lock) retained failure)
    | Cleanup_root capability -> (
        match Publication_cache.close_root capability with
        | Cache_fs.Teardown_complete | Teardown_local_only -> (errors, live)
        | Teardown_incomplete { live = retained; failure } ->
            retain errors live (fun root -> Cleanup_root root) retained failure)
  in
  let errors, residual = List.fold_left close ([], []) resources in
  {
    errors;
    residual =
      (if cleanup_resources_are_empty residual then None else Some residual);
  }

let native_lock_recreated_root ~phase root created =
  Error.create ~phase ~cause:Error.Workspace_violation
    ~context:
      [
        ("cache_root", root);
        ("created_components", string_of_int (List.length created));
      ]
    "native root lease acquisition refused a cache root that had to be \
     recreated"

type native_acquisition_failure = {
  error : Error.t;
  residual : cleanup_resources option;
}

let acquisition_failure ~root primary resources =
  let cleanup = cleanup_native_resources ~root resources in
  { error = suppress_all primary cleanup.errors; residual = cleanup.residual }

let acquisition_observation_name = function
  | Publication_cache.Creation_observed name ->
      "observed:" ^ Publication_cache.Native_name.encode name
  | Creation_may_have_committed name ->
      "may-have-committed:" ^ Publication_cache.Native_name.encode name

let add_acquisition_context error created =
  match created with
  | [] -> error
  | _ ->
      Error.with_context "created_components"
        (String.concat "," (List.map acquisition_observation_name created))
        error

let acquire_native_root_candidate ~phase (store : t) =
  match Dir_cap.System.probe_path store.lock_boundary with
  | Error failure ->
      Error
        {
          error =
            native_lease_dir_failure ~root:store.root ~phase failure
            |> Error.with_context "forbidden_boundary" store.lock_boundary;
          residual = None;
        }
  | Ok workspace -> (
      match Publication_cache.acquire ~workspace ~requested:store.root with
      | Publication_cache.Acquisition_incomplete { created; failure } ->
          Error
            (acquisition_failure ~root:store.root
               (add_acquisition_context
                  (native_lease_cache_failure ~root:store.root ~phase failure)
                  created)
               (retry_resources_of_failure failure))
      | Acquired acquisition -> (
          let root = acquisition.root in
          let acquisition_errors =
            native_lease_cache_advisories ~root:store.root ~phase
              acquisition.advisories
          in
          let acquisition_retries =
            retry_resources_of_advisories acquisition.advisories
          in
          match acquisition_errors with
          | [] -> Ok acquisition
          | primary :: suppressed ->
              Error
                (acquisition_failure ~root:store.root
                   (suppress_all primary suppressed)
                   (Cleanup_root root :: acquisition_retries))))

let acquire_native_root ~phase (store : t) =
  match acquire_native_root_candidate ~phase store with
  | Error _ as error -> error
  | Ok acquisition ->
      if acquisition.created = [] then Ok acquisition.root
      else
        Error
          (acquisition_failure ~root:store.root
             (native_lock_recreated_root ~phase store.root acquisition.created)
             [ Cleanup_root acquisition.root ])

let ownership_marker_error ~phase store message =
  Error.create ~phase ~cause:Error.Corrupt_cache
    ~context:
      [ ("cache_root", store.root); ("marker", store_path store marker_path) ]
    "cache ownership marker verification failed: %s" message

type ownership_state =
  | Ownership_verified
  | Ownership_missing
  | Ownership_other

let inspect_cache_ownership ~phase (store : t) root =
  match
    Publication_cache.read_regular root marker_path
      ~limit:(Int64.of_int (String.length marker_contents))
  with
  | Error failure ->
      Error
        ( suppress_all
            (ownership_marker_error ~phase store "cannot read the exact marker")
            [ native_lease_cache_failure ~root:store.root ~phase failure ],
          retry_resources_of_failure failure )
  | Ok Cache_fs.Read_missing -> Ok Ownership_missing
  | Ok (Cache_fs.Contents read) when String.equal read.contents marker_contents
    ->
      Ok Ownership_verified
  | Ok (Cache_fs.Contents _) -> Ok Ownership_other

let verify_cache_ownership ~phase (store : t) root =
  match inspect_cache_ownership ~phase store root with
  | Error _ as error -> error
  | Ok Ownership_verified -> Ok ()
  | Ok Ownership_missing ->
      Error (ownership_marker_error ~phase store "marker is missing", [])
  | Ok Ownership_other ->
      Error (ownership_marker_error ~phase store "marker contents differ", [])

let native_lock_owner_error ~phase store =
  Error.create ~phase ~cause:Error.Invariant_violation
    ~context:[ ("cache_root", store.root) ]
    "native cache lock and retained root have different owners"

let acquire_native_lock ~phase (store : t) mode =
  match acquire_native_root ~phase store with
  | Error failure -> Error failure
  | Ok root -> (
      let fail primary resources =
        Error (acquisition_failure ~root:store.root primary resources)
      in
      match Publication_cache.try_lock root maintenance_lock_path mode with
      | Error failure ->
          fail
            (native_lease_cache_failure ~root:store.root ~phase failure)
            (retry_resources_of_failure failure @ [ Cleanup_root root ])
      | Ok acquired -> (
          let lock_errors =
            native_lease_cache_advisories ~root:store.root ~phase
              acquired.advisories
          in
          let lock_retries =
            retry_resources_of_advisories acquired.advisories
          in
          match acquired.disposition with
          | `Busy -> (
              let cleanup =
                cleanup_native_resources ~root:store.root
                  (lock_retries @ [ Cleanup_root root ])
              in
              match lock_errors @ cleanup.errors with
              | [] -> Ok `Busy
              | primary :: suppressed ->
                  Error
                    {
                      error = suppress_all primary suppressed;
                      residual = cleanup.residual;
                    })
          | `Acquired lock -> (
              let native = { root; lock } in
              let resources =
                [ Cleanup_lock lock ] @ lock_retries @ [ Cleanup_root root ]
              in
              match lock_errors with
              | primary :: suppressed ->
                  fail (suppress_all primary suppressed) resources
              | [] -> (
                  if
                    not
                      (Publication_cache.owner_equal
                         (Publication_cache.root_owner root)
                         (Publication_cache.lock_owner lock))
                  then fail (native_lock_owner_error ~phase store) resources
                  else
                    match verify_cache_ownership ~phase store root with
                    | Ok () -> Ok (`Acquired native)
                    | Error (primary, ownership_retries) ->
                        fail primary (ownership_retries @ resources)))))

let residual_resources (released : lease_release) =
  Option.value ~default:[] released.residual

let publication_failure_after_cleanup ~root primary resources =
  let released = cleanup_native_resources ~root resources in
  {
    error = suppress_all primary released.errors;
    retained_resources = residual_resources released;
  }

let publication_success_after_advisories ~root ~path advisories =
  let released =
    cleanup_native_resources ~root (retry_resources_of_advisories advisories)
  in
  {
    advisories = cache_advisory_errors ~path advisories @ released.errors;
    retained_resources = residual_resources released;
  }

let close_stage_after_failure ?(after_stage = []) ~root ~path primary stage =
  let stage_failure =
    match Publication_cache.close_stage stage with
    | Cache_fs.Teardown_complete | Teardown_local_only ->
        { error = primary; retained_resources = [] }
    | Teardown_incomplete { live; failure } ->
        {
          error = suppress_all primary [ cache_failure_error ~path failure ];
          retained_resources =
            List.map (fun stage -> Cleanup_stage stage) (Option.to_list live);
        }
  in
  let released = cleanup_native_resources ~root after_stage in
  {
    error = suppress_all stage_failure.error released.errors;
    retained_resources =
      stage_failure.retained_resources @ residual_resources released;
  }

let native_stage_pending ~root ~pending ~display_path ~root_display ~contents =
  match Publication_cache.stage_file root pending ~contents with
  | Publication_cache.Staged stage -> Ok stage
  | Staging_not_created failure ->
      Error
        (publication_failure_after_cleanup ~root:root_display
           (cache_failure_error ~path:display_path failure)
           (retry_resources_of_failure failure))
  | Staging_incomplete_actionable { live_stage; failure } ->
      close_stage_after_failure ~root:root_display ~path:display_path
        (cache_failure_error ~path:display_path failure)
        live_stage
      |> Result.error
  | Staging_incomplete_audit_only { residual; failure } ->
      Error
        (publication_failure_after_cleanup ~root:root_display
           (add_residual_context
              (cache_failure_error ~path:display_path failure)
              [ residual ])
           (retry_resources_of_failure failure))

let native_publish_stage ~stage ~pending_display ~final ~final_display
    ~root_display =
  match Publication_cache.publish_no_replace stage ~target:final with
  | Publication_cache.Stage_published { advisories } ->
      Ok
        (publication_success_after_advisories ~root:root_display
           ~path:final_display advisories)
  | Stage_not_published { live_stage; failure } ->
      close_stage_after_failure ~root:root_display ~path:pending_display
        ~after_stage:(retry_resources_of_failure failure)
        (cache_failure_error ~path:final_display failure)
        live_stage
      |> Result.error
  | Stage_publish_rejected failure ->
      close_stage_after_failure ~root:root_display ~path:pending_display
        ~after_stage:(retry_resources_of_failure failure)
        (cache_failure_error ~path:final_display failure)
        stage
      |> Result.error

let native_update_latest ~root ~latest ~display_path ~id ~root_display =
  match Publication_cache.replace_file root latest ~contents:id with
  | Publication_cache.Replaced { advisories } ->
      Ok
        (publication_success_after_advisories ~root:root_display
           ~path:display_path advisories)
  | Replacement_not_published { live_stage; audit_only; failure } -> (
      let primary =
        add_residual_context
          (cache_failure_error ~path:display_path failure)
          audit_only
      in
      match live_stage with
      | None ->
          Error
            (publication_failure_after_cleanup ~root:root_display primary
               (retry_resources_of_failure failure))
      | Some stage ->
          Error
            (close_stage_after_failure ~root:root_display ~path:display_path
               ~after_stage:(retry_resources_of_failure failure)
               primary stage))

let system_operations =
  {
    stage_pending_report = native_stage_pending;
    publish_report = native_publish_stage;
    update_latest = native_update_latest;
    discard_marker =
      (fun ~root ~path stage ->
        cleanup_native_resources ~root [ Cleanup_marker { stage; path } ]);
    release_lease = cleanup_native_resources;
    publication_release_lease = cleanup_native_resources;
  }

let fault_message fail point =
  try fail point
  with exception_ ->
    Some ("fault injector raised: " ^ Printexc.to_string exception_)

let cleanup_operation_error ~operation ~root message =
  Error.create ~phase:Error.Cleanup ~cause:Error.Io_failure
    ~context:[ ("cache_root", root); ("operation", operation) ]
    "cache root lease %s failed: %s" operation message

let publication_fault_error ~operation ~path message =
  Error.create ~phase:Error.Reporting ~cause:Error.Io_failure
    ~context:[ ("operation", operation); ("path", path); ("fault", message) ]
    "run report %s failed by injection" operation

let faulting_operations fail =
  let release_with_faults ~unlock_point ~close_point ~root_close_point actual
      ~root resources =
    let released = actual ~root resources in
    let injected point operation =
      match fault_message fail point with
      | None -> []
      | Some message -> [ cleanup_operation_error ~operation ~root message ]
    in
    let unlock_errors = injected unlock_point "unlock" in
    let close_errors = injected close_point "close" in
    let root_close_errors = injected root_close_point "root-close" in
    {
      released with
      errors =
        released.errors @ unlock_errors @ close_errors @ root_close_errors;
    }
  in
  {
    stage_pending_report =
      (fun ~root ~pending ~display_path ~root_display ~contents ->
        match fault_message fail Pending_report_write with
        | Some message ->
            Error
              {
                error =
                  publication_fault_error ~operation:"pending-write"
                    ~path:display_path message;
                retained_resources = [];
              }
        | None ->
            system_operations.stage_pending_report ~root ~pending ~display_path
              ~root_display ~contents);
    publish_report =
      (fun ~stage ~pending_display ~final ~final_display ~root_display ->
        match fault_message fail Report_publish with
        | Some message ->
            let primary =
              publication_fault_error ~operation:"atomic-publish"
                ~path:final_display message
            in
            Error
              (close_stage_after_failure ~root:root_display
                 ~path:pending_display primary stage)
        | None ->
            system_operations.publish_report ~stage ~pending_display ~final
              ~final_display ~root_display);
    update_latest =
      (fun ~root ~latest ~display_path ~id ~root_display ->
        match fault_message fail Latest_index_update with
        | Some message ->
            Error
              {
                error =
                  publication_fault_error ~operation:"update-latest-index"
                    ~path:display_path message;
                retained_resources = [];
              }
        | None ->
            system_operations.update_latest ~root ~latest ~display_path ~id
              ~root_display);
    discard_marker =
      (fun ~root ~path stage ->
        let released = system_operations.discard_marker ~root ~path stage in
        match fault_message fail Reservation_marker_remove with
        | None -> released
        | Some message ->
            {
              released with
              errors =
                released.errors
                @ [
                    cleanup_operation_error
                      ~operation:"discard-reservation-marker" ~root message
                    |> Error.with_context "path" path;
                  ];
            });
    release_lease =
      release_with_faults ~unlock_point:Lease_unlock ~close_point:Lease_close
        ~root_close_point:Lease_root_close system_operations.release_lease;
    publication_release_lease =
      release_with_faults ~unlock_point:Publication_lease_unlock
        ~close_point:Publication_lease_close
        ~root_close_point:Publication_lease_root_close
        system_operations.publication_release_lease;
  }

let combine_errors = function
  | [] -> None
  | primary :: suppressed ->
      Some (List.fold_left Error.suppress primary suppressed)

let errors_as_result errors =
  match combine_errors errors with None -> Ok () | Some error -> Error error

let suppress_optional current next =
  match current with
  | None -> Some next
  | Some primary -> Some (Error.suppress primary next)

let reservation_lease shared operations =
  {
    shared;
    operations;
    retained_resources = [];
    retained_error = None;
    released = false;
  }

let identity_changed_root ?(phase = Error.Reporting) store =
  Error.create ~phase ~cause:Error.Workspace_violation
    ~context:[ ("cache_root", store.root) ]
    "cache root identity changed while joining a live root lease"

let retain_shared_residual (shared : shared_root_lease) error residual =
  shared.retained_resources <- residual @ shared.retained_resources;
  shared.retained_error <- suppress_optional shared.retained_error error

let close_join_root (shared : shared_root_lease) ~store ~primary ~before_root
    candidate =
  let cleanup =
    cleanup_native_resources ~root:store.root
      (before_root @ [ Cleanup_root candidate ])
  in
  let error = suppress_all primary cleanup.errors in
  Option.iter (retain_shared_residual shared error) cleanup.residual;
  if cleanup.errors = [] then Error primary else Error error

let join_shared_root (store : t) (shared : shared_root_lease) =
  match shared.retained_error with
  | Some error -> Error error
  | None -> (
      match acquire_native_root ~phase:Error.Reporting store with
      | Error failure ->
          Option.iter
            (retain_shared_residual shared failure.error)
            failure.residual;
          Error failure.error
      | Ok candidate -> (
          let validation =
            match
              verify_cache_ownership ~phase:Error.Reporting store candidate
            with
            | Error (error, retries) -> Error (error, retries)
            | Ok () ->
                if
                  Publication_cache.Identity.equal
                    (Publication_cache.root_identity candidate)
                    (Publication_cache.root_identity shared.native.root)
                then Ok ()
                else Error (identity_changed_root store, [])
          in
          match validation with
          | Error (primary, before_root) ->
              close_join_root shared ~store ~primary ~before_root candidate
          | Ok () -> (
              let cleanup =
                cleanup_native_resources ~root:store.root
                  [ Cleanup_root candidate ]
              in
              match cleanup.errors with
              | [] ->
                  shared.references <- shared.references + 1;
                  Ok (reservation_lease shared store.operations)
              | primary :: suppressed ->
                  let error = suppress_all primary suppressed in
                  Option.iter
                    (retain_shared_residual shared error)
                    cleanup.residual;
                  Error error)))

let poison_acquisition key failure =
  Option.iter
    (fun residual ->
      Hashtbl.replace root_leases key
        (Poisoned { error = failure.error; residual }))
    failure.residual;
  Error failure.error

let acquire_shared_root_lease (store : t) =
  with_lease_registry (fun () ->
      let key = normalize_for_compare store.root in
      match Hashtbl.find_opt root_leases key with
      | Some (Shared shared) -> join_shared_root store shared
      | Some (Maintenance _) -> Error (reservation_busy store.root)
      | Some (Poisoned { error; _ }) -> Error error
      | None -> (
          match
            acquire_native_lock ~phase:Error.Reporting store Cache_fs.Shared
          with
          | Ok (`Acquired native) ->
              let shared =
                {
                  root_key = key;
                  native;
                  references = 1;
                  retained_resources = [];
                  retained_error = None;
                }
              in
              Hashtbl.add root_leases key (Shared shared);
              Ok (reservation_lease shared store.operations)
          | Ok `Busy -> Error (reservation_busy store.root)
          | Error failure -> poison_acquisition key failure))

let record_root_release ~key ~previous_error (released : lease_release) =
  match released.residual with
  | None -> Hashtbl.remove root_leases key
  | Some residual ->
      let error =
        match (combine_errors released.errors, previous_error) with
        | Some error, _ -> error
        | None, Some error -> error
        | None, None ->
            lease_error ~phase:Error.Cleanup ~cause:Error.Invariant_violation
              ~context:[ ("cache_root", key) ]
              "cache root lease retained live cleanup capabilities without an \
               error"
      in
      Hashtbl.replace root_leases key (Poisoned { error; residual })

let transfer_reservation_residual (shared : shared_root_lease)
    (lease : reservation_lease) =
  shared.retained_resources <-
    lease.retained_resources @ shared.retained_resources;
  shared.retained_error <-
    (match lease.retained_error with
    | None -> shared.retained_error
    | Some error -> suppress_optional shared.retained_error error);
  lease.retained_resources <- [];
  lease.retained_error <- None

let release_shared_root_lease_errors (lease : reservation_lease) =
  with_lease_registry (fun () ->
      if lease.released then []
      else
        match Hashtbl.find_opt root_leases lease.shared.root_key with
        | Some (Shared shared) when shared == lease.shared ->
            lease.released <- true;
            transfer_reservation_residual shared lease;
            if shared.references > 1 then (
              shared.references <- shared.references - 1;
              [])
            else
              let previous_error = shared.retained_error in
              let resources =
                shared.retained_resources
                @ [
                    Cleanup_lock shared.native.lock;
                    Cleanup_root shared.native.root;
                  ]
              in
              let released =
                lease.operations.release_lease ~root:shared.root_key resources
              in
              record_root_release ~key:shared.root_key ~previous_error released;
              released.errors
        | Some (Poisoned { error; _ }) ->
            lease.released <- true;
            [ error ]
        | Some (Maintenance _) | Some (Shared _) | None ->
            lease.released <- true;
            [
              lease_error ~phase:Error.Cleanup ~cause:Error.Invariant_violation
                ~context:[ ("cache_root", lease.shared.root_key) ]
                "run reservation root lease registry does not match its \
                 capability";
            ])

let release_shared_root_lease lease =
  release_shared_root_lease_errors lease |> errors_as_result

let transfer_shared_root_lease_to_publication (store : t)
    (lease : reservation_lease) =
  with_lease_registry (fun () ->
      if lease.released then
        Error
          (lease_error ~phase:Error.Reporting ~cause:Error.Invariant_violation
             ~context:[ ("cache_root", lease.shared.root_key) ]
             "cannot transfer an already released reservation lease")
      else
        match Hashtbl.find_opt root_leases lease.shared.root_key with
        | Some (Shared shared) when shared == lease.shared ->
            lease.operations <-
              {
                store.operations with
                release_lease = store.operations.publication_release_lease;
              };
            Ok ()
        | Some (Poisoned { error; _ }) -> Error error
        | Some (Maintenance _) | Some (Shared _) | None ->
            Error
              (lease_error ~phase:Error.Reporting
                 ~cause:Error.Invariant_violation
                 ~context:[ ("cache_root", lease.shared.root_key) ]
                 "run reservation root lease registry does not match the \
                  publication transfer capability"))

let acquire_maintenance_lease (store : t) =
  with_lease_registry (fun () ->
      let key = normalize_for_compare store.root in
      match Hashtbl.find_opt root_leases key with
      | Some (Shared shared) ->
          Error
            (maintenance_busy ~active_reservations:shared.references store.root)
      | Some (Maintenance _) -> Error (maintenance_busy store.root)
      | Some (Poisoned { error; _ }) -> Error error
      | None -> (
          match
            acquire_native_lock ~phase:Error.Cache store Cache_fs.Exclusive
          with
          | Ok (`Acquired native) ->
              let lease =
                {
                  root_key = key;
                  native;
                  operations = store.operations;
                  retained_resources = [];
                  retained_error = None;
                  released = false;
                }
              in
              Hashtbl.add root_leases key (Maintenance lease);
              Ok lease
          | Ok `Busy -> Error (maintenance_busy store.root)
          | Error failure -> poison_acquisition key failure))

let release_maintenance_lease lease =
  with_lease_registry (fun () ->
      if lease.released then Ok ()
      else
        match Hashtbl.find_opt root_leases lease.root_key with
        | Some (Maintenance registered) when registered == lease ->
            lease.released <- true;
            let resources =
              lease.retained_resources
              @ [
                  Cleanup_lock lease.native.lock; Cleanup_root lease.native.root;
                ]
            in
            let released =
              lease.operations.release_lease ~root:lease.root_key resources
            in
            record_root_release ~key:lease.root_key
              ~previous_error:lease.retained_error released;
            errors_as_result released.errors
        | Some (Poisoned { error; _ }) ->
            lease.released <- true;
            Error error
        | Some (Shared _) | Some (Maintenance _) | None ->
            lease.released <- true;
            Error
              (lease_error ~phase:Error.Cleanup ~cause:Error.Invariant_violation
                 ~context:[ ("cache_root", lease.root_key) ]
                 "maintenance root lease registry does not match its capability"))

type 'a leased_action =
  | Leased_returned of ('a, Error.t) result
  | Leased_raised of exn * Printexc.raw_backtrace

let finish_leased_action outcome cleanup =
  match (outcome, cleanup) with
  | Leased_returned (Ok value), Ok () -> Ok value
  | Leased_returned (Error primary), Ok () -> Error primary
  | Leased_returned (Ok _), Error cleanup -> Error cleanup
  | Leased_returned (Error primary), Error cleanup ->
      Error (Error.suppress primary cleanup)
  | Leased_raised (exception_, backtrace), _ ->
      Printexc.raise_with_backtrace exception_ backtrace

let with_maintenance_lease store action =
  let* lease = acquire_maintenance_lease store in
  let outcome =
    try Leased_returned (action ())
    with exception_ ->
      Leased_raised (exception_, Printexc.get_raw_backtrace ())
  in
  finish_leased_action outcome (release_maintenance_lease lease)

let resolve_directory configured =
  match configured with
  | None -> (default_directory (), false)
  | Some path when Filename.is_relative path ->
      (Filename.concat (default_directory ()) path, true)
  | Some path -> (path, false)

let lexical_absolute path =
  let absolute =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
  Fpath.(v absolute |> normalize |> to_string)

(* Ambient store paths may traverse symlinked prefixes — macOS's /tmp and /var
   are symlinks into /private — while the capability walk deliberately refuses
   to follow any link. The ambient boundary therefore canonicalizes here: the
   deepest existing ancestor resolves through the OS, and a missing suffix is
   reattached verbatim for the walk's own missing-leaf handling. *)
let canonical_absolute path =
  let absolute = lexical_absolute path in
  let rec resolve prefix suffix =
    match Unix.realpath prefix with
    | canonical -> List.fold_left Filename.concat canonical suffix
    | exception Unix.Unix_error _ ->
        let parent = Filename.dirname prefix in
        if String.equal parent prefix then absolute
        else resolve parent (Filename.basename prefix :: suffix)
  in
  resolve absolute []

let is_within ~parent child =
  let parent = normalize_for_compare parent in
  let child = normalize_for_compare child in
  String.equal parent child || string_starts_with ~prefix:(parent ^ "/") child

let workspace_scope = function
  | None -> Store_path.root
  | Some workspace ->
      let identity = normalize_for_compare workspace |> sha256 in
      Store_path.child_internal workspace_scopes_path identity

let clean_stage_collision (failure : Publication_cache.operation_failure) =
  match (failure.primary, failure.suppressed) with
  | Cache_fs.Operation_error error, [] ->
      error.operation = Cache_fs.Stage_file
      && error.primitive_operation = Some Dir_cap.Create_file
      && error.class_ = Cache_fs.Already_exists
  | Cleanup_error _, _ | Operation_error _, _ -> false

let bootstrap_failure error retained_resources : publication_failure =
  { error; retained_resources }

let bootstrap_failure_after_cleanup ~root primary resources =
  let released = cleanup_native_resources ~root resources in
  bootstrap_failure
    (suppress_all primary released.errors)
    (residual_resources released)

let close_persistent_marker (store : t) stage =
  match Publication_cache.close_stage stage with
  | Cache_fs.Teardown_complete | Teardown_local_only -> Ok ()
  | Teardown_incomplete { live; failure } ->
      let retained_resources =
        match live with
        | Some stage -> [ Cleanup_stage stage ]
        | None -> retry_resources_of_failure failure
      in
      Error
        (bootstrap_failure
           (native_lease_cache_failure ~root:store.root ~phase:Error.Cache
              failure)
           retained_resources)

let initialize_cache_ownership (store : t) root =
  let display_path = store_path store marker_path in
  match
    Publication_cache.stage_file root marker_path ~contents:marker_contents
  with
  | Publication_cache.Staged stage -> close_persistent_marker store stage
  | Staging_not_created failure when clean_stage_collision failure -> (
      match inspect_cache_ownership ~phase:Error.Cache store root with
      | Error (error, retained_resources) ->
          Error (bootstrap_failure error retained_resources)
      | Ok Ownership_verified -> Ok ()
      | Ok Ownership_missing ->
          Error
            (bootstrap_failure
               (ownership_marker_error ~phase:Error.Cache store
                  "marker is missing")
               [])
      | Ok Ownership_other ->
          Error
            (bootstrap_failure
               (ownership_marker_error ~phase:Error.Cache store
                  "marker contents differ")
               []))
  | Staging_not_created failure ->
      Error
        (bootstrap_failure
           (cache_failure_error ~path:display_path failure)
           (retry_resources_of_failure failure))
  | Staging_incomplete_actionable { live_stage; failure } ->
      Error
        (bootstrap_failure_after_cleanup ~root:store.root
           (cache_failure_error ~path:display_path failure)
           [ Cleanup_marker { stage = live_stage; path = display_path } ])
  | Staging_incomplete_audit_only { residual; failure } ->
      Error
        (bootstrap_failure
           (add_residual_context
              (cache_failure_error ~path:display_path failure)
              [ residual ])
           (retry_resources_of_failure failure))

let empty_root_budget () =
  match
    Cache_fs.Traversal_budget.create ~max_depth:0L ~max_entries:1L
      ~max_native_name_bytes:Int64.max_int
  with
  | Ok budget -> budget
  | Error _ -> assert false

let close_bootstrap_listing (store : t) listing primary =
  let released =
    cleanup_native_resources ~root:store.root [ Cleanup_listing listing ]
  in
  match (primary, released.errors) with
  | None, [] -> Ok ()
  | Some primary, errors ->
      Error
        (bootstrap_failure
           (suppress_all primary errors)
           (residual_resources released))
  | None, primary :: suppressed ->
      Error
        (bootstrap_failure
           (suppress_all primary suppressed)
           (residual_resources released))

let require_empty_unowned_root (store : t) root =
  match
    Publication_cache.list root Store_path.root ~budget:(empty_root_budget ())
  with
  | Publication_cache.Listing_incomplete { failure; _ } ->
      Error
        (bootstrap_failure
           (native_lease_cache_failure ~root:store.root ~phase:Error.Cache
              failure)
           (retry_resources_of_failure failure))
  | Listing_complete { listing; _ } -> (
      match Publication_cache.listed_entries listing with
      | Error failure ->
          close_bootstrap_listing store listing
            (Some
               (native_lease_cache_failure ~root:store.root ~phase:Error.Cache
                  failure))
      | Ok [] -> close_bootstrap_listing store listing None
      | Ok entries -> (
          let primary =
            Error.create ~phase:Error.Cache ~cause:Error.Workspace_violation
              ~context:
                [
                  ("cache_root", store.root);
                  ("observed_entries", string_of_int (List.length entries));
                ]
              "refusing an unowned non-empty cache directory"
          in
          let* () = close_bootstrap_listing store listing None in
          match inspect_cache_ownership ~phase:Error.Cache store root with
          | Ok Ownership_verified -> Ok ()
          | Ok _ -> Error (bootstrap_failure primary [])
          | Error (error, retained_resources) ->
              Error
                (bootstrap_failure
                   (Error.suppress primary error)
                   retained_resources)))

let validate_created_root (store : t) created =
  match created with
  | [] -> Ok `Existing
  | [ Publication_cache.Creation_observed _ ] -> Ok `Fresh
  | _ ->
      Error
        (bootstrap_failure
           (add_acquisition_context
              (Error.create ~phase:Error.Cache ~cause:Error.Invariant_violation
                 ~context:[ ("cache_root", store.root) ]
                 "cache root materialization returned ambiguous creation \
                  evidence")
              created)
           [])

let establish_cache_ownership (store : t)
    (acquisition : Publication_cache.acquisition) =
  let* disposition = validate_created_root store acquisition.created in
  match disposition with
  | `Fresh -> initialize_cache_ownership store acquisition.root
  | `Existing -> (
      match
        inspect_cache_ownership ~phase:Error.Cache store acquisition.root
      with
      | Error (error, retained_resources) ->
          Error (bootstrap_failure error retained_resources)
      | Ok Ownership_verified -> Ok ()
      | Ok Ownership_other ->
          Error
            (bootstrap_failure
               (ownership_marker_error ~phase:Error.Cache store
                  "marker contents differ")
               [])
      | Ok Ownership_missing ->
          let* () = require_empty_unowned_root store acquisition.root in
          initialize_cache_ownership store acquisition.root)

let bootstrap_registry_error (store : t)
    (acquisition : Publication_cache.acquisition) =
  let key = normalize_for_compare store.root in
  match Hashtbl.find_opt root_leases key with
  | None -> Ok ()
  | Some (Shared shared) ->
      if acquisition.created <> [] then
        Error
          (bootstrap_failure
             (native_lock_recreated_root ~phase:Error.Cache store.root
                acquisition.created)
             [])
      else if
        Publication_cache.Identity.equal
          (Publication_cache.root_identity acquisition.root)
          (Publication_cache.root_identity shared.native.root)
      then Ok ()
      else
        Error
          (bootstrap_failure
             (identity_changed_root ~phase:Error.Cache store)
             [])
  | Some (Maintenance _) ->
      Error (bootstrap_failure (maintenance_busy store.root) [])
  | Some (Poisoned { error; _ }) -> Error (bootstrap_failure error [])

let retain_bootstrap_residual (store : t) error residual =
  let actual_key = normalize_for_compare store.root in
  match Hashtbl.find_opt root_leases actual_key with
  | Some (Shared shared) -> retain_shared_residual shared error residual
  | Some (Poisoned poisoned) ->
      Hashtbl.replace root_leases actual_key
        (Poisoned
           {
             error = Error.suppress poisoned.error error;
             residual = residual @ poisoned.residual;
           })
  | None -> Hashtbl.add root_leases actual_key (Poisoned { error; residual })
  | Some (Maintenance lease) ->
      lease.retained_resources <- residual @ lease.retained_resources;
      lease.retained_error <- suppress_optional lease.retained_error error

let retain_bootstrap_residual_at_key key error residual =
  match Hashtbl.find_opt root_leases key with
  | Some (Shared shared) -> retain_shared_residual shared error residual
  | Some (Poisoned poisoned) ->
      Hashtbl.replace root_leases key
        (Poisoned
           {
             error = Error.suppress poisoned.error error;
             residual = residual @ poisoned.residual;
           })
  | None -> Hashtbl.add root_leases key (Poisoned { error; residual })
  | Some (Maintenance lease) ->
      lease.retained_resources <- residual @ lease.retained_resources;
      lease.retained_error <- suppress_optional lease.retained_error error

let finish_cache_bootstrap (store : t)
    (acquisition : Publication_cache.acquisition)
    (outcome : (unit, publication_failure) result) =
  let primary, before_root =
    match outcome with
    | Ok () -> (None, [])
    | Error failure -> (Some failure.error, failure.retained_resources)
  in
  let released =
    cleanup_native_resources ~root:store.root
      (before_root @ [ Cleanup_root acquisition.root ])
  in
  let final_error =
    match (primary, released.errors) with
    | None, [] -> None
    | Some primary, errors -> Some (suppress_all primary errors)
    | None, primary :: suppressed -> Some (suppress_all primary suppressed)
  in
  Option.iter
    (fun residual ->
      let error =
        Option.value final_error
          ~default:
            (lease_error ~phase:Error.Cleanup ~cause:Error.Invariant_violation
               ~context:[ ("cache_root", store.root) ]
               "cache bootstrap retained live cleanup authority without an \
                error")
      in
      retain_bootstrap_residual store error residual)
    released.residual;
  match final_error with None -> Ok store | Some error -> Error error

let create_with_operations
    ?(next_reservation_sequence = next_process_reservation_sequence) operations
    ?workspace ?directory:configured () =
  let unresolved, relative = resolve_directory configured in
  let requested = canonical_absolute unresolved in
  let workspace = Option.map canonical_absolute workspace in
  let lock_boundary =
    Option.value workspace ~default:(canonical_absolute (Sys.getcwd ()))
  in
  let expected_cache_root = canonical_absolute (default_directory ()) in
  let escaped_cache_root =
    relative && not (is_within ~parent:expected_cache_root requested)
  in
  if escaped_cache_root then
    Error
      (Error.create ~phase:Error.Cache ~cause:Error.Workspace_violation
         "relative cache directory escaped the OS cache root: %s" requested)
  else
    with_lease_registry (fun () ->
        let requested_key = normalize_for_compare requested in
        match Hashtbl.find_opt root_leases requested_key with
        | Some (Maintenance _) -> Error (maintenance_busy requested)
        | Some (Poisoned { error; _ }) -> Error error
        | None | Some (Shared _) -> (
            let provisional =
              {
                root = requested;
                workspace;
                lock_boundary;
                topology = topology (workspace_scope workspace);
                operations;
                next_reservation_sequence;
              }
            in
            match
              acquire_native_root_candidate ~phase:Error.Cache provisional
            with
            | Error failure ->
                Option.iter
                  (fun residual ->
                    retain_bootstrap_residual_at_key requested_key failure.error
                      residual)
                  failure.residual;
                Error failure.error
            | Ok acquisition ->
                let workspace =
                  Option.map
                    (fun _ -> acquisition.workspace_display_path)
                    workspace
                in
                let store =
                  {
                    root = acquisition.display_path;
                    workspace;
                    lock_boundary = acquisition.workspace_display_path;
                    topology = topology (workspace_scope workspace);
                    operations;
                    next_reservation_sequence;
                  }
                in
                let outcome =
                  let* () = bootstrap_registry_error store acquisition in
                  establish_cache_ownership store acquisition
                in
                finish_cache_bootstrap store acquisition outcome))

let create ?workspace ?directory () =
  create_with_operations system_operations ?workspace ?directory ()

module For_testing = struct
  let create ?workspace ?directory ?next_reservation_sequence ~fail () =
    create_with_operations ?next_reservation_sequence (faulting_operations fail)
      ?workspace ?directory ()
end

let directory (store : t) = store.root

let run_key inputs =
  let framed =
    inputs
    |> List.map (fun input ->
        Printf.sprintf "%d:%s" (String.length input) input)
    |> String.concat ""
  in
  sha256 ("ocaml-mutants-cache-key-v3:" ^ framed)

let range_to_json range =
  `Assoc
    [
      ("start_byte", `Int (Core.Source_range.start_byte range));
      ("end_byte", `Int (Core.Source_range.end_byte range));
      ("start_line", `Int (Core.Source_range.start_line range));
      ("start_column", `Int (Core.Source_range.start_column range));
      ("end_line", `Int (Core.Source_range.end_line range));
      ("end_column", `Int (Core.Source_range.end_column range));
    ]

let range_of_json json =
  let open Yojson.Safe.Util in
  Core.Source_range.make
    ~start_byte:(json |> member "start_byte" |> to_int)
    ~end_byte:(json |> member "end_byte" |> to_int)
    ~start_line:(json |> member "start_line" |> to_int)
    ~start_column:(json |> member "start_column" |> to_int)
    ~end_line:(json |> member "end_line" |> to_int)
    ~end_column:(json |> member "end_column" |> to_int)

let mutant_to_json mutant =
  `Assoc
    [
      ("id", `String (Core.Mutant.Id.short (Core.Mutant.id mutant)));
      ("full_id", `String (Core.Mutant.Id.full (Core.Mutant.id mutant)));
      ("lineage_id", `String (Core.Mutant.lineage_id mutant));
      ("path", `String (Core.Mutant.path mutant));
      ("range", range_to_json (Core.Mutant.range mutant));
      ("family", `String (Core.Operator.Family.name (Core.Mutant.family mutant)));
      ( "rule",
        `String (Core.Operator.Rule.stable_name (Core.Mutant.rule mutant)) );
      ("original", `String (Core.Mutant.original mutant));
      ("replacement", `String (Core.Mutant.replacement mutant));
      ("source_digest", `String (Core.Mutant.source_digest mutant));
    ]

let mutant_of_json json =
  let open Yojson.Safe.Util in
  let* range = range_of_json (member "range" json) in
  let* rule =
    Core.Operator.Rule.of_stable_name (json |> member "rule" |> to_string)
  in
  let* mutant =
    Core.Mutant.restore
      ~path:(json |> member "path" |> to_string)
      ~range ~rule
      ~original:(json |> member "original" |> to_string)
      ~replacement:(json |> member "replacement" |> to_string)
      ~source_digest:(json |> member "source_digest" |> to_string)
      ~full_id:(json |> member "full_id" |> to_string)
    |> Result.map_error (Format.asprintf "%a" Core.Mutant.pp_validation_error)
  in
  let encoded_short_id = json |> member "id" |> to_string in
  let actual_short_id = Core.Mutant.Id.short (Core.Mutant.id mutant) in
  let* () =
    if String.equal encoded_short_id actual_short_id then Ok ()
    else Error "short mutant ID contradicts full mutant identity"
  in
  let encoded_family = json |> member "family" |> to_string in
  let actual_family = Core.Operator.Family.name (Core.Mutant.family mutant) in
  let* () =
    if String.equal encoded_family actual_family then Ok ()
    else Error "operator family contradicts the mutant rule"
  in
  let* () =
    match json with
    | `Assoc fields -> (
        match List.assoc_opt "lineage_id" fields with
        | None -> Ok ()
        | Some (`String encoded)
          when String.equal encoded (Core.Mutant.lineage_id mutant) ->
            Ok ()
        | Some (`String _) -> Error "lineage ID contradicts mutant structure"
        | Some _ -> Error "lineage ID must be a string")
    | _ -> Error "mutant must be an object"
  in
  Ok mutant

let captured_to_json captured =
  `Assoc
    [
      ("contents", `String captured.contents);
      ("truncated", `Bool captured.truncated);
      ("total_bytes", `Int captured.total_bytes);
    ]

let captured_of_json json =
  let open Yojson.Safe.Util in
  let contents = json |> member "contents" |> to_string in
  let truncated = json |> member "truncated" |> to_bool in
  let total_bytes = json |> member "total_bytes" |> to_int in
  let retained_bytes = String.length contents in
  if total_bytes < retained_bytes then
    Error "captured total byte count is smaller than retained output"
  else if (not truncated) && total_bytes <> retained_bytes then
    Error "untruncated output byte count contradicts retained output"
  else Ok { contents; truncated; total_bytes }

let stage_result_to_json (stage : stage_result) =
  `Assoc
    [
      ("name", `String stage.name);
      ("status", `String stage.status);
      ("duration_seconds", `Float (Core.Duration.to_seconds stage.duration));
    ]

let stage_result_of_json json =
  let open Yojson.Safe.Util in
  let* duration =
    Core.Duration.of_seconds (json |> member "duration_seconds" |> to_float)
  in
  Ok
    {
      name = json |> member "name" |> to_string;
      status = json |> member "status" |> to_string;
      duration;
    }

let outcome_detail = function
  | Core.Outcome.Error message | Core.Outcome.Inconclusive message ->
      `String message
  | Core.Outcome.Killed | Core.Outcome.Survived | Core.Outcome.Timeout -> `Null

let outcome_of_json json =
  let open Yojson.Safe.Util in
  let* outcome =
    Core.Outcome.of_string (json |> member "outcome" |> to_string)
  in
  match (outcome, member "error" json) with
  | Core.Outcome.Error _, `String value -> Ok (Core.Outcome.Error value)
  | Core.Outcome.Inconclusive _, `String value ->
      Ok (Core.Outcome.Inconclusive value)
  | (Core.Outcome.Killed | Core.Outcome.Survived | Core.Outcome.Timeout), `Null
    ->
      Ok outcome
  | (Core.Outcome.Error _ | Core.Outcome.Inconclusive _), _ ->
      Error "error and inconclusive outcomes require a string detail"
  | (Core.Outcome.Killed | Core.Outcome.Survived | Core.Outcome.Timeout), _ ->
      Error "non-error outcomes must not carry an error detail"

let retry_attempt_to_json (attempt : retry_attempt) =
  `Assoc
    [
      ("outcome", `String (Core.Outcome.name attempt.outcome));
      ("error", outcome_detail attempt.outcome);
      ("duration_seconds", `Float (Core.Duration.to_seconds attempt.duration));
      ("stages", `List (List.map stage_result_to_json attempt.stages));
      ("stdout", captured_to_json attempt.stdout);
      ("stderr", captured_to_json attempt.stderr);
    ]

let retry_attempt_of_json json =
  let open Yojson.Safe.Util in
  let* outcome = outcome_of_json json in
  let* duration =
    Core.Duration.of_seconds (json |> member "duration_seconds" |> to_float)
  in
  let* stages =
    let rec decode decoded = function
      | [] -> Ok (List.rev decoded)
      | value :: rest ->
          let* stage = stage_result_of_json value in
          decode (stage :: decoded) rest
    in
    decode [] (json |> member "stages" |> to_list)
  in
  let* stdout = captured_of_json (member "stdout" json) in
  let* stderr = captured_of_json (member "stderr" json) in
  Ok { outcome; duration; stages; stdout; stderr }

let expectation_status_of_result result =
  match result.outcome with
  | Core.Outcome.Survived -> Expectation_fulfilled
  | Core.Outcome.Killed -> Expectation_unfulfilled_killed
  | Core.Outcome.Timeout when result.timeout_confirmed ->
      Expectation_unfulfilled_confirmed_timeout
  | Core.Outcome.Timeout ->
      Expectation_inconclusive "timeout was not confirmed by a serial retry"
  | Core.Outcome.Inconclusive message -> Expectation_inconclusive message
  | Core.Outcome.Error message -> Expectation_error message

let expectation_status_name = function
  | Expectation_fulfilled -> "fulfilled"
  | Expectation_unfulfilled_killed -> "unfulfilled-killed"
  | Expectation_unfulfilled_confirmed_timeout -> "unfulfilled-confirmed-timeout"
  | Expectation_inconclusive _ -> "inconclusive"
  | Expectation_error _ -> "error"
  | Expectation_stale -> "stale"
  | Expectation_not_evaluated -> "not-evaluated"

let expectation_status_is_failure = function
  | Expectation_fulfilled | Expectation_not_evaluated -> false
  | Expectation_unfulfilled_killed | Expectation_unfulfilled_confirmed_timeout
  | Expectation_inconclusive _ | Expectation_error _ | Expectation_stale ->
      true

let expectation_status_detail = function
  | Expectation_inconclusive message | Expectation_error message ->
      `String message
  | Expectation_fulfilled | Expectation_unfulfilled_killed
  | Expectation_unfulfilled_confirmed_timeout | Expectation_stale
  | Expectation_not_evaluated ->
      `Null

let expectation_status_of_json json =
  let open Yojson.Safe.Util in
  let detail () =
    match json |> member "detail" with
    | `String value -> Ok value
    | _ -> Error "expectation status requires a string detail"
  in
  let no_detail status =
    match json |> member "detail" with
    | `Null -> Ok status
    | _ -> Error "expectation status does not accept a detail"
  in
  match json |> member "status" |> to_string with
  | "fulfilled" -> no_detail Expectation_fulfilled
  | "unfulfilled-killed" -> no_detail Expectation_unfulfilled_killed
  | "unfulfilled-confirmed-timeout" ->
      no_detail Expectation_unfulfilled_confirmed_timeout
  | "inconclusive" ->
      Result.map (fun value -> Expectation_inconclusive value) (detail ())
  | "error" -> Result.map (fun value -> Expectation_error value) (detail ())
  | "stale" -> no_detail Expectation_stale
  | "not-evaluated" -> no_detail Expectation_not_evaluated
  | value -> Error (Printf.sprintf "unknown expectation status %S" value)

let expectation_evaluation_to_json evaluation =
  `Assoc
    [
      ("mutant_id", `String evaluation.mutant_id);
      ("reason", `String evaluation.reason);
      ("status", `String (expectation_status_name evaluation.status));
      ("detail", expectation_status_detail evaluation.status);
    ]

let expectation_evaluation_of_json json =
  let open Yojson.Safe.Util in
  let* status = expectation_status_of_json json in
  Ok
    {
      mutant_id = json |> member "mutant_id" |> to_string;
      reason = json |> member "reason" |> to_string;
      status;
    }

type evidence_level = Executed | Exact_cache | Estimated

let result_evidence_level result =
  match result.evidence_origin with
  | Execution -> Executed
  | Checkpoint_resume | Historical_exact -> Exact_cache
  | Checkpoint_estimated | Historical_estimated | Fast_estimated -> Estimated

let evidence_origin_name = function
  | Execution -> "execution"
  | Checkpoint_resume -> "checkpoint-resume"
  | Checkpoint_estimated -> "checkpoint-estimated"
  | Historical_exact -> "historical-exact"
  | Historical_estimated -> "historical-estimated"
  | Fast_estimated -> "fast-estimated"

let evidence_origin_is_cached = function
  | Checkpoint_resume | Checkpoint_estimated | Historical_exact
  | Historical_estimated ->
      true
  | Execution | Fast_estimated -> false

let evidence_origin_is_estimated = function
  | Checkpoint_estimated | Historical_estimated | Fast_estimated -> true
  | Execution | Checkpoint_resume | Historical_exact -> false

let evidence_origin_is_resumed = function
  | Checkpoint_resume | Checkpoint_estimated -> true
  | Execution | Historical_exact | Historical_estimated | Fast_estimated ->
      false

let evidence_level_name = function
  | Executed -> "executed"
  | Exact_cache -> "exact-cache"
  | Estimated -> "estimated"

let result_attempts result =
  match result.timeout_retry with
  | Some retry -> [ retry.initial_timeout; retry.serial_retry ]
  | None ->
      [
        {
          outcome = result.outcome;
          duration = result.duration;
          stages = result.stages;
          stdout = result.stdout;
          stderr = result.stderr;
        };
      ]

let killing_stage result =
  if result.outcome <> Core.Outcome.Killed then None
  else
    match List.rev result.stages with
    | [] -> None
    | stage :: _ -> Some stage.name

let result_to_json ?coverage result =
  let coverage =
    Option.value coverage
      ~default:
        (match result.outcome with
        | Core.Outcome.Killed -> "covered"
        | Core.Outcome.Survived | Core.Outcome.Timeout
        | Core.Outcome.Inconclusive _ | Core.Outcome.Error _ ->
            "unknown")
  in
  `Assoc
    [
      ("mutant", mutant_to_json result.mutant);
      ("outcome", `String (Core.Outcome.name result.outcome));
      ("error", outcome_detail result.outcome);
      ("duration_seconds", `Float (Core.Duration.to_seconds result.duration));
      ("cached", `Bool result.cached);
      ( "evidence",
        `Assoc
          [
            ( "level",
              `String (evidence_level_name (result_evidence_level result)) );
            ("origin", `String (evidence_origin_name result.evidence_origin));
            ( "estimated",
              `Bool (evidence_origin_is_estimated result.evidence_origin) );
          ] );
      ( "attempts",
        `List (List.map retry_attempt_to_json (result_attempts result)) );
      ( "killing_test",
        match killing_stage result with
        | None -> `Null
        | Some name -> `String name );
      ("coverage", `String coverage);
      ( "checkpoint",
        `Assoc
          [
            ("settled", `Bool true);
            ( "resumed",
              `Bool (evidence_origin_is_resumed result.evidence_origin) );
          ] );
      ("stages", `List (List.map stage_result_to_json result.stages));
      ("timeout_confirmed", `Bool result.timeout_confirmed);
      ( "timeout_retry",
        match result.timeout_retry with
        | None -> `Null
        | Some retry ->
            `Assoc
              [
                ("initial_timeout", retry_attempt_to_json retry.initial_timeout);
                ("serial_retry", retry_attempt_to_json retry.serial_retry);
              ] );
      ( "expected_survivor",
        `Bool
          (result.expected_reason <> None
          && result.outcome = Core.Outcome.Survived) );
      ( "expectation",
        match result.expected_reason with
        | None -> `Null
        | Some reason ->
            `Assoc
              [
                ("reason", `String reason);
                ( "status",
                  `String
                    (expectation_status_name
                       (expectation_status_of_result result)) );
                ( "detail",
                  expectation_status_detail
                    (expectation_status_of_result result) );
              ] );
      ("stdout", captured_to_json result.stdout);
      ("stderr", captured_to_json result.stderr);
    ]

let result_of_json json =
  let open Yojson.Safe.Util in
  let* mutant = mutant_of_json (member "mutant" json) in
  let* outcome = outcome_of_json json in
  let* duration =
    Core.Duration.of_seconds (json |> member "duration_seconds" |> to_float)
  in
  let* stages =
    let rec decode decoded = function
      | [] -> Ok (List.rev decoded)
      | value :: rest ->
          let* stage = stage_result_of_json value in
          decode (stage :: decoded) rest
    in
    decode [] (json |> member "stages" |> to_list)
  in
  let* encoded_expectation =
    match member "expectation" json with
    | `Null -> Ok None
    | value ->
        let* status = expectation_status_of_json value in
        let reason = value |> member "reason" |> to_string in
        if String.trim reason = "" then Error "expectation reason is empty"
        else Ok (Some (reason, status))
  in
  let expected_reason = Option.map fst encoded_expectation in
  let* timeout_retry =
    match member "timeout_retry" json with
    | `Null -> Ok None
    | value ->
        let* initial_timeout =
          retry_attempt_of_json (member "initial_timeout" value)
        in
        let* serial_retry =
          retry_attempt_of_json (member "serial_retry" value)
        in
        Ok (Some { initial_timeout; serial_retry })
  in
  let timeout_confirmed = json |> member "timeout_confirmed" |> to_bool in
  let* () =
    match timeout_retry with
    | None when timeout_confirmed ->
        Error "confirmed timeout is missing two timeout attempts"
    | None -> Ok ()
    | Some retry when retry.initial_timeout.outcome <> Core.Outcome.Timeout ->
        Error "timeout retry evidence does not begin with a timeout"
    | Some retry ->
        let serial = retry.serial_retry.outcome in
        let outcome_matches_serial =
          match serial with
          | Core.Outcome.Error message ->
              outcome = Core.Outcome.Error message
              || outcome
                 = Core.Outcome.Inconclusive
                     ("timeout confirmation failed: " ^ message)
          | serial -> outcome = serial
        in
        let evidence_is_consistent =
          match (serial, timeout_confirmed, outcome) with
          | Core.Outcome.Timeout, true, Core.Outcome.Timeout -> true
          | Core.Outcome.Timeout, _, _ -> false
          | _, false, _ -> outcome_matches_serial
          | _, true, _ -> false
        in
        if not evidence_is_consistent then
          Error "serial retry outcome contradicts the final mutant outcome"
        else
          let measured =
            Core.Duration.add retry.initial_timeout.duration
              retry.serial_retry.duration
          in
          if Core.Duration.compare measured duration = 0 then Ok ()
          else Error "mutant duration contradicts timeout retry evidence"
  in
  let* stdout = captured_of_json (member "stdout" json) in
  let* stderr = captured_of_json (member "stderr" json) in
  let cached = json |> member "cached" |> to_bool in
  let evidence_json = member "evidence" json in
  let* evidence_origin =
    match evidence_json with
    | `Null -> Ok (if cached then Historical_exact else Execution)
    | evidence -> (
        match evidence |> member "origin" |> to_string with
        | "execution" -> Ok Execution
        | "checkpoint-resume" -> Ok Checkpoint_resume
        | "checkpoint-estimated" -> Ok Checkpoint_estimated
        | "historical-exact" | "historical-cache" -> Ok Historical_exact
        | "historical-estimated" -> Ok Historical_estimated
        | "fast-estimated" -> Ok Fast_estimated
        | value -> Error ("unknown evidence origin: " ^ value))
  in
  let* () =
    if cached = evidence_origin_is_cached evidence_origin then Ok ()
    else Error "cached flag contradicts evidence origin"
  in
  let* () =
    if evidence_origin = Fast_estimated && outcome = Core.Outcome.Killed then
      Error "an observed fast-mode kill must retain executed evidence"
    else Ok ()
  in
  let result =
    {
      mutant;
      outcome;
      duration;
      cached;
      evidence_origin;
      stages;
      timeout_confirmed;
      timeout_retry;
      expected_reason;
      stdout;
      stderr;
    }
  in
  let* () =
    match evidence_json with
    | `Null -> Ok ()
    | evidence ->
        let encoded_level = evidence |> member "level" |> to_string in
        let encoded_estimated = evidence |> member "estimated" |> to_bool in
        let expected_level =
          evidence_level_name (result_evidence_level result)
        in
        let expected_estimated = evidence_origin_is_estimated evidence_origin in
        if not (String.equal encoded_level expected_level) then
          Error "evidence level contradicts evidence origin"
        else if encoded_estimated <> expected_estimated then
          Error "estimated marker contradicts evidence origin"
        else Ok ()
  in
  let* () =
    match member "checkpoint" json with
    | `Null -> Ok ()
    | checkpoint ->
        if not (checkpoint |> member "settled" |> to_bool) then
          Error "settled result has an unset checkpoint marker"
        else if
          checkpoint |> member "resumed" |> to_bool
          <> evidence_origin_is_resumed evidence_origin
        then Error "checkpoint resume marker contradicts evidence origin"
        else Ok ()
  in
  let encoded_expected_survivor =
    json |> member "expected_survivor" |> to_bool
  in
  let expected_survivor =
    result.expected_reason <> None && result.outcome = Core.Outcome.Survived
  in
  let* () =
    if encoded_expected_survivor = expected_survivor then Ok ()
    else Error "expected_survivor contradicts expectation and outcome"
  in
  let* () =
    match encoded_expectation with
    | None -> Ok ()
    | Some (_, encoded_status) ->
        let actual_status = expectation_status_of_result result in
        if encoded_status = actual_status then Ok ()
        else
          Error
            "encoded expectation status/detail contradicts the mutant result"
  in
  Ok result

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

let run_evidence_level (run : run) =
  if
    List.exists
      (fun result -> result_evidence_level result = Estimated)
      run.results
  then Estimated
  else if
    List.exists
      (fun result -> result_evidence_level result = Exact_cache)
      run.results
  then Exact_cache
  else Executed

let summary (run : run) =
  let not_run = not_run run in
  let count_results predicate =
    List.fold_left
      (fun count result -> count + if predicate result then 1 else 0)
      0 run.results
  in
  let count_outcome predicate =
    count_results (fun result -> predicate result.outcome)
  in
  let killed = count_outcome (fun outcome -> outcome = Core.Outcome.Killed) in
  let survived =
    count_outcome (fun outcome -> outcome = Core.Outcome.Survived)
  in
  let timeout = count_outcome (fun outcome -> outcome = Core.Outcome.Timeout) in
  let unconfirmed_timeouts =
    count_results (fun result ->
        result.outcome = Core.Outcome.Timeout && not result.timeout_confirmed)
  in
  let inconclusive =
    count_outcome (function Core.Outcome.Inconclusive _ -> true | _ -> false)
  in
  let error =
    count_outcome (function Core.Outcome.Error _ -> true | _ -> false)
  in
  let expected_survivors =
    List.fold_left
      (fun count (evaluation : expectation_evaluation) ->
        count
        +
        match evaluation.status with
        | Expectation_fulfilled -> 1
        | Expectation_unfulfilled_killed
        | Expectation_unfulfilled_confirmed_timeout | Expectation_inconclusive _
        | Expectation_error _ | Expectation_stale | Expectation_not_evaluated ->
            0)
      0 run.expectations
  in
  let unexpected_survivors =
    count_results (fun result ->
        result.outcome = Core.Outcome.Survived && result.expected_reason = None)
  in
  let unfulfilled_expectations =
    List.fold_left
      (fun count (evaluation : expectation_evaluation) ->
        count
        +
        match evaluation.status with
        | Expectation_unfulfilled_killed
        | Expectation_unfulfilled_confirmed_timeout ->
            1
        | Expectation_fulfilled | Expectation_inconclusive _
        | Expectation_error _ | Expectation_stale | Expectation_not_evaluated ->
            0)
      0 run.expectations
  in
  (* Detected mutants are kills plus confirmed timeouts; the score denominator
     adds only unexpected survivors. Unconfirmed timeouts exist only in
     interrupted runs, where the confirmation retry was cancelled, and carry no
     detection signal. Expected survivors, inconclusive results, errors, and
     not-run mutants stay out as well; the last three already surface through
     exit code 2. *)
  let detected = killed + timeout - unconfirmed_timeouts in
  let scoreable = detected + unexpected_survivors in
  let score =
    if scoreable = 0 then None
    else Some (100. *. float_of_int detected /. float_of_int scoreable)
  in
  {
    kind =
      (match run.completeness with
      | Complete -> "complete"
      | Partial _ -> "partial");
    total = List.length run.results + List.length not_run;
    executed = List.length run.results;
    not_run = List.length not_run;
    killed;
    survived;
    timeout;
    unconfirmed_timeouts;
    inconclusive;
    error;
    expected_survivors;
    unexpected_survivors;
    unfulfilled_expectations;
    detected;
    score;
  }

let summary_json run =
  let summary = summary run in
  `Assoc
    [
      ("kind", `String summary.kind);
      ("total", `Int summary.total);
      ("executed", `Int summary.executed);
      ("not_run", `Int summary.not_run);
      ("killed", `Int summary.killed);
      ("survived", `Int summary.survived);
      ("timeout", `Int summary.timeout);
      ("unconfirmed_timeouts", `Int summary.unconfirmed_timeouts);
      ("inconclusive", `Int summary.inconclusive);
      ("error", `Int summary.error);
      ("expected_survivors", `Int summary.expected_survivors);
      ("unexpected_survivors", `Int summary.unexpected_survivors);
      ("unfulfilled_expectations", `Int summary.unfulfilled_expectations);
      ("detected", `Int summary.detected);
      ( "score",
        match summary.score with None -> `Null | Some score -> `Float score );
    ]

let failure_to_json error =
  let rec encode error =
    `Assoc
      [
        ("phase", `String (Error.phase_name (Error.phase error)));
        ("cause", `String (Error.cause_name (Error.cause error)));
        ("code", `String (Error.code error));
        ("message", `String (Error.message error));
        ("remediation", `String (Error.remediation error));
        ( "context",
          `Assoc
            (List.map
               (fun (key, value) -> (key, `String value))
               (Error.context error)) );
        ("suppressed", `List (List.map encode (Error.suppressed error)));
      ]
  in
  encode error

let status_name = function
  | Completed -> "completed"
  | Interrupted -> "interrupted"
  | Failed _ -> "failed"

let result_coverage_from_hit_map hit_map (result : mutant_result) =
  if result.outcome = Core.Outcome.Killed then "covered"
  else
    let id = Core.Mutant.Id.full (Core.Mutant.id result.mutant) in
    if
      List.exists
        (fun (entry : hit_map_entry) -> List.mem id entry.mutant_ids)
        hit_map
    then "covered"
    else if hit_map <> [] then "not-covered"
    else "unknown"

let result_coverage (run : run) result =
  result_coverage_from_hit_map run.metadata.hit_map result

let run_to_yojson (run : run) =
  let not_run = not_run run in
  let duration_option_to_json = function
    | None -> `Null
    | Some duration -> `Float (Core.Duration.to_seconds duration)
  in
  let baseline_stage_to_json stage =
    `Assoc
      [
        ("name", `String stage.name);
        ( "command",
          `List
            (List.map
               (fun value -> `String value)
               (Core.Nonempty_argv.to_list stage.command)) );
        ( "baseline_runs_seconds",
          `List
            (List.map
               (fun duration -> `Float (Core.Duration.to_seconds duration))
               stage.runs) );
        ( "slowest_baseline_seconds",
          `Float (Core.Duration.to_seconds stage.slowest) );
      ]
  in
  let evidence_counts =
    List.fold_left
      (fun (executed, exact_cache, estimated) result ->
        match result_evidence_level result with
        | Executed -> (executed + 1, exact_cache, estimated)
        | Exact_cache -> (executed, exact_cache + 1, estimated)
        | Estimated -> (executed, exact_cache, estimated + 1))
      (0, 0, 0) run.results
  in
  let executed_evidence, exact_cache_evidence, estimated_evidence =
    evidence_counts
  in
  let resumed_evidence =
    List.fold_left
      (fun count result ->
        if evidence_origin_is_resumed result.evidence_origin then count + 1
        else count)
      0 run.results
  in
  let stages_json =
    `List (List.map baseline_stage_to_json run.metadata.baseline_stages)
  in
  let resolved_config = run.metadata.resolved_config in
  `Assoc
    [
      ("document_type", `String "ocaml-mutants.run-report-v2");
      ("schema_version", `Int 2);
      ("run_id", `String (Core.Run_id.to_string run.metadata.id));
      ("status", `String (status_name run.status));
      ("started_at", `String run.metadata.started_at);
      ("finished_at", `String run.metadata.finished_at);
      ( "workspace",
        `Assoc
          [
            ("digest", `String run.metadata.workspace_digest);
            ("toolchain", `String run.metadata.toolchain);
          ] );
      ("resolved_config", resolved_config);
      ( "input_fingerprint",
        `Assoc
          [
            ("digest", `String run.metadata.input_fingerprint);
            ("workspace_digest", `String run.metadata.workspace_digest);
            ("toolchain", `String run.metadata.toolchain);
            ("config_digest", `String run.metadata.config_digest);
          ] );
      ("profile", `String (Core.Operator.Profile.name run.metadata.profile));
      ("selection", `Assoc [ ("description", `String run.metadata.selection) ]);
      ( "test",
        `Assoc
          [
            ( "command",
              `List
                (List.map
                   (fun value -> `String value)
                   (Core.Nonempty_argv.to_list run.metadata.test_command)) );
            ( "baseline_duration_seconds",
              duration_option_to_json run.metadata.baseline_duration );
            ("timeout_seconds", duration_option_to_json run.metadata.timeout);
            ("stages", stages_json);
            ( "inventory",
              `Assoc
                [
                  ( "state",
                    `String
                      (if run.metadata.baseline_stages = [] then "unavailable"
                       else "stage-level") );
                  ( "tests",
                    `List
                      (List.map
                         (fun stage -> `String stage.name)
                         run.metadata.baseline_stages) );
                  ( "hit_map",
                    `List
                      (List.map
                         (fun entry ->
                           `Assoc
                             [
                               ("test", `String entry.test);
                               ( "mutant_ids",
                                 `List
                                   (List.map
                                      (fun id -> `String id)
                                      entry.mutant_ids) );
                             ])
                         run.metadata.hit_map) );
                ] );
          ] );
      ( "cache",
        `Assoc
          [
            ("mode", `String run.metadata.cache_mode);
            ("key", `String run.metadata.cache_key);
          ] );
      ("summary", summary_json run);
      ( "evidence",
        `Assoc
          [
            ("level", `String (evidence_level_name (run_evidence_level run)));
            ("complete", `Bool (run.completeness = Complete));
            ("executed", `Int executed_evidence);
            ("exact_cache", `Int exact_cache_evidence);
            ("estimated", `Int estimated_evidence);
            ("checkpointed", `Int run.checkpointed);
            ("resumed", `Int resumed_evidence);
          ] );
      ( "mutants",
        `List
          (List.map
             (fun result ->
               result_to_json ~coverage:(result_coverage run result) result)
             run.results) );
      ("not_run", `List (List.map mutant_to_json not_run));
      ( "expectations",
        `List (List.map expectation_evaluation_to_json run.expectations) );
      ( "failure",
        match run.status with
        | Failed error -> failure_to_json error
        | Completed | Interrupted -> `Null );
      ( "skips",
        `List
          (List.map
             (fun (skip : skip_summary) ->
               `Assoc
                 [
                   ("reason", `String skip.reason);
                   ("count", `Int skip.count);
                   ( "examples",
                     `List (List.map (fun value -> `String value) skip.examples)
                   );
                 ])
             run.skipped) );
      ( "warnings",
        `List
          (List.map
             (fun (warning : warning) ->
               `Assoc
                 [
                   ("code", `String warning.code);
                   ("message", `String warning.message);
                 ])
             run.warnings) );
    ]

let run_to_string run =
  Yojson.Safe.pretty_to_string ~std:true (run_to_yojson run) ^ "\n"

let rec failure_of_json json =
  let open Yojson.Safe.Util in
  let* phase = Error.phase_of_name (json |> member "phase" |> to_string) in
  let* cause = Error.cause_of_name (json |> member "cause" |> to_string) in
  let context =
    json |> member "context" |> to_assoc
    |> List.map (fun (key, value) -> (key, to_string value))
  in
  let rec decode_suppressed decoded = function
    | [] -> Ok (List.rev decoded)
    | value :: rest ->
        let* error = failure_of_json value in
        decode_suppressed (error :: decoded) rest
  in
  let* suppressed =
    decode_suppressed [] (json |> member "suppressed" |> to_list)
  in
  let optional_string name =
    match json |> member name with `String value -> Some value | _ -> None
  in
  let message = json |> member "message" |> to_string in
  match (optional_string "code", optional_string "remediation") with
  | Some code, Some remediation ->
      Ok
        (Error.restore_with_details ~phase ~cause ~code ~remediation ~message
           ~context ~suppressed)
  | _ -> Ok (Error.restore ~phase ~cause ~message ~context ~suppressed)

let decode_status json =
  let open Yojson.Safe.Util in
  let* failure =
    match json with
    | `Assoc fields -> (
        match List.assoc_opt "failure" fields with
        | Some value -> Ok value
        | None -> Error "run report is missing its failure member")
    | _ -> Error "run report must be an object"
  in
  match (json |> member "status" |> to_string, failure) with
  | "completed", `Null -> Ok Completed
  | "interrupted", `Null -> Ok Interrupted
  | "failed", `Null -> Error "failed run status requires a failure object"
  | "failed", failure ->
      Result.map (fun error -> Failed error) (failure_of_json failure)
  | ("completed" | "interrupted"), _ ->
      Error "non-failed run status must have a null failure"
  | value, _ -> Error (Printf.sprintf "unknown run status %S" value)

let duration_equal left right = Core.Duration.compare left right = 0

let slowest_duration = function
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun slowest duration ->
             if Core.Duration.compare duration slowest > 0 then duration
             else slowest)
           first rest)

let validate_baseline baseline_duration stages =
  let rec validate_stages runs = function
    | [] -> Ok (List.rev runs |> List.concat)
    | (stage : baseline_stage) :: rest -> (
        match slowest_duration stage.runs with
        | None -> Error "baseline stage has no successful run"
        | Some slowest when not (duration_equal slowest stage.slowest) ->
            Error
              (Printf.sprintf
                 "baseline stage %S slowest duration contradicts its runs"
                 stage.name)
        | Some _ -> validate_stages (stage.runs :: runs) rest)
  in
  let* runs = validate_stages [] stages in
  match (baseline_duration, slowest_duration runs) with
  | None, None -> Ok ()
  | Some encoded, Some derived when duration_equal encoded derived -> Ok ()
  | None, Some _ ->
      Error "overall baseline duration is absent despite recorded runs"
  | Some _, None ->
      Error "overall baseline duration exists without recorded runs"
  | Some _, Some _ ->
      Error "overall baseline duration contradicts the recorded runs"

let full_id mutant = Core.Mutant.Id.full (Core.Mutant.id mutant)

let add_unique table ~set_name id value =
  if Hashtbl.mem table id then
    Error (Printf.sprintf "%s contains duplicate mutant ID %s" set_name id)
  else (
    Hashtbl.add table id value;
    Ok ())

let validate_mutant_sets results not_run =
  let executed = Hashtbl.create (List.length results) in
  let pending = Hashtbl.create (List.length not_run) in
  let* () =
    List.fold_left
      (fun validation (result : mutant_result) ->
        let* () = validation in
        add_unique executed ~set_name:"mutants" (full_id result.mutant) result)
      (Ok ()) results
  in
  let* () =
    List.fold_left
      (fun validation mutant ->
        let* () = validation in
        let id = full_id mutant in
        if Hashtbl.mem executed id then
          Error
            (Printf.sprintf "mutant ID %s occurs in both mutants and not_run" id)
        else add_unique pending ~set_name:"not_run" id mutant)
      (Ok ()) not_run
  in
  Ok (executed, pending)

let evaluated_expectation_status = function
  | Expectation_fulfilled | Expectation_unfulfilled_killed
  | Expectation_unfulfilled_confirmed_timeout | Expectation_inconclusive _
  | Expectation_error _ ->
      true
  | Expectation_stale | Expectation_not_evaluated -> false

let valid_full_id id =
  String.length id = 64 && Core.Mutant.Id.is_valid_prefix id

let validate_expectations ~executed ~pending expectations =
  let ledger = Hashtbl.create (List.length expectations) in
  let* () =
    List.fold_left
      (fun validation (evaluation : expectation_evaluation) ->
        let* () = validation in
        if not (valid_full_id evaluation.mutant_id) then
          Error "expectation ledger contains an invalid full mutant ID"
        else if String.trim evaluation.reason = "" then
          Error "expectation ledger contains an empty reason"
        else
          add_unique ledger ~set_name:"expectations" evaluation.mutant_id
            evaluation)
      (Ok ()) expectations
  in
  let* () =
    List.fold_left
      (fun validation (evaluation : expectation_evaluation) ->
        let* () = validation in
        match Hashtbl.find_opt executed evaluation.mutant_id with
        | Some result -> (
            match result.expected_reason with
            | None ->
                Error
                  "expectation ledger marks a result that has no expectation"
            | Some reason when not (String.equal reason evaluation.reason) ->
                Error "expectation ledger reason contradicts the mutant result"
            | Some _
              when evaluation.status <> expectation_status_of_result result ->
                Error "expectation ledger status contradicts the mutant result"
            | Some _ -> Ok ())
        | None when evaluated_expectation_status evaluation.status ->
            Error "evaluated expectation has no executed mutant result"
        | None
          when Hashtbl.mem pending evaluation.mutant_id
               && evaluation.status = Expectation_stale ->
            Error "not-run mutant is recorded as a stale expectation"
        | None -> Ok ())
      (Ok ()) expectations
  in
  Hashtbl.fold
    (fun id (result : mutant_result) validation ->
      let* () = validation in
      match (result.expected_reason, Hashtbl.find_opt ledger id) with
      | None, None -> Ok ()
      | None, Some _ ->
          Error "unexpected mutant result has an expectation ledger entry"
      | Some _, None ->
          Error "expected mutant result is absent from the expectation ledger"
      | Some _, Some _ -> Ok ())
    executed (Ok ())

let validate_hit_map ~executed ~pending ~baseline_stages hit_map =
  let stage_names =
    List.map (fun (stage : baseline_stage) -> stage.name) baseline_stages
  in
  List.fold_left
    (fun validation (entry : hit_map_entry) ->
      let* () = validation in
      if not (List.mem entry.test stage_names) then
        Error
          (Printf.sprintf "hit-map test %S is not a recorded stage" entry.test)
      else
        List.fold_left
          (fun validation id ->
            let* () = validation in
            if Hashtbl.mem executed id || Hashtbl.mem pending id then Ok ()
            else
              Error
                (Printf.sprintf
                   "hit-map mutant ID %s is absent from the report catalog" id))
          (Ok ()) entry.mutant_ids)
    (Ok ()) hit_map

let decode_completeness summary_json not_run =
  let open Yojson.Safe.Util in
  match summary_json |> member "kind" |> to_string with
  | "complete" when not_run = [] -> Ok Complete
  | "complete" -> Error "complete report contains not-run mutants"
  | "partial" -> Ok (Partial not_run)
  | value -> Error (Printf.sprintf "unknown summary kind %S" value)

let validate_summary encoded run =
  let open Yojson.Safe.Util in
  let expected = summary_json run in
  let fields =
    [
      "kind";
      "total";
      "executed";
      "not_run";
      "killed";
      "survived";
      "timeout";
      "unconfirmed_timeouts";
      "inconclusive";
      "error";
      "expected_survivors";
      "unexpected_survivors";
      "unfulfilled_expectations";
      "detected";
      "score";
    ]
  in
  List.fold_left
    (fun validation field ->
      let* () = validation in
      if member field encoded = member field expected then Ok ()
      else Error (Printf.sprintf "summary.%s contradicts report data" field))
    (Ok ()) fields

let validate_evidence encoded run =
  let open Yojson.Safe.Util in
  let expected = run_to_yojson run |> member "evidence" in
  let fields =
    [
      "level";
      "complete";
      "executed";
      "exact_cache";
      "estimated";
      "checkpointed";
      "resumed";
    ]
  in
  List.fold_left
    (fun validation field ->
      let* () = validation in
      if member field encoded = member field expected then Ok ()
      else Error (Printf.sprintf "evidence.%s contradicts report data" field))
    (Ok ()) fields

let run_of_json json =
  try
    let open Yojson.Safe.Util in
    let document_type = json |> member "document_type" |> to_string in
    let schema_version = json |> member "schema_version" |> to_int in
    if
      not
        (String.equal document_type "ocaml-mutants.run-report-v1"
         && schema_version = 1
        || String.equal document_type "ocaml-mutants.run-report-v2"
           && schema_version = 2)
    then Error "unsupported run report document"
    else
      let* id = Core.Run_id.of_string (json |> member "run_id" |> to_string) in
      let* command =
        Core.Nonempty_argv.of_list
          (json |> member "test" |> member "command" |> to_list
         |> List.map to_string)
      in
      let duration_option value =
        match value with
        | `Null -> Ok None
        | value ->
            Result.map
              (fun duration -> Some duration)
              (Core.Duration.of_seconds (to_float value))
      in
      let* baseline_duration =
        duration_option
          (json |> member "test" |> member "baseline_duration_seconds")
      in
      let* timeout =
        duration_option (json |> member "test" |> member "timeout_seconds")
      in
      let* baseline_stages =
        let decode_stage value =
          let* command =
            Core.Nonempty_argv.of_list
              (value |> member "command" |> to_list |> List.map to_string)
          in
          let* runs =
            let rec decode decoded = function
              | [] -> Ok (List.rev decoded)
              | value :: rest ->
                  let* duration = Core.Duration.of_seconds (to_float value) in
                  decode (duration :: decoded) rest
            in
            decode [] (value |> member "baseline_runs_seconds" |> to_list)
          in
          let* slowest =
            Core.Duration.of_seconds
              (value |> member "slowest_baseline_seconds" |> to_float)
          in
          Ok
            {
              name = value |> member "name" |> to_string;
              command;
              runs;
              slowest;
            }
        in
        let rec decode decoded = function
          | [] -> Ok (List.rev decoded)
          | value :: rest ->
              let* stage = decode_stage value in
              decode (stage :: decoded) rest
        in
        decode [] (json |> member "test" |> member "stages" |> to_list)
      in
      let* () = validate_baseline baseline_duration baseline_stages in
      let* hit_map =
        if schema_version = 1 then Ok []
        else
          let decode_entry value =
            let test = value |> member "test" |> to_string in
            let mutant_ids =
              value |> member "mutant_ids" |> to_list |> List.map to_string
            in
            if String.trim test = "" then Error "hit-map test name is empty"
            else if
              List.exists
                (fun id ->
                  String.length id <> 64
                  || not
                       (String.for_all
                          (function
                            | '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
                          id))
                mutant_ids
            then Error "hit map contains an invalid full mutant ID"
            else if
              List.length mutant_ids
              <> List.length (List.sort_uniq String.compare mutant_ids)
            then Error "hit map contains duplicate mutant IDs"
            else Ok { test; mutant_ids }
          in
          let rec decode seen decoded = function
            | [] -> Ok (List.rev decoded)
            | value :: rest ->
                let* entry = decode_entry value in
                if List.mem entry.test seen then
                  Error "hit map contains duplicate test names"
                else decode (entry.test :: seen) (entry :: decoded) rest
          in
          decode [] []
            (json |> member "test" |> member "inventory" |> member "hit_map"
           |> to_list)
      in
      let* status = decode_status json in
      let decode_list decoder values =
        List.fold_left
          (fun result value ->
            match (result, decoder value) with
            | Ok values, Ok value -> Ok (value :: values)
            | (Error _ as error), _ -> error
            | Ok _, Error error -> Error error)
          (Ok []) values
        |> Result.map List.rev
      in
      let* results =
        decode_list result_of_json (json |> member "mutants" |> to_list)
      in
      let resumed =
        List.fold_left
          (fun count result ->
            if evidence_origin_is_resumed result.evidence_origin then count + 1
            else count)
          0 results
      in
      let* checkpointed =
        if schema_version = 1 then Ok (List.length results)
        else
          let count =
            json |> member "evidence" |> member "checkpointed" |> to_int
          in
          if count < resumed then
            Error "evidence.checkpointed is below the resumed result count"
          else if count > List.length results then
            Error "evidence.checkpointed exceeds the result count"
          else Ok count
      in
      let* not_run =
        decode_list mutant_of_json (json |> member "not_run" |> to_list)
      in
      let* expectations =
        decode_list expectation_evaluation_of_json
          (json |> member "expectations" |> to_list)
      in
      let encoded_summary = member "summary" json in
      let* completeness = decode_completeness encoded_summary not_run in
      let* executed, pending = validate_mutant_sets results not_run in
      let* () = validate_expectations ~executed ~pending expectations in
      let* () = validate_hit_map ~executed ~pending ~baseline_stages hit_map in
      let workspace = member "workspace" json in
      let cache = member "cache" json in
      let selection = member "selection" json in
      let* profile =
        Core.Operator.Profile.of_string (json |> member "profile" |> to_string)
      in
      let skipped =
        json |> member "skips" |> to_list
        |> List.map (fun value ->
            {
              reason = value |> member "reason" |> to_string;
              count = value |> member "count" |> to_int;
              examples =
                value |> member "examples" |> to_list |> List.map to_string;
            })
      in
      let execution_mode, historical_reuse =
        if schema_version = 1 then
          ( "strict",
            match cache |> member "mode" |> to_string with
            | "on" -> "exact"
            | _ -> "off" )
        else
          ( json |> member "resolved_config" |> member "execution"
            |> member "mode" |> to_string,
            json |> member "resolved_config" |> member "cache"
            |> member "historical_reuse" |> to_string )
      in
      let resolved_config, input_fingerprint, config_digest =
        if schema_version = 1 then
          let reconstructed =
            let cache_mode =
              match cache |> member "mode" |> to_string with
              | "on" -> Config.On
              | "auto" -> Config.Auto
              | _ -> Config.Off
            in
            let stages =
              match baseline_stages with
              | [] -> [ { Config.name = "full"; command } ]
              | stages ->
                  List.map
                    (fun (stage : baseline_stage) ->
                      { Config.name = stage.name; command = stage.command })
                    stages
            in
            let historical_reuse =
              if cache_mode = Config.On then Config.Reuse_exact
              else Config.Reuse_off
            in
            {
              Config.defaults with
              mutation = { Config.defaults.mutation with profile };
              test = { Config.defaults.test with command; stages; timeout };
              execution =
                { Config.defaults.execution with mode = Config.Strict };
              cache =
                {
                  Config.defaults.cache with
                  mode = cache_mode;
                  historical_reuse;
                };
            }
            |> Config.to_yojson
          in
          let digest = cache |> member "key" |> to_string in
          ( reconstructed,
            digest,
            sha256 (Yojson.Safe.to_string ~std:true reconstructed) )
        else
          let resolved = json |> member "resolved_config" in
          let fingerprint = json |> member "input_fingerprint" in
          ( resolved,
            fingerprint |> member "digest" |> to_string,
            fingerprint |> member "config_digest" |> to_string )
      in
      let* () =
        if schema_version = 1 then Ok ()
        else
          let redactions =
            resolved_config |> member "privacy" |> member "redactions"
            |> to_list |> List.map to_string
          in
          if
            List.for_all
              (String.equal Config.report_redaction_placeholder)
              redactions
          then Ok ()
          else Error "resolved_config exposes a privacy redaction literal"
      in
      let expected_config_digest =
        sha256 (Yojson.Safe.to_string ~std:true resolved_config)
      in
      let* () =
        if String.equal config_digest expected_config_digest then Ok ()
        else Error "input_fingerprint.config_digest contradicts resolved_config"
      in
      let run =
        {
          metadata =
            {
              id;
              started_at = json |> member "started_at" |> to_string;
              finished_at = json |> member "finished_at" |> to_string;
              workspace_digest = workspace |> member "digest" |> to_string;
              toolchain = workspace |> member "toolchain" |> to_string;
              profile;
              selection = selection |> member "description" |> to_string;
              test_command = command;
              baseline_duration;
              baseline_stages;
              hit_map;
              timeout;
              cache_mode = cache |> member "mode" |> to_string;
              execution_mode;
              historical_reuse;
              cache_key = cache |> member "key" |> to_string;
              resolved_config;
              input_fingerprint;
              config_digest;
            };
          status;
          results;
          checkpointed;
          completeness;
          expectations;
          skipped;
          warnings =
            json |> member "warnings" |> to_list
            |> List.map (fun value ->
                {
                  code = value |> member "code" |> to_string;
                  message = value |> member "message" |> to_string;
                });
        }
      in
      let* () =
        if schema_version = 1 then Ok ()
        else
          let encoded_results = json |> member "mutants" |> to_list in
          let rec validate_coverage encoded decoded =
            match (encoded, decoded) with
            | [], [] -> Ok ()
            | value :: encoded, result :: decoded ->
                let actual = value |> member "coverage" |> to_string in
                let expected = result_coverage run result in
                if String.equal actual expected then
                  validate_coverage encoded decoded
                else
                  Error
                    "mutant coverage contradicts the recorded runtime hit map"
            | _ -> Error "mutant coverage list length is inconsistent"
          in
          validate_coverage encoded_results run.results
      in
      let* () = validate_summary encoded_summary run in
      let* () =
        if schema_version = 1 then Ok ()
        else validate_evidence (member "evidence" json) run
      in
      Ok run
  with
  | Yojson.Safe.Util.Type_error (message, _) -> Error message
  | Yojson.Json_error message | Invalid_argument message -> Error message

let outcome_path (store : t) ~key ~id =
  let* key_path = Store_path.child store.topology.outcomes key in
  let* outcome = Store_path.child key_path (id ^ ".json") in
  Ok (store_path store outcome)

let cacheable_result result =
  if result_evidence_level result = Estimated then false
  else
    match result.outcome with
    | Core.Outcome.Timeout -> result.timeout_confirmed
    | Core.Outcome.Killed | Core.Outcome.Survived -> true
    | Core.Outcome.Inconclusive _ | Core.Outcome.Error _ -> false

let load_mutant (store : t) ~key ~source ~expected =
  let id = Core.Mutant.Id.short (Core.Mutant.id expected) in
  match Result.bind (outcome_path store ~key ~id) read_file with
  | Error _ -> None
  | Ok contents -> (
      try
        let json = Yojson.Safe.from_string contents in
        let open Yojson.Safe.Util in
        if
          json |> member "document_type" |> to_string
          <> "ocaml-mutants.cache-outcome-v3"
          || json |> member "cache_abi" |> to_int <> 3
        then None
        else
          match result_of_json (member "result" json) with
          | Ok result
            when Core.Mutant.equal_identity result.mutant expected
                 && String.equal
                      (Core.Source.digest source)
                      (Core.Mutant.source_digest result.mutant)
                 && cacheable_result result ->
              Some
                {
                  result with
                  cached = true;
                  evidence_origin = Historical_exact;
                }
          | _ -> None
      with _ -> None)

let save_mutant (store : t) ~key result =
  if not (cacheable_result result) then Ok ()
  else
    let id = Core.Mutant.Id.short (Core.Mutant.id result.mutant) in
    let json =
      `Assoc
        [
          ("document_type", `String "ocaml-mutants.cache-outcome-v3");
          ("cache_abi", `Int 3);
          ( "result",
            result_to_json
              { result with cached = false; evidence_origin = Execution } );
        ]
      |> Yojson.Safe.to_string
    in
    match outcome_path store ~key ~id with
    | Error message ->
        Error
          (Error.create ~phase:Error.Cache ~cause:Error.Invalid_input
             "cannot address mutant cache entry: %s" message)
    | Ok path -> (
        match atomic_write path json with
        | Ok () -> Ok ()
        | Error message ->
            Error
              (Error.create ~phase:Error.Cache ~cause:Error.Io_failure
                 "cannot save mutant cache: %s" message))

let reservation_id reservation = reservation.id

let reservation_owner_proof_for_scope ~scope ~run_id ~owner_pid =
  String.concat "\000"
    [
      "ocaml-mutants-reservation-owner-v2";
      normalize_for_compare scope;
      run_id;
      string_of_int owner_pid;
    ]
  |> sha256

let reservation_owner_proof (store : t) id owner_pid =
  reservation_owner_proof_for_scope ~scope:(scope_display store)
    ~run_id:(Core.Run_id.to_string id) ~owner_pid

let reservation_marker_contents (store : t) id owner_pid =
  Printf.sprintf
    "owner=ocaml-mutants\n\
     schema=2\n\
     scope=%s\n\
     run_id=%s\n\
     owner_pid=%d\n\
     proof=%s\n"
    (sha256 (normalize_for_compare (scope_display store)))
    (Core.Run_id.to_string id) owner_pid
    (reservation_owner_proof store id owner_pid)

let reservation_run_id ~started_at ~owner_pid ~sequence =
  if Int64.compare sequence 0L < 0 then
    Error "run reservation sequence cannot be negative"
  else
    Core.Run_id.create ~started_at
      ~nonce:(Printf.sprintf "p%d-s%Ld" owner_pid sequence)

let retain_reservation_residual (lease : reservation_lease) error
    (released : lease_release) =
  match released.residual with
  | None -> ()
  | Some residual ->
      lease.retained_resources <- residual @ lease.retained_resources;
      lease.retained_error <- suppress_optional lease.retained_error error

let reservation_native_failure store lease primary resources =
  let released = cleanup_native_resources ~root:store.root resources in
  let error = suppress_all primary released.errors in
  retain_reservation_residual lease error released;
  Error error

let directory_observation_name = function
  | Publication_cache.Directory_creation_observed { path } ->
      Format.asprintf "observed:%a" Cache_fs.Relative.pp path
  | Directory_creation_may_have_committed { path } ->
      Format.asprintf "may-have-committed:%a" Cache_fs.Relative.pp path

let add_directory_observation_context error created =
  match created with
  | [] -> error
  | _ ->
      Error.with_context "created_components"
        (String.concat "," (List.map directory_observation_name created))
        error

let ensure_run_directory (store : t) (lease : reservation_lease) =
  match
    Publication_cache.mkdirs lease.shared.native.root store.topology.runs
  with
  | Publication_cache.Directories_ready { advisories = []; _ } -> Ok ()
  | Directories_ready { advisories; _ } ->
      let errors =
        native_lease_cache_advisories ~root:store.root ~phase:Error.Reporting
          advisories
      in
      let primary = suppress_all (List.hd errors) (List.tl errors) in
      reservation_native_failure store lease primary
        (retry_resources_of_advisories advisories)
  | Directories_incomplete { created; failure } ->
      let primary =
        add_directory_observation_context
          (native_lease_cache_failure ~root:store.root ~phase:Error.Reporting
             failure)
          created
      in
      reservation_native_failure store lease primary
        (retry_resources_of_failure failure)

let fail_marker_stage store lease primary resources =
  reservation_native_failure store lease primary resources

let reserve_marker (store : t) (lease : reservation_lease) ~marker_path
    ~contents =
  let display_path = store_path store marker_path in
  match
    Publication_cache.stage_file lease.shared.native.root marker_path ~contents
  with
  | Publication_cache.Staged stage -> Ok (`Created stage)
  | Staging_not_created failure when clean_stage_collision failure ->
      Ok `Collision
  | Staging_not_created failure ->
      fail_marker_stage store lease
        (cache_failure_error ~path:display_path failure)
        (retry_resources_of_failure failure)
  | Staging_incomplete_actionable { live_stage; failure } ->
      fail_marker_stage store lease
        (cache_failure_error ~path:display_path failure)
        [ Cleanup_marker { stage = live_stage; path = display_path } ]
  | Staging_incomplete_audit_only { residual; failure } ->
      fail_marker_stage store lease
        (add_residual_context
           (cache_failure_error ~path:display_path failure)
           [ residual ])
        (retry_resources_of_failure failure)

let reserve (store : t) ~started_at =
  let* lease = acquire_shared_root_lease store in
  let outcome =
    try
      let result =
        match ensure_run_directory store lease with
        | Error _ as error -> error
        | Ok () ->
            let owner_pid = Unix.getpid () in
            let rec allocate () =
              match store.next_reservation_sequence () with
              | Error message ->
                  Error
                    (Error.create ~phase:Error.Reporting
                       ~cause:Error.Invariant_violation "%s" message)
              | Ok sequence -> (
                  match reservation_run_id ~started_at ~owner_pid ~sequence with
                  | Error message ->
                      Error
                        (Error.create ~phase:Error.Reporting
                           ~cause:Error.Invariant_violation "%s" message)
                  | Ok id -> (
                      match
                        Store_path.child store.topology.runs
                          (Core.Run_id.to_string id ^ ".reserved")
                      with
                      | Error message ->
                          Error
                            (Error.create ~phase:Error.Reporting
                               ~cause:Error.Invariant_violation
                               "cannot address run reservation: %s" message)
                      | Ok marker_path -> (
                          let contents =
                            reservation_marker_contents store id owner_pid
                          in
                          match
                            reserve_marker store lease ~marker_path ~contents
                          with
                          | Ok (`Created marker_stage) ->
                              Ok
                                {
                                  id;
                                  owner_pid;
                                  scope = store.topology.scope;
                                  marker_path;
                                  marker_stage = Some marker_stage;
                                  lease;
                                  mutex = Mutex.create ();
                                  state = Active;
                                }
                          | Ok `Collision -> allocate ()
                          | Error _ as error -> error)))
            in
            allocate ()
      in
      Leased_returned result
    with exception_ ->
      Leased_raised (exception_, Printexc.get_raw_backtrace ())
  in
  match outcome with
  | Leased_returned (Ok reservation) -> Ok reservation
  | Leased_returned (Error _) | Leased_raised _ ->
      finish_leased_action outcome (release_shared_root_lease lease)

let with_reservation_lock reservation action =
  Mutex.lock reservation.mutex;
  Fun.protect action ~finally:(fun () -> Mutex.unlock reservation.mutex)

let same_scope left right = Store_path.equal left right

let verify_store (store : t) (reservation : reservation) =
  if reservation.owner_pid <> Unix.getpid () then
    Error
      (Error.create ~phase:Error.Reporting ~cause:Error.Invariant_violation
         ~context:
           [
             ("reservation_owner_pid", string_of_int reservation.owner_pid);
             ("current_pid", string_of_int (Unix.getpid ()));
           ]
         "run reservation capability cannot cross a process boundary")
  else if not (same_scope store.topology.scope reservation.scope) then
    Error
      (Error.create ~phase:Error.Reporting ~cause:Error.Invariant_violation
         "run reservation belongs to a different store namespace")
  else
    match
      Store_path.child store.topology.runs
        (Core.Run_id.to_string reservation.id ^ ".reserved")
    with
    | Error message ->
        Error
          (Error.create ~phase:Error.Reporting ~cause:Error.Invariant_violation
             "cannot address run reservation: %s" message)
    | Ok expected ->
        if not (Store_path.equal expected reservation.marker_path) then
          Error
            (Error.create ~phase:Error.Reporting
               ~cause:Error.Invariant_violation
               "run reservation marker path does not match its store namespace")
        else Ok ()

let verify_active_marker (store : t) (reservation : reservation) =
  let* () = verify_store store reservation in
  match reservation.state with
  | Finalized ->
      Error
        (Error.create ~phase:Error.Reporting ~cause:Error.Invariant_violation
           "run reservation has already been finalized")
  | Active | Staged -> (
      match reservation.marker_stage with
      | Some marker_stage
        when (not reservation.lease.released)
             && Publication_cache.owner_equal
                  (Publication_cache.staged_owner marker_stage)
                  (Publication_cache.root_owner
                     reservation.lease.shared.native.root) ->
          Ok ()
      | Some _ ->
          Error
            (Error.create ~phase:Error.Reporting
               ~cause:Error.Invariant_violation
               "run reservation marker capability belongs to a different root \
                owner")
      | None ->
          Error
            (Error.create ~phase:Error.Reporting
               ~cause:Error.Invariant_violation
               "run reservation no longer owns a live marker capability"))

let journal_error ~path message =
  Error.create ~phase:Error.Cache ~cause:Error.Corrupt_cache
    ~context:[ ("path", path) ]
    "run journal %s" message

let journal_path_error message =
  Error.create ~phase:Error.Cache ~cause:Error.Invalid_input
    "cannot address run journal: %s" message

let journal_body ~key ~session_id ~status =
  `Assoc
    [
      ("fingerprint", `String key);
      ("session_run_id", `String (Core.Run_id.to_string session_id));
      ("status", `String status);
    ]

let checked_document ~document_type body =
  let encoded_body = Yojson.Safe.to_string body in
  `Assoc
    [
      ("document_type", `String document_type);
      ("schema_version", `Int 1);
      ("body", body);
      ("checksum_sha256", `String (sha256 encoded_body));
    ]

let journal_state_string ~key ~session_id ~status =
  checked_document ~document_type:"ocaml-mutants.run-journal-state-v1"
    (journal_body ~key ~session_id ~status)
  |> Yojson.Safe.to_string

let decode_checked_document ~document_type contents =
  try
    let open Yojson.Safe.Util in
    let document = Yojson.Safe.from_string contents in
    if document |> member "document_type" |> to_string <> document_type then
      Error "has an unsupported document type"
    else if document |> member "schema_version" |> to_int <> 1 then
      Error "has an unsupported schema version"
    else
      let body = member "body" document in
      let checksum = document |> member "checksum_sha256" |> to_string in
      if String.equal checksum (sha256 (Yojson.Safe.to_string body)) then
        Ok body
      else Error "checksum does not match its canonical body"
  with
  | Yojson.Json_error message
  | Yojson.Safe.Util.Type_error (message, _)
  | Invalid_argument message
  ->
    Error message

let decode_journal_state ~key contents =
  let open Yojson.Safe.Util in
  let* body =
    decode_checked_document ~document_type:"ocaml-mutants.run-journal-state-v1"
      contents
  in
  try
    if body |> member "fingerprint" |> to_string <> key then
      Error "fingerprint does not match its directory"
    else
      let* session_id =
        Core.Run_id.of_string (body |> member "session_run_id" |> to_string)
      in
      match body |> member "status" |> to_string with
      | "open" -> Ok (session_id, `Open)
      | "complete" -> Ok (session_id, `Complete)
      | _ -> Error "state is neither open nor complete"
  with Yojson.Safe.Util.Type_error (message, _) | Invalid_argument message ->
    Error message

let journal_retry_failure (store : t) (reservation : reservation) ~path failure
    =
  reservation_native_failure store reservation.lease
    (cache_failure_error ~path failure)
    (retry_resources_of_failure failure)

let journal_advisory_failure (store : t) (reservation : reservation) ~path
    advisories =
  match cache_advisory_errors ~path advisories with
  | [] -> Ok ()
  | primary :: suppressed ->
      reservation_native_failure store reservation.lease
        (suppress_all primary suppressed)
        (retry_resources_of_advisories advisories)

let ensure_journal_directory (store : t) (reservation : reservation) path =
  let display_path = store_path store path in
  match Publication_cache.mkdirs reservation.lease.shared.native.root path with
  | Publication_cache.Directories_ready { advisories; _ } ->
      journal_advisory_failure store reservation ~path:display_path advisories
  | Directories_incomplete { created; failure } ->
      reservation_native_failure store reservation.lease
        (add_directory_observation_context
           (cache_failure_error ~path:display_path failure)
           created)
        (retry_resources_of_failure failure)

let read_journal_file (store : t) (reservation : reservation) path ~limit =
  let display_path = store_path store path in
  match
    Publication_cache.read_regular reservation.lease.shared.native.root path
      ~limit
  with
  | Ok Cache_fs.Read_missing -> Ok None
  | Ok (Cache_fs.Contents read) -> Ok (Some read.contents)
  | Error failure ->
      journal_retry_failure store reservation ~path:display_path failure

let replace_journal_file (store : t) (reservation : reservation) path contents =
  let display_path = store_path store path in
  match
    Publication_cache.replace_file reservation.lease.shared.native.root path
      ~contents
  with
  | Publication_cache.Replaced { advisories } ->
      journal_advisory_failure store reservation ~path:display_path advisories
  | Replacement_not_published { live_stage; audit_only; failure } ->
      let primary =
        add_residual_context
          (cache_failure_error ~path:display_path failure)
          audit_only
      in
      reservation_native_failure store reservation.lease primary
        (Option.to_list
           (Option.map (fun stage -> Cleanup_stage stage) live_stage)
        @ retry_resources_of_failure failure)

let create_journal_file (store : t) (reservation : reservation) path contents =
  let display_path = store_path store path in
  match
    Publication_cache.create_exclusive reservation.lease.shared.native.root path
      ~contents
  with
  | Publication_cache.File_created { advisories } ->
      let* () =
        journal_advisory_failure store reservation ~path:display_path advisories
      in
      Ok `Created
  | File_exists { advisories } ->
      let* () =
        journal_advisory_failure store reservation ~path:display_path advisories
      in
      Ok `Exists
  | File_not_created failure ->
      journal_retry_failure store reservation ~path:display_path failure
  | File_creation_incomplete { live_file; audit_only; failure } ->
      let primary =
        add_residual_context
          (cache_failure_error ~path:display_path failure)
          audit_only
      in
      reservation_native_failure store reservation.lease primary
        (Option.to_list
           (Option.map (fun stage -> Cleanup_stage stage) live_file)
        @ retry_resources_of_failure failure)

let journal_paths store key session_id =
  let* key_directory = Store_path.child store.topology.journals key in
  let* state_path = Store_path.child key_directory "state.json" in
  let* directory =
    Store_path.child key_directory (Core.Run_id.to_string session_id)
  in
  Ok (key_directory, state_path, directory)

let open_journal store reservation ~key ~fresh =
  with_reservation_lock reservation (fun () ->
      let* () = verify_active_marker store reservation in
      let* key_directory, state_path, current_directory =
        journal_paths store key reservation.id
        |> Result.map_error journal_path_error
      in
      let* () = ensure_journal_directory store reservation key_directory in
      let* state =
        read_journal_file store reservation state_path ~limit:65536L
      in
      let* prior =
        match state with
        | None -> Ok None
        | Some contents -> (
            match decode_journal_state ~key contents with
            | Ok state -> Ok (Some state)
            | Error message ->
                Error
                  (journal_error ~path:(store_path store state_path) message))
      in
      let session_id, resumed =
        match (fresh, prior) with
        | false, Some (session_id, `Open) -> (session_id, true)
        | _ -> (reservation.id, false)
      in
      let* _, _, directory =
        if Core.Run_id.compare session_id reservation.id = 0 then
          Ok (key_directory, state_path, current_directory)
        else
          journal_paths store key session_id
          |> Result.map_error journal_path_error
      in
      let* () = ensure_journal_directory store reservation directory in
      let* () =
        if resumed then Ok ()
        else
          replace_journal_file store reservation state_path
            (journal_state_string ~key ~session_id ~status:"open")
      in
      Ok
        {
          store;
          reservation;
          key;
          session_id;
          directory;
          state_path;
          resumed;
          closed = false;
        })

let journal_resumed (journal : journal) = journal.resumed

let checkpoint_path (journal : journal) mutant =
  Store_path.child journal.directory
    (Core.Mutant.Id.full (Core.Mutant.id mutant) ^ ".json")

let checkpointable_result result =
  result.outcome <> Core.Outcome.Timeout || result.timeout_confirmed

let checkpoint_string (journal : journal) result =
  let mutant_id = Core.Mutant.Id.full (Core.Mutant.id result.mutant) in
  let body =
    `Assoc
      [
        ("fingerprint", `String journal.key);
        ("session_run_id", `String (Core.Run_id.to_string journal.session_id));
        ("mutant_id", `String mutant_id);
        ("result", result_to_json result);
      ]
  in
  checked_document ~document_type:"ocaml-mutants.run-checkpoint-v1" body
  |> Yojson.Safe.to_string

let decode_checkpoint (journal : journal) ~source ~expected contents =
  let open Yojson.Safe.Util in
  try
    match
      decode_checked_document ~document_type:"ocaml-mutants.run-checkpoint-v1"
        contents
    with
    | Error _ -> Ok None
    | Ok body -> (
        let expected_id = Core.Mutant.Id.full (Core.Mutant.id expected) in
        if body |> member "fingerprint" |> to_string <> journal.key then Ok None
        else if
          body |> member "session_run_id" |> to_string
          <> Core.Run_id.to_string journal.session_id
        then Ok None
        else if body |> member "mutant_id" |> to_string <> expected_id then
          Ok None
        else
          match result_of_json (member "result" body) with
          | Ok result
            when Core.Mutant.equal_identity result.mutant expected
                 && String.equal
                      (Core.Source.digest source)
                      (Core.Mutant.source_digest result.mutant)
                 && checkpointable_result result ->
              let evidence_origin =
                if result_evidence_level result = Estimated then
                  Checkpoint_estimated
                else Checkpoint_resume
              in
              Ok (Some { result with cached = true; evidence_origin })
          | Ok _ | Error _ -> Ok None)
  with Yojson.Safe.Util.Type_error _ | Invalid_argument _ -> Ok None

let load_checkpoint (journal : journal) ~source ~expected =
  with_reservation_lock journal.reservation (fun () ->
      if journal.closed then Ok None
      else
        let* () = verify_active_marker journal.store journal.reservation in
        match checkpoint_path journal expected with
        | Error _ -> Ok None
        | Ok path -> (
            let* contents =
              read_journal_file journal.store journal.reservation path
                ~limit:33554432L
            in
            match contents with
            | None -> Ok None
            | Some contents ->
                decode_checkpoint journal ~source ~expected contents))

let valid_checkpoint_for (journal : journal) ~expected contents =
  let open Yojson.Safe.Util in
  try
    match
      decode_checked_document ~document_type:"ocaml-mutants.run-checkpoint-v1"
        contents
    with
    | Error _ -> false
    | Ok body -> (
        String.equal (body |> member "fingerprint" |> to_string) journal.key
        && String.equal
             (body |> member "session_run_id" |> to_string)
             (Core.Run_id.to_string journal.session_id)
        && String.equal
             (body |> member "mutant_id" |> to_string)
             (Core.Mutant.Id.full (Core.Mutant.id expected))
        &&
        match result_of_json (member "result" body) with
        | Ok result ->
            Core.Mutant.equal_identity result.mutant expected
            && checkpointable_result result
        | Error _ -> false)
  with Yojson.Safe.Util.Type_error _ | Invalid_argument _ -> false

let checkpoint_mutant (journal : journal) result =
  if not (checkpointable_result result) then Ok ()
  else
    with_reservation_lock journal.reservation (fun () ->
        let* () = verify_active_marker journal.store journal.reservation in
        if journal.closed then
          Error
            (Error.create ~phase:Error.Cache ~cause:Error.Invariant_violation
               "cannot checkpoint into a completed run journal")
        else
          let* path =
            checkpoint_path journal result.mutant
            |> Result.map_error journal_path_error
          in
          let contents = checkpoint_string journal result in
          let* disposition =
            create_journal_file journal.store journal.reservation path contents
          in
          match disposition with
          | `Created -> Ok ()
          | `Exists ->
              let* existing =
                read_journal_file journal.store journal.reservation path
                  ~limit:33554432L
              in
              if existing = Some contents then Ok ()
              else if
                match existing with
                | Some contents ->
                    valid_checkpoint_for journal ~expected:result.mutant
                      contents
                | None -> false
              then
                Error
                  (journal_error
                     ~path:(store_path journal.store path)
                     "checkpoint conflicts with an existing immutable record")
              else
                replace_journal_file journal.store journal.reservation path
                  contents)

let complete_journal (journal : journal) =
  with_reservation_lock journal.reservation (fun () ->
      let* () = verify_active_marker journal.store journal.reservation in
      if journal.closed then Ok ()
      else
        let* current =
          read_journal_file journal.store journal.reservation journal.state_path
            ~limit:65536L
        in
        let* () =
          match current with
          | None ->
              Error
                (journal_error
                   ~path:(store_path journal.store journal.state_path)
                   "state disappeared before completion")
          | Some contents -> (
              match decode_journal_state ~key:journal.key contents with
              | Ok (session_id, `Open)
                when Core.Run_id.compare session_id journal.session_id = 0 ->
                  Ok ()
              | Ok _ ->
                  Error
                    (journal_error
                       ~path:(store_path journal.store journal.state_path)
                       "state no longer names this open session")
              | Error message ->
                  Error
                    (journal_error
                       ~path:(store_path journal.store journal.state_path)
                       message))
        in
        let* () =
          replace_journal_file journal.store journal.reservation
            journal.state_path
            (journal_state_string ~key:journal.key
               ~session_id:journal.session_id ~status:"complete")
        in
        journal.closed <- true;
        Ok ())

let cleanup_marker store reservation =
  match verify_active_marker store reservation with
  | Error error -> [ error ]
  | Ok () -> (
      match reservation.marker_stage with
      | None -> assert false
      | Some marker_stage ->
          reservation.marker_stage <- None;
          let released =
            store.operations.discard_marker ~root:store.root
              ~path:(store_path store reservation.marker_path)
              marker_stage
          in
          let errors, residual_error =
            match (released.residual, released.errors) with
            | Some _, [] ->
                let error =
                  lease_error ~phase:Error.Cleanup
                    ~cause:Error.Invariant_violation
                    ~context:
                      [
                        ("cache_root", store.root);
                        ("path", store_path store reservation.marker_path);
                      ]
                    "reservation marker cleanup retained live authority \
                     without an error"
                in
                ([ error ], Some error)
            | Some _, (primary :: suppressed as errors) ->
                (errors, Some (List.fold_left Error.suppress primary suppressed))
            | None, errors -> (errors, None)
          in
          Option.iter
            (fun error ->
              retain_reservation_residual reservation.lease error released)
            residual_error;
          errors)

let finalize_reservation store reservation =
  let marker_errors = cleanup_marker store reservation in
  reservation.state <- Finalized;
  marker_errors @ release_shared_root_lease_errors reservation.lease

let abandon_reservation (store : t) (reservation : reservation) =
  with_reservation_lock reservation (fun () ->
      let* () = verify_store store reservation in
      match reservation.state with
      | Finalized -> release_shared_root_lease reservation.lease
      | Active | Staged ->
          finalize_reservation store reservation |> errors_as_result)

let stage_run (store : t) (reservation : reservation) =
  with_reservation_lock reservation (fun () ->
      let* () = verify_store store reservation in
      match reservation.state with
      | Staged ->
          Error
            (Error.create ~phase:Error.Reporting
               ~cause:Error.Invariant_violation
               "run reservation has already been staged")
      | Finalized ->
          Error
            (Error.create ~phase:Error.Reporting
               ~cause:Error.Invariant_violation
               "run reservation has already been finalized")
      | Active ->
          let id = Core.Run_id.to_string reservation.id in
          let* final_path =
            match Store_path.child store.topology.runs (id ^ ".json") with
            | Ok path -> Ok path
            | Error message ->
                Error
                  (Error.create ~phase:Error.Reporting
                     ~cause:Error.Invariant_violation
                     "cannot address staged run: %s" message)
          in
          let* pending_path =
            match
              Store_path.child store.topology.runs (id ^ ".json.pending")
            with
            | Ok path -> Ok path
            | Error message ->
                Error
                  (Error.create ~phase:Error.Reporting
                     ~cause:Error.Invariant_violation
                     "cannot address staged run: %s" message)
          in
          reservation.state <- Staged;
          Ok
            {
              store;
              reservation;
              pending_path;
              final_path;
              staged_state = Awaiting_finalization;
            })

let finalize_run staged =
  with_reservation_lock staged.reservation (fun () ->
      match staged.staged_state with
      | Finalization_issued ->
          Error
            (Error.create ~phase:Error.Reporting
               ~cause:Error.Invariant_violation
               "staged run has already been finalized")
      | Awaiting_finalization -> (
          match staged.reservation.state with
          | Active | Finalized ->
              Error
                (Error.create ~phase:Error.Reporting
                   ~cause:Error.Invariant_violation
                   "staged run and reservation state disagree")
          | Staged ->
              let* () =
                transfer_shared_root_lease_to_publication staged.store
                  staged.reservation.lease
              in
              staged.staged_state <- Finalization_issued;
              let cleanup_errors =
                cleanup_marker staged.store staged.reservation
              in
              staged.reservation.state <- Finalized;
              Ok
                {
                  publication =
                    {
                      staged;
                      lease = staged.reservation.lease;
                      publish_state = Ready_to_publish;
                    };
                  cleanup_errors;
                }))

let report_id_mismatch reservation run =
  Error.create ~phase:Error.Reporting ~cause:Error.Invariant_violation
    ~context:
      [
        ("reserved_run_id", Core.Run_id.to_string reservation.id);
        ("report_run_id", Core.Run_id.to_string run.metadata.id);
      ]
    "run report ID does not match its publication capability"

let latest_advisory ~path underlying =
  Error.suppress
    (Error.create ~phase:Error.Reporting ~cause:Error.Io_failure
       ~context:[ ("operation", "update-latest-index"); ("path", path) ]
       "authoritative run report was published but latest index update failed")
    underlying

let retain_publication_failure (lease : reservation_lease)
    (failure : publication_failure) =
  lease.retained_resources <-
    failure.retained_resources @ lease.retained_resources;
  if failure.retained_resources <> [] then
    lease.retained_error <- suppress_optional lease.retained_error failure.error;
  failure.error

let retain_publication_success (lease : reservation_lease)
    (success : publication_success) =
  if success.retained_resources <> [] then (
    lease.retained_resources <-
      success.retained_resources @ lease.retained_resources;
    let residual_error =
      match combine_errors success.advisories with
      | Some error -> error
      | None ->
          lease_error ~phase:Error.Cleanup ~cause:Error.Invariant_violation
            ~context:[ ("cache_root", lease.shared.root_key) ]
            "publication cleanup retained live authority without an advisory"
    in
    lease.retained_error <-
      suppress_optional lease.retained_error residual_error);
  success.advisories

let publish_while_leased staged lease run =
  let pending_path = store_path staged.store staged.pending_path in
  let final_path = store_path staged.store staged.final_path in
  let contents = run_to_string run in
  match
    staged.store.operations.stage_pending_report ~root:lease.shared.native.root
      ~pending:staged.pending_path ~display_path:pending_path
      ~root_display:staged.store.root ~contents
  with
  | Error failure -> Error (retain_publication_failure lease failure)
  | Ok stage -> (
      match
        staged.store.operations.publish_report ~stage
          ~pending_display:pending_path ~final:staged.final_path
          ~final_display:final_path ~root_display:staged.store.root
      with
      | Error failure -> Error (retain_publication_failure lease failure)
      | Ok publication_success ->
          let publication_advisories =
            retain_publication_success lease publication_success
          in
          let id = Core.Run_id.to_string staged.reservation.id in
          let latest = store_path staged.store staged.store.topology.latest in
          let advisories =
            match
              staged.store.operations.update_latest
                ~root:lease.shared.native.root
                ~latest:staged.store.topology.latest ~display_path:latest ~id
                ~root_display:staged.store.root
            with
            | Ok success -> retain_publication_success lease success
            | Error failure ->
                let underlying = retain_publication_failure lease failure in
                [ latest_advisory ~path:latest underlying ]
          in
          Ok (publication_advisories @ advisories))

let publish_run capability run =
  with_reservation_lock capability.staged.reservation (fun () ->
      match capability.publish_state with
      | Publish_attempted ->
          Error
            (Error.create ~phase:Error.Reporting
               ~cause:Error.Invariant_violation
               "run publication capability has already been used")
      | Ready_to_publish -> (
          capability.publish_state <- Publish_attempted;
          let staged = capability.staged in
          let reservation = staged.reservation in
          let fail primary =
            let cleanup_errors =
              release_shared_root_lease_errors capability.lease
            in
            Error (List.fold_left Error.suppress primary cleanup_errors)
          in
          if reservation.state <> Finalized then
            fail
              (Error.create ~phase:Error.Reporting
                 ~cause:Error.Invariant_violation
                 "run publication requires a finalized reservation")
          else if Core.Run_id.compare reservation.id run.metadata.id <> 0 then
            fail (report_id_mismatch reservation run)
          else
            match publish_while_leased staged capability.lease run with
            | Error primary -> fail primary
            | Ok advisories ->
                let advisories =
                  advisories @ release_shared_root_lease_errors capability.lease
                in
                Ok
                  {
                    path = store_path staged.store staged.final_path;
                    run;
                    advisories;
                  }))

let load_run (store : t) id =
  let id_result =
    if id = "latest" then
      match read_file (store_path store store.topology.latest) with
      | Ok value -> Core.Run_id.of_string (String.trim value)
      | Error _ -> Error "there is no latest run"
    else Core.Run_id.of_string id
  in
  match id_result with
  | Error message ->
      Error
        (Error.create ~phase:Error.Reporting ~cause:Error.Invalid_input "%s"
           message)
  | Ok id -> (
      let path =
        Store_path.child store.topology.runs (Core.Run_id.to_string id ^ ".json")
      in
      match path with
      | Error message ->
          Error
            (Error.create ~phase:Error.Reporting
               ~cause:Error.Invariant_violation "cannot address stored run: %s"
               message)
      | Ok relative -> (
          let path = store_path store relative in
          match read_file path with
          | Error message ->
              Error
                (Error.create ~phase:Error.Reporting ~cause:Error.Io_failure
                   "cannot read run %s: %s" (Core.Run_id.to_string id) message)
          | Ok contents -> (
              try
                match run_of_json (Yojson.Safe.from_string contents) with
                | Ok run -> Ok run
                | Error message ->
                    Error
                      (Error.create ~phase:Error.Reporting
                         ~cause:Error.Decode_failure "invalid stored run: %s"
                         message)
              with Yojson.Json_error message ->
                Error
                  (Error.create ~phase:Error.Reporting
                     ~cause:Error.Decode_failure "invalid stored run: %s"
                     message))))

let stored_run_ids (store : t) =
  let directory = store_path store store.topology.runs in
  if not (Sys.file_exists (windows_extended_path directory)) then []
  else
    files_recursive directory
    |> List.filter_map (fun relative ->
        if
          String.contains relative '/'
          || String.contains relative '\\'
          || not (Filename.check_suffix relative ".json")
        then None
        else
          let encoded =
            String.sub relative 0
              (String.length relative - String.length ".json")
          in
          match Core.Run_id.of_string encoded with
          | Ok id -> Some id
          | Error _ -> None)
    |> List.sort_uniq Core.Run_id.compare
    |> List.rev

let list_runs (store : t) =
  let rec load loaded = function
    | [] -> Ok (List.rev loaded)
    | id :: rest ->
        let* run = load_run store (Core.Run_id.to_string id) in
        load (run :: loaded) rest
  in
  load [] (stored_run_ids store)

let list_runs_best_effort (store : t) =
  let rec load loaded rejected = function
    | [] -> (List.rev loaded, List.rev rejected)
    | id :: rest -> (
        let encoded = Core.Run_id.to_string id in
        match load_run store encoded with
        | Ok run -> load (run :: loaded) rejected rest
        | Error error -> load loaded ((encoded, error) :: rejected) rest)
  in
  load [] [] (stored_run_ids store)

let stats (store : t) =
  files_recursive store.root
  |> List.filter (fun relative -> not (String.equal relative maintenance_lock))
  |> List.fold_left
       (fun (count, bytes) relative ->
         let path =
           Filename.concat store.root relative |> windows_extended_path
         in
         try
           let stat = Unix.stat path in
           (count + 1, Int64.add bytes (Int64.of_int stat.st_size))
         with Unix.Unix_error _ -> (count, bytes))
       (0, 0L)

let valid_lower_hex ~length value =
  String.length value = length
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
       value

let structural_run_id_has_owner run_id owner_pid =
  match List.rev (String.split_on_char '-' run_id) with
  | sequence_field :: pid_field :: _ ->
      let expected_pid = Printf.sprintf "p%d" owner_pid in
      let sequence =
        if string_starts_with ~prefix:"s" sequence_field then
          String.sub sequence_field 1 (String.length sequence_field - 1)
          |> Int64.of_string_opt
        else None
      in
      String.equal pid_field expected_pid
      && Option.fold ~none:false
           ~some:(fun sequence ->
             Int64.compare sequence 0L >= 0
             && String.equal sequence_field ("s" ^ Int64.to_string sequence))
           sequence
  | _ -> false

let reservation_marker_is_owned path contents =
  let suffix = ".reserved" in
  let name = Filename.basename path in
  let runs = Filename.dirname path in
  let scope = Filename.dirname runs in
  if
    (not (Filename.check_suffix name suffix))
    || not (String.equal (Filename.basename runs) "runs")
  then false
  else
    let path_id =
      String.sub name 0 (String.length name - String.length suffix)
    in
    match split_lines contents with
    | [
     owner; schema; scope_field; run_id_field; owner_pid_field; proof_field; "";
    ] ->
        let field prefix value =
          if string_starts_with ~prefix value then
            Some
              (String.sub value (String.length prefix)
                 (String.length value - String.length prefix))
          else None
        in
        let scope_value = field "scope=" scope_field in
        let run_id = field "run_id=" run_id_field in
        let owner_pid =
          Option.bind (field "owner_pid=" owner_pid_field) int_of_string_opt
        in
        let proof = field "proof=" proof_field in
        let expected_proof owner_pid =
          reservation_owner_proof_for_scope ~scope ~run_id:path_id ~owner_pid
        in
        String.equal owner "owner=ocaml-mutants"
        && String.equal schema "schema=2"
        && scope_value = Some (sha256 (normalize_for_compare scope))
        && run_id = Some path_id
        && Result.is_ok (Core.Run_id.of_string path_id)
        && Option.fold ~none:false ~some:(fun pid -> pid > 0) owner_pid
        && Option.fold ~none:false
             ~some:(structural_run_id_has_owner path_id)
             owner_pid
        && Option.fold ~none:false ~some:(valid_lower_hex ~length:64) proof
        && Option.map expected_proof owner_pid = proof
    | _ -> false

let validate_stale_reservation path =
  match read_file path with
  | Ok contents when reservation_marker_is_owned path contents -> Ok ()
  | Ok _ ->
      Error
        (Error.create ~phase:Error.Cache ~cause:Error.Corrupt_cache
           ~context:[ ("path", path) ]
           "refusing to collect an invalid run reservation marker")
  | Error message ->
      Error
        (Error.create ~phase:Error.Cache ~cause:Error.Io_failure
           ~context:[ ("path", path) ]
           "cannot verify stale run reservation marker: %s" message)

let gc (store : t) ~older_than_days =
  if older_than_days < 0 then
    Error
      (Error.create ~phase:Error.Cache ~cause:Error.Invalid_input
         "cache gc age must be non-negative")
  else
    with_maintenance_lease store (fun () ->
        let cutoff =
          Unix.gettimeofday () -. (float_of_int older_than_days *. 86400.)
        in
        let roots =
          List.map (store_path store)
            [
              workspace_scopes_path;
              legacy_outcome_path;
              legacy_outcomes_v2_path;
              legacy_runs_path;
            ]
        in
        try
          let stale =
            List.concat_map
              (fun root ->
                let native_root = windows_extended_path root in
                if Sys.file_exists native_root then
                  files_recursive root
                  |> List.filter_map (fun relative ->
                      let path = Filename.concat root relative in
                      if
                        (Unix.stat (windows_extended_path path)).st_mtime
                        < cutoff
                      then Some path
                      else None)
                else [])
              roots
            |> List.sort_uniq String.compare
          in
          let rec validate = function
            | [] -> Ok ()
            | path :: rest when Filename.check_suffix path ".reserved" ->
                let* () = validate_stale_reservation path in
                validate rest
            | _ :: rest -> validate rest
          in
          let* () = validate stale in
          let rec remove removed = function
            | [] -> Ok removed
            | path :: rest -> (
                try
                  Sys.remove (windows_extended_path path);
                  remove (removed + 1) rest
                with Sys_error message ->
                  Error
                    (Error.create ~phase:Error.Cache ~cause:Error.Io_failure
                       ~context:[ ("path", path) ]
                       "cache gc could not remove stale entry: %s" message))
          in
          remove 0 stale
        with
        | Sys_error message ->
            Error
              (Error.create ~phase:Error.Cache ~cause:Error.Io_failure
                 "cache gc failed: %s" message)
        | Unix.Unix_error (error, operation, path) ->
            Error
              (Error.create ~phase:Error.Cache ~cause:Error.Io_failure
                 ~context:[ ("operation", operation); ("path", path) ]
                 "cache gc failed: %s" (Unix.error_message error)))

let clean (store : t) =
  with_maintenance_lease store (fun () ->
      match read_file (store_path store marker_path) with
      | Ok value when String.equal value marker_contents ->
          let rec remove = function
            | [] -> Ok ()
            | relative :: rest -> (
                let path = store_path store relative in
                if not (Sys.file_exists (windows_extended_path path)) then
                  remove rest
                else
                  match remove_tree path with
                  | Ok () -> remove rest
                  | Error message ->
                      Error
                        (Error.create ~phase:Error.Cache ~cause:Error.Io_failure
                           "cache clean failed: %s" message))
          in
          let* () =
            remove
              [
                workspace_scopes_path;
                legacy_outcome_path;
                legacy_outcomes_v2_path;
                legacy_runs_path;
              ]
          in
          (try
             Sys.remove
               (store_path store root_latest_path |> windows_extended_path)
           with Sys_error _ -> ());
          Ok ()
      | _ ->
          Error
            (Error.create ~phase:Error.Cache ~cause:Error.Workspace_violation
               "refusing to clean an unmarked cache directory: %s" store.root))
