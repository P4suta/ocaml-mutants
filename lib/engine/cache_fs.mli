(** Capability-oriented filesystem contract for the cache store.

    Portable names are used only for entries created by the cache. Native names
    returned by enumeration remain opaque and are never parsed back into
    portable or ambient-path authority. *)

module Name : sig
  type t

  type error =
    | Empty
    | Dot_component
    | Trailing_dot
    | Trailing_space
    | Reserved_device
    | Unsafe_character of char

  val of_string : string -> (t, error) result
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val pp_error : Format.formatter -> error -> unit
end

module Relative : sig
  type t
  type error = { component : string; reason : Name.error }

  val root : t
  val child : t -> Name.t -> t
  val of_strings : string list -> (t, error) result
  val components : t -> Name.t list
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

type operation =
  | Acquire
  | Close_root
  | Retry_cleanup
  | Mkdirs
  | Read_regular
  | Capture_regular
  | Close_captured
  | Unlink_captured
  | Inspect_kind
  | Inspect_path
  | Inspect_listed
  | Close_inspection
  | Create_exclusive
  | Replace_file
  | Stage_file
  | Capture_stage_for_publish
  | Close_stage
  | Publish_no_replace
  | Discard_stage
  | List_directory
  | Close_listing
  | Remove_tree
  | Validate_deletion_authority
  | Try_lock
  | Release_lock

val operation_name : operation -> string

type error_class =
  | Missing
  | Already_exists
  | Busy
  | Link_like
  | Not_directory
  | Not_regular
  | Too_large
  | Budget_exhausted
  | Access_denied
  | Unsupported
  | Invalid_name
  | Unsafe_relationship
  | Identity_changed
  | Closed_capability
  | Wrong_process
  | Backend_contract_violation
  | Other

type native_domain = Posix_errno | Win32 | Ntstatus | In_memory | Contract

type native_error = {
  operation : operation;
  primitive_operation : Dir_cap.operation option;
  class_ : error_class;
  native_domain : native_domain;
  native_code : string;
  component : string option;
}

val make_error :
  operation:operation ->
  ?primitive_operation:Dir_cap.operation ->
  class_:error_class ->
  native_domain:native_domain ->
  native_code:string ->
  ?component:string ->
  unit ->
  native_error

type 'retry local_handle_progress =
  | Handle_closed
  | Handle_invalidated_unknown
  | Handle_still_open of 'retry

type 'retry cleanup_problem = {
  error : native_error;
  local_handle : 'retry local_handle_progress;
  namespace_released : bool;
}

type 'retry cleanup_failure = {
  primary : 'retry cleanup_problem;
  suppressed : 'retry cleanup_problem list;
}

type 'retry issue =
  | Operation_error of native_error
  | Cleanup_error of 'retry cleanup_failure

type 'retry failure = { primary : 'retry issue; suppressed : 'retry issue list }
(** Issues preserve execution order. Callers must inspect the primary and every
    suppressed cleanup problem in both failures and successful advisories. Each
    [Handle_still_open retry] retains the exact capability and must be passed to
    [retry_cleanup]; [Handle_closed] and [Handle_invalidated_unknown] are
    terminal. Several problems may intentionally carry the same retry token for
    one multi-handle capability. *)

val issue_error : 'retry issue -> native_error

type ('resource, 'retry) teardown_outcome =
  | Teardown_complete
  | Teardown_local_only
  | Teardown_incomplete of { live : 'resource option; failure : 'retry failure }
      (** [live] is [Some resource] exactly when at least one handle owned by
          [resource] is still open and teardown can be retried. [None] is a
          terminal [Closed] or [Invalidated_unknown] state. *)

type entry_kind = Regular | Directory | Link_like_entry | Other_entry
type 'a read_result = Contents of 'a | Read_missing
type lock_mode = Shared | Exclusive

type conditional_unlink_disposition =
  | Unlinked
  | Unlink_missing
  | Identity_changed_entry

(** Traversal has no implicit or protocol-specific default. The inspected target
    is depth zero, and [max_depth] permits entries whose depth is less than or
    equal to that value. Before an entry deeper than the limit is probed,
    opened, or mutated, traversal stops with [Budget_exhausted]. Thus depth zero
    can delete an empty inspected directory or a non-directory target, but never
    one of that directory's children.

    Entry and native-name-byte budgets are charged by enumeration before any
    returned name is used. One budget is consumed globally across every
    recursive enumeration in the operation; the inspected target itself is not
    charged as an enumerated entry. Zero is a valid explicit bound. *)
module Traversal_budget : sig
  type t

  type error =
    | Negative_max_depth of int64
    | Negative_max_entries of int64
    | Negative_max_native_name_bytes of int64

  val create :
    max_depth:int64 ->
    max_entries:int64 ->
    max_native_name_bytes:int64 ->
    (t, error) result

  val max_depth : t -> int64
  val max_entries : t -> int64
  val max_native_name_bytes : t -> int64
end

type traversal_progress = {
  entries : int64;
  native_name_bytes : int64;
  removed : int64;
}

module type IDENTITY = sig
  type t

  val equal : t -> t -> bool
  val hash : t -> int
  val pp : Format.formatter -> t -> unit
end

module type NATIVE_NAME = sig
  type t

  val equal : t -> t -> bool

  val encode : t -> string
  (** Lossless diagnostic encoding of an enumerated native name. It is not a
      portable [Name.t] and must never be fed back into descendant I/O. *)

  val pp : Format.formatter -> t -> unit
end

module type DELETION_AUTHORITY_PROVIDER = sig
  type dir
  type identity
  type owner
  type t

  val establish :
    t ->
    root:dir ->
    identity:identity ->
    owner:owner ->
    (unit, Dir_cap.failure) result

  val validate_continuation :
    t ->
    root:dir ->
    identity:identity ->
    owner:owner ->
    (unit, Dir_cap.failure) result
  (** [establish] starts the deletion selection boundary and proves that [t]
      owns an exclusive namespace/GC lease bound to the exact live [root], its
      identity, and its owner. A successful provider must keep that same lease
      continuously active: it may not release and later reactivate [t].

      [validate_continuation] succeeds only while the lease established for [t]
      has remained continuously active and is still bound to the supplied live
      root. Both operations return structured failures, never a bool. They must
      not return a [Still_open] cleanup problem because the provider exposes no
      cleanup capability through this interface. *)
end

module type S = sig
  module Identity : IDENTITY
  module Native_name : NATIVE_NAME

  type owner
  type workspace_probe
  type deletion_authority
  type cleanup_retry
  type root
  type lock
  type staged_file
  type captured_regular
  type listing
  type listed_entry
  type inspected_path
  type inspected_entry
  type advisory = cleanup_retry issue
  type operation_failure = cleanup_retry failure

  type stat = {
    identity : Identity.t;
    kind : entry_kind;
    size : int64;
    mtime_ns : int64;
  }
  (** [identity] is diagnostic unless the value is obtained from an inspection
      or capture which keeps the exact target capability live. It must not be
      persisted as deletion authority. *)

  type captured_read = {
    contents : string;
    stat : stat;
    captured : captured_regular;
  }

  type captured_read_without_handle = { contents : string; stat : stat }

  type acquisition_observation =
    | Creation_observed of Native_name.t
    | Creation_may_have_committed of Native_name.t

  type acquisition = {
    root : root;
    display_path : string;
    workspace_display_path : string;
    created : acquisition_observation list;
    advisories : advisory list;
  }

  type acquisition_outcome =
    | Acquired of acquisition
    | Acquisition_incomplete of {
        created : acquisition_observation list;
        failure : operation_failure;
      }
        (** Creation observations are ordered audit evidence only. They neither
            own the named entry nor authorize rollback. *)

  type directory_commit_observation =
    | Directory_creation_observed of { path : Relative.t }
    | Directory_creation_may_have_committed of { path : Relative.t }

  type mkdirs_outcome =
    | Directories_ready of {
        created : directory_commit_observation list;
        advisories : advisory list;
      }
    | Directories_incomplete of {
        created : directory_commit_observation list;
        failure : operation_failure;
      }

  type residual_observation =
    | Residual_creation_observed of { name : Native_name.t }
    | Residual_creation_may_have_committed of { name : Native_name.t }
        (** Residual observations are audit data, never cleanup capabilities. *)

  type create_outcome =
    | File_not_created of operation_failure
    | File_exists of { advisories : advisory list }
    | File_created of { advisories : advisory list }
    | File_creation_incomplete of {
        live_file : staged_file option;
        audit_only : residual_observation list;
        failure : operation_failure;
      }

  type stage_outcome =
    | Staged of staged_file
    | Staging_not_created of operation_failure
    | Staging_incomplete_actionable of {
        live_stage : staged_file;
        failure : operation_failure;
      }
    | Staging_incomplete_audit_only of {
        residual : residual_observation;
        failure : operation_failure;
      }

  type replacement_outcome =
    | Replacement_not_published of {
        live_stage : staged_file option;
        audit_only : residual_observation list;
        failure : operation_failure;
      }
    | Replaced of { advisories : advisory list }

  type publish_outcome =
    | Stage_publish_rejected of operation_failure
    | Stage_not_published of {
        live_stage : staged_file;
        failure : operation_failure;
      }
    | Stage_published of { advisories : advisory list }
        (** [Stage_not_published] proves the destination was not committed and
            explicitly returns the still-live input stage. *)

  type stage_discard_outcome =
    | Stage_discarded of { advisories : advisory list }
    | Stage_discard_local_only of { advisories : advisory list }
    | Stage_discard_retained of {
        live_stage : staged_file;
        failure : operation_failure;
      }
    | Stage_discard_incomplete_audit_only of {
        residual : residual_observation;
        failure : operation_failure;
      }

  type captured_unlink_outcome =
    | Captured_unlink_rejected of operation_failure
    | Captured_unlink_complete of {
        disposition : conditional_unlink_disposition;
        advisories : advisory list;
      }
    | Captured_unlink_retained of {
        live_capture : captured_regular;
        failure : operation_failure;
      }

  type listing_outcome =
    | Listing_complete of { listing : listing; progress : traversal_progress }
    | Listing_incomplete of {
        progress : traversal_progress;
        failure : operation_failure;
      }
        (** Partial names returned on enumeration exhaustion are never placed in
            a [listing] and therefore never become authority. *)

  type 'inspection removal_outcome =
    | Removal_complete of {
        progress : traversal_progress;
        advisories : advisory list;
      }
    | Removal_incomplete of {
        progress : traversal_progress;
        live_inspection : 'inspection option;
        failure : operation_failure;
      }

  type lock_acquisition = {
    disposition : [ `Acquired of lock | `Busy ];
    advisories : advisory list;
  }

  val acquire :
    workspace:workspace_probe -> requested:string -> acquisition_outcome
  (** [workspace] is the already-captured workspace probe and is consumed. The
      candidate probe and workspace probe must be moved into an unforgeable
      backend separation witness; only that witness may be materialized. *)

  val root_identity : root -> Identity.t
  val root_owner : root -> owner
  val owner_equal : owner -> owner -> bool
  val close_root : root -> (root, cleanup_retry) teardown_outcome

  val retry_cleanup :
    cleanup_retry -> (cleanup_retry, cleanup_retry) teardown_outcome

  val mkdirs : root -> Relative.t -> mkdirs_outcome

  val read_regular :
    root ->
    Relative.t ->
    limit:int64 ->
    (captured_read_without_handle read_result, operation_failure) result
  (** Ordinary read-and-close. Its identity field is diagnostic only. *)

  val capture_regular :
    root ->
    Relative.t ->
    limit:int64 ->
    (captured_read read_result, operation_failure) result
  (** Captures the exact regular-file handle and parent capability required by
      [unlink_captured], then reads through that same handle. Backends without
      native captured-file deletion authority fail closed; ordinary
      [read_regular] does not require that authority. *)

  val close_captured :
    captured_regular -> (captured_regular, cleanup_retry) teardown_outcome

  val unlink_captured : captured_regular -> captured_unlink_outcome
  (** Deletes only through the captured parent and file capabilities; it never
      falls back to a name-based unlink. A pre-commit refusal returns the same
      live capture. A committed deletion consumes the capture and reports any
      ordered post-commit/cleanup issues as advisories. An incomplete deletion
      preserves native local-handle and namespace-release evidence; a
      [Handle_still_open] problem retains the exact retry capability. *)

  val inspect_kind :
    root -> Relative.t -> (entry_kind option, operation_failure) result

  val create_exclusive : root -> Relative.t -> contents:string -> create_outcome

  val replace_file :
    root -> Relative.t -> contents:string -> replacement_outcome

  val stage_file : root -> Relative.t -> contents:string -> stage_outcome

  val capture_stage_for_publish :
    root ->
    Relative.t ->
    expected_contents:string ->
    (staged_file, operation_failure) result
  (** Captures an already-created stage through a backend publication handle,
      then reads that exact immutable handle and requires byte-for-byte equality
      with [expected_contents]. It never creates or mutates a named entry. *)

  val close_stage : staged_file -> (staged_file, cleanup_retry) teardown_outcome
  (** Releases every local capability retained by an unpublished stage without
      unlinking its source name. This is the deterministic teardown for a
      one-shot publication failure; the residual name remains audit/GC input,
      never implicit cleanup authority. *)

  val staged_owner : staged_file -> owner

  val publish_no_replace : staged_file -> target:Relative.t -> publish_outcome
  (** Performs one backend atomic no-replace commit from the captured stage; it
      never probes target existence first. [Stage_not_published] preserves the
      native commit failure as primary and returns the exact still-live stage.
      [Stage_published] means the destination committed; every post-commit or
      handle-cleanup problem is retained in execution order as an advisory. *)

  val discard_stage : staged_file -> stage_discard_outcome
  (** Uses the created stage's exact parent and file capabilities. It never
      re-resolves [source_name] for deletion. Pre-commit and retryable failures
      retain the stage; terminal failures with no proven namespace release
      retain an audit residual, while proven committed deletion is discarded
      with ordered advisories. *)

  val list : root -> Relative.t -> budget:Traversal_budget.t -> listing_outcome
  val listed_entries : listing -> (listed_entry list, operation_failure) result

  val listed_name : listed_entry -> Native_name.t
  (** A listing preserves native names losslessly but is not deletion authority.
      [inspect_listed] establishes the continuous deletion lease and freshly
      captures the object then bound to the name. Stale predicates must be
      derived from that inspected witness, never from earlier listing data. *)

  val inspect_path :
    deletion_authority ->
    root ->
    Relative.t ->
    (inspected_path read_result, operation_failure) result

  val inspect_listed :
    deletion_authority ->
    listed_entry ->
    (inspected_entry read_result, operation_failure) result

  val inspected_path_stat : inspected_path -> stat
  val inspected_entry_stat : inspected_entry -> stat
  val inspected_entry_name : inspected_entry -> Native_name.t

  val close_inspected_path :
    inspected_path -> (inspected_path, cleanup_retry) teardown_outcome

  val close_inspected_entry :
    inspected_entry -> (inspected_entry, cleanup_retry) teardown_outcome

  val remove_tree :
    inspected_path ->
    budget:Traversal_budget.t ->
    inspected_path removal_outcome

  val remove_inspected_tree :
    inspected_entry ->
    budget:Traversal_budget.t ->
    inspected_entry removal_outcome
  (** An inspected witness borrows and retains the exact authority token passed
      to its inspection. Removal accepts no substitute token: it validates that
      the stored lease has remained continuously active, then rechecks the
      captured parent binding against the exact live target or the
      lease-protected atomic binding. A replacement is refused with
      [Identity_changed] and is never traversed or removed. Closing an inspected
      witness does not release the caller-owned lease. *)

  val close_listing : listing -> (listing, cleanup_retry) teardown_outcome

  val try_lock :
    root ->
    Relative.t ->
    lock_mode ->
    (lock_acquisition, operation_failure) result

  val lock_owner : lock -> owner
  val release_lock : lock -> (lock, cleanup_retry) teardown_outcome
end

(** No deletion-authority provider for [Dir_cap.System] is supplied.
    Instantiating this functor with a test/fake provider is contract validation,
    not production wiring; the native backend remains blocked by the gaps
    documented in [Dir_cap.System]. *)
module Make
    (Backend : Dir_cap.S)
    (Deletion_authority :
      DELETION_AUTHORITY_PROVIDER
        with type dir = Backend.dir
         and type identity = Backend.Identity.t
         and type owner = Backend.owner) :
  S
    with type Identity.t = Backend.Identity.t
     and type Native_name.t = Backend.Native_name.t
     and type owner = Backend.owner
     and type workspace_probe = Backend.path_probe
     and type deletion_authority = Deletion_authority.t
