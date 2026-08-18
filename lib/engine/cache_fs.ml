module Name = struct
  type t = string

  type error =
    | Empty
    | Dot_component
    | Trailing_dot
    | Trailing_space
    | Reserved_device
    | Unsafe_character of char

  let portable_character = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '.' | '-' | '_' -> true
    | _ -> false

  let windows_device_basename value =
    let basename =
      match String.index_opt value '.' with
      | None -> value
      | Some index -> String.sub value 0 index
    in
    let basename = String.uppercase_ascii basename in
    String.equal basename "CON"
    || String.equal basename "PRN"
    || String.equal basename "AUX"
    || String.equal basename "NUL"
    ||
    let numbered prefix =
      String.length basename = 4
      && String.starts_with ~prefix basename
      && match basename.[3] with '1' .. '9' -> true | _ -> false
    in
    numbered "COM" || numbered "LPT"

  let of_string value =
    if String.equal value "" then Error Empty
    else if String.equal value "." || String.equal value ".." then
      Error Dot_component
    else if value.[String.length value - 1] = '.' then Error Trailing_dot
    else if value.[String.length value - 1] = ' ' then Error Trailing_space
    else if windows_device_basename value then Error Reserved_device
    else
      let rec first_unsafe index =
        if index = String.length value then None
        else if portable_character value.[index] then first_unsafe (index + 1)
        else Some value.[index]
      in
      match first_unsafe 0 with
      | Some character -> Error (Unsafe_character character)
      | None -> Ok value

  let to_string value = value
  let pp = Format.pp_print_string

  let pp_error formatter = function
    | Empty -> Format.pp_print_string formatter "component is empty"
    | Dot_component ->
        Format.pp_print_string formatter "component is dot or dot-dot"
    | Trailing_dot -> Format.pp_print_string formatter "component ends in a dot"
    | Trailing_space ->
        Format.pp_print_string formatter "component ends in a space"
    | Reserved_device ->
        Format.pp_print_string formatter
          "component uses a reserved device basename"
    | Unsafe_character character ->
        Format.fprintf formatter "component contains unsafe character %C"
          character
end

module Relative = struct
  type t = Name.t list
  type error = { component : string; reason : Name.error }

  let root = []
  let child path name = path @ [ name ]

  let of_strings values =
    let rec convert converted = function
      | [] -> Ok (List.rev converted)
      | component :: rest -> (
          match Name.of_string component with
          | Ok name -> convert (name :: converted) rest
          | Error reason -> Error { component; reason })
    in
    convert [] values

  let components value = value
  let equal left right = left = right

  let pp formatter value =
    match value with
    | [] -> Format.pp_print_char formatter '.'
    | _ ->
        Format.pp_print_string formatter
          (String.concat "/" (List.map Name.to_string value))
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

let operation_name = function
  | Acquire -> "acquire"
  | Close_root -> "close-root"
  | Retry_cleanup -> "retry-cleanup"
  | Mkdirs -> "mkdirs"
  | Read_regular -> "read-regular"
  | Capture_regular -> "capture-regular"
  | Close_captured -> "close-captured"
  | Unlink_captured -> "unlink-captured"
  | Inspect_kind -> "inspect-kind"
  | Inspect_path -> "inspect-path"
  | Inspect_listed -> "inspect-listed"
  | Close_inspection -> "close-inspection"
  | Create_exclusive -> "create-exclusive"
  | Replace_file -> "replace-file"
  | Stage_file -> "stage-file"
  | Capture_stage_for_publish -> "capture-stage-for-publish"
  | Close_stage -> "close-stage"
  | Publish_no_replace -> "publish-no-replace"
  | Discard_stage -> "discard-stage"
  | List_directory -> "list-directory"
  | Close_listing -> "close-listing"
  | Remove_tree -> "remove-tree"
  | Validate_deletion_authority -> "validate-deletion-authority"
  | Try_lock -> "try-lock"
  | Release_lock -> "release-lock"

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

let make_error ~operation ?primitive_operation ~class_ ~native_domain
    ~native_code ?component () =
  {
    operation;
    primitive_operation;
    class_;
    native_domain;
    native_code;
    component;
  }

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

let issue_error = function
  | Operation_error error -> error
  | Cleanup_error failure -> failure.primary.error

type ('resource, 'retry) teardown_outcome =
  | Teardown_complete
  | Teardown_local_only
  | Teardown_incomplete of { live : 'resource option; failure : 'retry failure }

type entry_kind = Regular | Directory | Link_like_entry | Other_entry
type 'a read_result = Contents of 'a | Read_missing
type lock_mode = Shared | Exclusive

type conditional_unlink_disposition =
  | Unlinked
  | Unlink_missing
  | Identity_changed_entry

module Traversal_budget = struct
  type t = {
    max_depth : int64;
    max_entries : int64;
    max_native_name_bytes : int64;
  }

  type error =
    | Negative_max_depth of int64
    | Negative_max_entries of int64
    | Negative_max_native_name_bytes of int64

  let create ~max_depth ~max_entries ~max_native_name_bytes =
    if Int64.compare max_depth 0L < 0 then Error (Negative_max_depth max_depth)
    else if Int64.compare max_entries 0L < 0 then
      Error (Negative_max_entries max_entries)
    else if Int64.compare max_native_name_bytes 0L < 0 then
      Error (Negative_max_native_name_bytes max_native_name_bytes)
    else Ok { max_depth; max_entries; max_native_name_bytes }

  let max_depth value = value.max_depth
  let max_entries value = value.max_entries
  let max_native_name_bytes value = value.max_native_name_bytes
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

  val capture_regular :
    root ->
    Relative.t ->
    limit:int64 ->
    (captured_read read_result, operation_failure) result

  val close_captured :
    captured_regular -> (captured_regular, cleanup_retry) teardown_outcome

  val unlink_captured : captured_regular -> captured_unlink_outcome

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

  val close_stage : staged_file -> (staged_file, cleanup_retry) teardown_outcome
  val staged_owner : staged_file -> owner
  val publish_no_replace : staged_file -> target:Relative.t -> publish_outcome
  val discard_stage : staged_file -> stage_discard_outcome
  val list : root -> Relative.t -> budget:Traversal_budget.t -> listing_outcome
  val listed_entries : listing -> (listed_entry list, operation_failure) result
  val listed_name : listed_entry -> Native_name.t

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

  val close_listing : listing -> (listing, cleanup_retry) teardown_outcome

  val try_lock :
    root ->
    Relative.t ->
    lock_mode ->
    (lock_acquisition, operation_failure) result

  val lock_owner : lock -> owner
  val release_lock : lock -> (lock, cleanup_retry) teardown_outcome
end

module Make
    (Backend : Dir_cap.S)
    (Deletion_authority :
      DELETION_AUTHORITY_PROVIDER
        with type dir = Backend.dir
         and type identity = Backend.Identity.t
         and type owner = Backend.owner) =
struct
  let ( let* ) value continuation = Result.bind value continuation

  module Identity = struct
    type t = Backend.Identity.t

    let equal = Backend.Identity.equal
    let hash value = Hashtbl.hash (Backend.Identity.encode value)
    let pp = Backend.Identity.pp
  end

  module Native_name = Backend.Native_name

  type owner = Backend.owner
  type workspace_probe = Backend.path_probe
  type deletion_authority = Deletion_authority.t
  type held_state = Held_open | Held_closed | Held_invalidated_unknown
  type 'cap held = { capability : 'cap; mutable held_state : held_state }

  type stat = {
    identity : Identity.t;
    kind : entry_kind;
    size : int64;
    mtime_ns : int64;
  }

  type root = {
    directory : Backend.dir held;
    identity : Identity.t;
    owner : owner;
  }

  type staged_state = Stage_open | Stage_published | Stage_discarded

  type staged_file = {
    authority_root : Backend.dir held;
    parent : Backend.dir held;
    source_name : Backend.Native_name.t;
    file : Backend.file held;
    identity : Identity.t;
    owner : owner;
    source_released : bool ref;
    mutable staged_state : staged_state;
  }

  type captured_state = Capture_open | Capture_consumed

  type captured_regular = {
    parent : Backend.dir held;
    name : Backend.Native_name.t;
    file : Backend.file held;
    identity : Identity.t;
    owner : owner;
    namespace_released : bool ref;
    mutable captured_state : captured_state;
  }

  type listing = {
    authority_root : Backend.dir held;
    directory : Backend.dir held;
    owner : owner;
    mutable listing_closed : bool;
    mutable entries : listed_entry list;
  }

  and listed_entry = { listing : listing; name : Backend.Native_name.t }

  type captured_target =
    | Captured_directory of Backend.dir held
    | Captured_file of Backend.file held
    | Lease_protected_atomic_binding of Dir_cap.entry_kind

  type inspection_core = {
    authority_root : Backend.dir held;
    parent : Backend.dir held;
    name : Backend.Native_name.t;
    target : captured_target;
    stat : stat;
    root_identity : Identity.t;
    root_owner : owner;
    owner : owner;
    deletion_authority : Deletion_authority.t;
    mutable inspection_consumed : bool;
  }

  and inspected_path = Inspected_path of inspection_core
  and inspected_entry = Inspected_entry of inspection_core

  type lock = { backend_lock : Backend.lock held; owner : owner }

  type cleanup_target =
    | Cleanup_probe of Backend.path_probe held
    | Cleanup_separation of Backend.separation_witness held
    | Cleanup_directory of Backend.dir held
    | Cleanup_file of Backend.file held
    | Cleanup_deleting_file of {
        file : Backend.file held;
        namespace_released : bool ref;
      }
    | Cleanup_lock of Backend.lock held

  and cleanup_retry = {
    cleanup_operation : operation;
    cleanup_target : cleanup_target;
  }

  type advisory = cleanup_retry issue
  type operation_failure = cleanup_retry failure

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

  let held capability = { capability; held_state = Held_open }
  let is_open held = held.held_state = Held_open

  let error_class_of_backend = function
    | Dir_cap.Missing -> Missing
    | Dir_cap.Already_exists -> Already_exists
    | Dir_cap.Not_directory -> Not_directory
    | Dir_cap.Not_regular -> Not_regular
    | Dir_cap.Not_link -> Link_like
    | Dir_cap.Link_like -> Link_like
    | Dir_cap.Too_large -> Too_large
    | Dir_cap.Invalid_name -> Invalid_name
    | Dir_cap.Access_denied -> Access_denied
    | Dir_cap.Busy -> Busy
    | Dir_cap.Unsupported -> Unsupported
    | Dir_cap.Wrong_process -> Wrong_process
    | Dir_cap.Closed_capability -> Closed_capability
    | Dir_cap.Other -> Other

  let native_domain_of_backend = function
    | Dir_cap.Posix_errno -> Posix_errno
    | Dir_cap.Win32 -> Win32
    | Dir_cap.Ntstatus -> Ntstatus
    | Dir_cap.In_memory -> In_memory
    | Dir_cap.Contract -> Contract

  let map_error operation (error : Dir_cap.error) =
    {
      operation;
      primitive_operation = Some error.operation;
      class_ = error_class_of_backend error.class_;
      native_domain = native_domain_of_backend error.native_domain;
      native_code = error.native_code;
      component = error.component;
    }

  let contract_error ?component operation class_ native_code =
    make_error ~operation ~class_ ~native_domain:Contract ~native_code
      ?component ()

  let operation_failure error =
    { primary = Operation_error error; suppressed = [] }

  let failure_issues issues =
    match issues with
    | primary :: suppressed -> { primary; suppressed }
    | [] -> invalid_arg "Cache_fs.failure_issues: empty issue list"

  let append_issues failure issues =
    { failure with suppressed = failure.suppressed @ issues }

  let cleanup_target_state = function
    | Cleanup_probe held -> held.held_state
    | Cleanup_separation held -> held.held_state
    | Cleanup_directory held -> held.held_state
    | Cleanup_file held -> held.held_state
    | Cleanup_deleting_file { file; _ } -> file.held_state
    | Cleanup_lock held -> held.held_state

  let set_cleanup_target_state target state =
    match target with
    | Cleanup_probe held -> held.held_state <- state
    | Cleanup_separation held -> held.held_state <- state
    | Cleanup_directory held -> held.held_state <- state
    | Cleanup_file held -> held.held_state <- state
    | Cleanup_deleting_file { file; _ } -> file.held_state <- state
    | Cleanup_lock held -> held.held_state <- state

  let cleanup_state_join left right =
    match (left, right) with
    | Some Held_open, _ | _, Some Held_open -> Some Held_open
    | Some Held_invalidated_unknown, _ | _, Some Held_invalidated_unknown ->
        Some Held_invalidated_unknown
    | Some Held_closed, _ | _, Some Held_closed -> Some Held_closed
    | None, None -> None

  let cleanup_state_of_problem (problem : Dir_cap.cleanup_problem) =
    match problem.local_handle_state with
    | Dir_cap.Still_open -> Held_open
    | Dir_cap.Invalidated_unknown -> Held_invalidated_unknown
    | Dir_cap.Closed -> Held_closed

  let cleanup_state_of_failure (failure : Dir_cap.cleanup_failure) =
    List.fold_left
      (fun state problem ->
        cleanup_state_join state (Some (cleanup_state_of_problem problem)))
      (Some (cleanup_state_of_problem failure.primary))
      failure.suppressed

  let cleanup_failure_still_open (failure : Dir_cap.cleanup_failure) =
    cleanup_state_of_failure failure = Some Held_open

  let cleanup_state_of_issue = function
    | Dir_cap.Operation_error _ -> None
    | Dir_cap.Cleanup_error failure -> cleanup_state_of_failure failure

  let cleanup_state_of_issues issues =
    List.fold_left
      (fun state issue ->
        cleanup_state_join state (cleanup_state_of_issue issue))
      None issues

  let prepare_backend_issues ?retry issues =
    match (cleanup_state_of_issues issues, retry) with
    | Some Held_open, None ->
        invalid_arg
          "Dir_cap contract violation: Still_open without its exact \
           caller-visible capability"
    | Some state, Some retry ->
        set_cleanup_target_state retry.cleanup_target state
    | None, Some _ -> ()
    | (None | Some (Held_closed | Held_invalidated_unknown)), None -> ()

  let progress_of_backend retry = function
    | Dir_cap.Closed -> Handle_closed
    | Dir_cap.Invalidated_unknown -> Handle_invalidated_unknown
    | Dir_cap.Still_open -> (
        match retry with
        | Some retry -> Handle_still_open retry
        | None ->
            invalid_arg
              "Dir_cap contract violation: Still_open without its exact \
               caller-visible capability")

  let map_cleanup_problem operation retry (problem : Dir_cap.cleanup_problem) =
    {
      error = map_error operation problem.error;
      local_handle = progress_of_backend retry problem.local_handle_state;
      namespace_released = problem.namespace_released;
    }

  let map_cleanup_failure operation retry (failure : Dir_cap.cleanup_failure) =
    ({
       primary = map_cleanup_problem operation retry failure.primary;
       suppressed =
         List.map (map_cleanup_problem operation retry) failure.suppressed;
     }
      : cleanup_retry cleanup_failure)

  let map_backend_issue_unchecked operation retry = function
    | Dir_cap.Operation_error error ->
        Operation_error (map_error operation error)
    | Dir_cap.Cleanup_error failure ->
        Cleanup_error (map_cleanup_failure operation retry failure)

  let map_backend_issues operation ?retry issues =
    prepare_backend_issues ?retry issues;
    List.map (map_backend_issue_unchecked operation retry) issues

  let map_backend_failure operation ?retry (failure : Dir_cap.failure) =
    match
      map_backend_issues operation ?retry (failure.primary :: failure.suppressed)
    with
    | primary :: suppressed -> { primary; suppressed }
    | [] -> assert false

  let reclassify_enumeration_budget_issue = function
    | Operation_error ({ class_ = Too_large; _ } as error) ->
        Operation_error { error with class_ = Budget_exhausted }
    | (Operation_error _ | Cleanup_error _) as issue -> issue

  let map_enumeration_failure operation failure =
    let mapped = map_backend_failure operation failure in
    {
      primary = reclassify_enumeration_budget_issue mapped.primary;
      suppressed =
        List.map reclassify_enumeration_budget_issue mapped.suppressed;
    }

  type cleanup_summary = { issues : advisory list; local_only : bool }

  let no_cleanup = { issues = []; local_only = false }

  let run_cleanup operation target close =
    if cleanup_target_state target <> Held_open then no_cleanup
    else
      match close () with
      | Dir_cap.Cleanup_complete ->
          set_cleanup_target_state target Held_closed;
          no_cleanup
      | Dir_cap.Cleanup_local_only ->
          set_cleanup_target_state target Held_closed;
          { issues = []; local_only = true }
      | Dir_cap.Cleanup_failed failure ->
          let retry =
            { cleanup_operation = operation; cleanup_target = target }
          in
          {
            issues =
              map_backend_issues operation ~retry
                [ Dir_cap.Cleanup_error failure ];
            local_only = false;
          }

  let cleanup_probe operation probe =
    run_cleanup operation (Cleanup_probe probe) (fun () ->
        Backend.close_probe probe.capability)

  let cleanup_separation operation witness =
    run_cleanup operation (Cleanup_separation witness) (fun () ->
        Backend.close_separation witness.capability)

  let cleanup_directory operation directory =
    run_cleanup operation (Cleanup_directory directory) (fun () ->
        Backend.close_directory directory.capability)

  let cleanup_file operation file =
    run_cleanup operation (Cleanup_file file) (fun () ->
        Backend.close_file file.capability)

  let cleanup_failure_namespace_released (failure : Dir_cap.cleanup_failure) =
    let released (problem : Dir_cap.cleanup_problem) =
      problem.namespace_released
    in
    released failure.primary || List.exists released failure.suppressed

  let cleanup_deleting_file operation file namespace_released =
    let target = Cleanup_deleting_file { file; namespace_released } in
    if cleanup_target_state target <> Held_open then no_cleanup
    else
      match Backend.close_file file.capability with
      | Dir_cap.Cleanup_complete ->
          set_cleanup_target_state target Held_closed;
          namespace_released := true;
          no_cleanup
      | Dir_cap.Cleanup_local_only ->
          set_cleanup_target_state target Held_closed;
          { issues = []; local_only = true }
      | Dir_cap.Cleanup_failed failure ->
          if cleanup_failure_namespace_released failure then
            namespace_released := true;
          let retry =
            { cleanup_operation = operation; cleanup_target = target }
          in
          {
            issues =
              map_backend_issues operation ~retry
                [ Dir_cap.Cleanup_error failure ];
            local_only = false;
          }

  let cleanup_lock operation lock =
    run_cleanup operation (Cleanup_lock lock) (fun () ->
        Backend.release_lock lock.capability)

  let combine_cleanup summaries =
    {
      issues = List.concat_map (fun summary -> summary.issues) summaries;
      local_only = List.exists (fun summary -> summary.local_only) summaries;
    }

  let finish action cleanup =
    match (action, cleanup.issues) with
    | Ok value, [] -> Ok value
    | Error failure, issues -> Error (append_issues failure issues)
    | Ok _, issues -> Error (failure_issues issues)

  let teardown resource ~live cleanup =
    match cleanup.issues with
    | [] ->
        if cleanup.local_only then Teardown_local_only else Teardown_complete
    | issues ->
        Teardown_incomplete
          {
            live = (if live () then Some resource else None);
            failure = failure_issues issues;
          }

  let retry_cleanup retry =
    let cleanup =
      match retry.cleanup_target with
      | Cleanup_probe probe -> cleanup_probe Retry_cleanup probe
      | Cleanup_separation witness -> cleanup_separation Retry_cleanup witness
      | Cleanup_directory directory -> cleanup_directory Retry_cleanup directory
      | Cleanup_file file -> cleanup_file Retry_cleanup file
      | Cleanup_deleting_file { file; namespace_released } ->
          cleanup_deleting_file Retry_cleanup file namespace_released
      | Cleanup_lock lock -> cleanup_lock Retry_cleanup lock
    in
    teardown retry
      ~live:(fun () -> cleanup_target_state retry.cleanup_target = Held_open)
      cleanup

  let entry_kind_of_backend = function
    | Dir_cap.Regular -> Regular
    | Dir_cap.Directory -> Directory
    | Dir_cap.Symbolic_link -> Link_like_entry
    | Dir_cap.Other_entry -> Other_entry

  let stat_of_backend (stat : Backend.stat) =
    {
      identity = stat.identity;
      kind = entry_kind_of_backend stat.kind;
      size = stat.size;
      mtime_ns = stat.mtime_ns;
    }

  let owner_equal = Backend.owner_equal

  let require_open operation label held =
    match held.held_state with
    | Held_open -> Ok held.capability
    | Held_closed | Held_invalidated_unknown ->
        Error
          (operation_failure
             (contract_error operation Closed_capability (label ^ "-closed")))

  let require_owner operation label owner =
    if Backend.owner_equal owner (Backend.current_owner ()) then Ok ()
    else
      Error
        (operation_failure
           (contract_error operation Wrong_process (label ^ "-wrong-process")))

  let duplicate_directory operation directory =
    let* directory = require_open operation "directory" directory in
    match Backend.duplicate_directory directory with
    | Ok duplicate -> Ok (held duplicate)
    | Error failure -> Error (map_backend_failure operation failure)

  let name operation name =
    match Backend.name_of_component (Name.to_string name) with
    | Ok name -> Ok name
    | Error error -> Error (operation_failure (map_error operation error))

  let native_components operation relative =
    let rec convert converted = function
      | [] -> Ok (List.rev converted)
      | component :: rest ->
          let* component = name operation component in
          convert (component :: converted) rest
    in
    convert [] (Relative.components relative)

  let split_last operation relative =
    let* components = native_components operation relative in
    let rec split prefix = function
      | [] ->
          Error
            (operation_failure
               (contract_error operation Invalid_name "root-has-no-parent"))
      | [ name ] -> Ok (List.rev prefix, name)
      | name :: rest -> split (name :: prefix) rest
    in
    split [] components

  let rec resolve_directory_from operation current = function
    | [] -> Ok current
    | component :: rest -> (
        match Backend.open_directory_no_follow current.capability component with
        | Error failure ->
            Error
              (append_issues
                 (map_backend_failure operation failure)
                 (cleanup_directory operation current).issues)
        | Ok child ->
            let child = held child in
            let current_cleanup = cleanup_directory operation current in
            if current_cleanup.issues = [] then
              resolve_directory_from operation child rest
            else
              let child_cleanup = cleanup_directory operation child in
              Error
                (failure_issues (current_cleanup.issues @ child_cleanup.issues))
        )

  let resolve_directory_from_base operation base relative =
    let* components = native_components operation relative in
    let* duplicate = duplicate_directory operation base in
    resolve_directory_from operation duplicate components

  let resolve_parent_from_base operation base relative =
    let* parents, leaf = split_last operation relative in
    let* duplicate = duplicate_directory operation base in
    let* parent = resolve_directory_from operation duplicate parents in
    Ok (parent, leaf)

  let private_directory_permissions =
    match Dir_cap.Permissions.of_int 0o700 with
    | Ok permissions -> permissions
    | Error _ -> invalid_arg "private directory permissions are invalid"

  let private_file_permissions =
    match Dir_cap.Permissions.of_int 0o600 with
    | Ok permissions -> permissions
    | Error _ -> invalid_arg "private file permissions are invalid"

  let backend_issue_still_open = function
    | Dir_cap.Operation_error _ -> false
    | Dir_cap.Cleanup_error failure -> cleanup_failure_still_open failure

  let backend_issues_still_open primary suppressed =
    backend_issue_still_open primary
    || List.exists backend_issue_still_open suppressed

  let acquisition_observation_of_backend = function
    | Backend.Creation_observed name -> Creation_observed name
    | Backend.Creation_may_have_committed name ->
        Creation_may_have_committed name

  let acquisition_observations values =
    List.map acquisition_observation_of_backend values

  let acquire ~workspace ~requested =
    let workspace = held workspace in
    let workspace_display_path = Backend.probe_display workspace.capability in
    match Backend.probe_path requested with
    | Error failure ->
        let cleanup = cleanup_probe Acquire workspace in
        Acquisition_incomplete
          {
            created = [];
            failure =
              append_issues (map_backend_failure Acquire failure) cleanup.issues;
          }
    | Ok candidate_capability -> (
        let candidate = held candidate_capability in
        let display_path = Backend.probe_display candidate.capability in
        match
          Backend.establish_separation ~forbidden:workspace.capability
            ~candidate:candidate.capability
        with
        | Error failure ->
            let cleanup =
              combine_cleanup
                [
                  cleanup_probe Acquire candidate;
                  cleanup_probe Acquire workspace;
                ]
            in
            Acquisition_incomplete
              {
                created = [];
                failure =
                  append_issues
                    (map_backend_failure Acquire failure)
                    cleanup.issues;
              }
        | Ok witness_capability -> (
            workspace.held_state <- Held_closed;
            candidate.held_state <- Held_closed;
            let witness = held witness_capability in
            let retry =
              {
                cleanup_operation = Acquire;
                cleanup_target = Cleanup_separation witness;
              }
            in
            match
              Backend.materialize witness.capability
                ~permissions:private_directory_permissions
            with
            | Backend.Materialization_incomplete { created; failure } ->
                let mapped = map_backend_failure Acquire ~retry failure in
                if
                  (not
                     (backend_issues_still_open failure.primary
                        failure.suppressed))
                  && witness.held_state = Held_open
                then witness.held_state <- Held_closed;
                Acquisition_incomplete
                  {
                    created = acquisition_observations created;
                    failure = mapped;
                  }
            | Backend.Materialized materialized ->
                let advisories =
                  map_backend_issues Acquire ~retry materialized.advisories
                in
                if
                  (not
                     (List.exists backend_issue_still_open
                        materialized.advisories))
                  && witness.held_state = Held_open
                then witness.held_state <- Held_closed;
                let directory = held materialized.directory in
                let root =
                  {
                    directory;
                    identity = Backend.dir_identity directory.capability;
                    owner = Backend.dir_owner directory.capability;
                  }
                in
                Acquired
                  {
                    root;
                    display_path;
                    workspace_display_path;
                    created = acquisition_observations materialized.created;
                    advisories;
                  }))

  let root_identity (root : root) = root.identity
  let root_owner (root : root) = root.owner

  let close_root (root : root) =
    let cleanup = cleanup_directory Close_root root.directory in
    teardown root ~live:(fun () -> is_open root.directory) cleanup

  type directory_step =
    | Directory_opened of Backend.dir held
    | Directory_created of Backend.dir held
    | Directory_incomplete of
        directory_commit_observation * operation_failure * cleanup_summary
    | Directory_failed of operation_failure

  let opened_existing_directory operation parent component expected =
    match Backend.open_directory_no_follow parent.capability component with
    | Error failure -> Directory_failed (map_backend_failure operation failure)
    | Ok child_capability ->
        let child = held child_capability in
        if
          Backend.Identity.equal expected
            (Backend.dir_identity child.capability)
        then Directory_opened child
        else
          let cleanup = cleanup_directory operation child in
          Directory_failed
            (append_issues
               (operation_failure
                  (contract_error
                     ~component:(Backend.Native_name.encode component)
                     operation Identity_changed "directory-identity-changed"))
               cleanup.issues)

  let open_or_create_directory operation path parent component =
    match Backend.probe_entry_no_follow parent.capability component with
    | Error failure -> Directory_failed (map_backend_failure operation failure)
    | Ok (Some stat) -> (
        match stat.kind with
        | Dir_cap.Directory ->
            opened_existing_directory operation parent component stat.identity
        | Dir_cap.Symbolic_link ->
            Directory_failed
              (operation_failure
                 (contract_error
                    ~component:(Backend.Native_name.encode component)
                    operation Link_like "link-like"))
        | Dir_cap.Regular | Dir_cap.Other_entry ->
            Directory_failed
              (operation_failure
                 (contract_error
                    ~component:(Backend.Native_name.encode component)
                    operation Not_directory "not-directory")))
    | Ok None -> (
        match
          Backend.create_directory parent.capability component
            ~permissions:private_directory_permissions
        with
        | Backend.Created child_capability ->
            let child = held child_capability in
            Directory_created child
        | Backend.Not_created { class_ = Dir_cap.Already_exists; _ } -> (
            match Backend.probe_entry_no_follow parent.capability component with
            | Error failure ->
                Directory_failed (map_backend_failure operation failure)
            | Ok None ->
                Directory_failed
                  (operation_failure
                     (contract_error
                        ~component:(Backend.Native_name.encode component)
                        operation Identity_changed "create-race-missing"))
            | Ok (Some stat) when stat.kind = Dir_cap.Directory ->
                opened_existing_directory operation parent component
                  stat.identity
            | Ok (Some stat) ->
                Directory_failed
                  (operation_failure
                     (contract_error
                        ~component:(Backend.Native_name.encode component)
                        operation
                        (if stat.kind = Dir_cap.Symbolic_link then Link_like
                         else Not_directory)
                        "create-race-not-directory")))
        | Backend.Not_created error ->
            Directory_failed (operation_failure (map_error operation error))
        | Backend.Creation_incomplete { residual; failure } -> (
            let mapped = map_backend_failure operation failure in
            match residual with
            | Backend.Captured child_capability ->
                let child = held child_capability in
                let observation = Directory_creation_observed { path } in
                Directory_incomplete
                  (observation, mapped, cleanup_directory operation child)
            | Backend.Uncaptured (Backend.Creation_observed _) ->
                Directory_incomplete
                  (Directory_creation_observed { path }, mapped, no_cleanup)
            | Backend.Uncaptured (Backend.Creation_may_have_committed _) ->
                Directory_incomplete
                  ( Directory_creation_may_have_committed { path },
                    mapped,
                    no_cleanup )))

  let mkdirs (root : root) relative =
    match require_owner Mkdirs "root" root.owner with
    | Error failure -> Directories_incomplete { created = []; failure }
    | Ok () -> (
        match duplicate_directory Mkdirs root.directory with
        | Error failure -> Directories_incomplete { created = []; failure }
        | Ok initial ->
            let rec loop prefix created current = function
              | [] ->
                  let cleanup = cleanup_directory Mkdirs current in
                  Directories_ready
                    { created = List.rev created; advisories = cleanup.issues }
              | portable :: rest -> (
                  let next_path = Relative.child prefix portable in
                  match name Mkdirs portable with
                  | Error failure ->
                      Directories_incomplete
                        {
                          created = List.rev created;
                          failure =
                            append_issues failure
                              (cleanup_directory Mkdirs current).issues;
                        }
                  | Ok component -> (
                      match
                        open_or_create_directory Mkdirs next_path current
                          component
                      with
                      | Directory_failed failure ->
                          Directories_incomplete
                            {
                              created = List.rev created;
                              failure =
                                append_issues failure
                                  (cleanup_directory Mkdirs current).issues;
                            }
                      | Directory_incomplete (observation, failure, residual) ->
                          let cleanup =
                            combine_cleanup
                              [ residual; cleanup_directory Mkdirs current ]
                          in
                          Directories_incomplete
                            {
                              created = List.rev (observation :: created);
                              failure = append_issues failure cleanup.issues;
                            }
                      | Directory_opened child ->
                          let cleanup = cleanup_directory Mkdirs current in
                          if cleanup.issues = [] then
                            loop next_path created child rest
                          else
                            let child_cleanup =
                              cleanup_directory Mkdirs child
                            in
                            Directories_incomplete
                              {
                                created = List.rev created;
                                failure =
                                  failure_issues
                                    (cleanup.issues @ child_cleanup.issues);
                              }
                      | Directory_created child ->
                          let observation =
                            Directory_creation_observed { path = next_path }
                          in
                          let cleanup = cleanup_directory Mkdirs current in
                          if cleanup.issues = [] then
                            loop next_path (observation :: created) child rest
                          else
                            let child_cleanup =
                              cleanup_directory Mkdirs child
                            in
                            Directories_incomplete
                              {
                                created = List.rev (observation :: created);
                                failure =
                                  failure_issues
                                    (cleanup.issues @ child_cleanup.issues);
                              }))
            in
            loop Relative.root [] initial (Relative.components relative))

  let captured_is_live (captured : captured_regular) =
    captured.captured_state = Capture_open
    && (is_open captured.file || is_open captured.parent)

  let cleanup_captured operation (captured : captured_regular) =
    combine_cleanup
      [
        cleanup_file operation captured.file;
        cleanup_directory operation captured.parent;
      ]

  let close_captured (captured : captured_regular) =
    if captured.captured_state = Capture_consumed then Teardown_complete
    else
      let cleanup = cleanup_captured Close_captured captured in
      if not (captured_is_live captured) then
        captured.captured_state <- Capture_consumed;
      teardown captured ~live:(fun () -> captured_is_live captured) cleanup

  let capture_regular_with open_file (root : root) relative ~limit =
    if Int64.compare limit 0L < 0 then
      Error
        (operation_failure
           (contract_error Capture_regular Invalid_name "negative-read-limit"))
    else
      let* () = require_owner Capture_regular "root" root.owner in
      let* parent, leaf =
        resolve_parent_from_base Capture_regular root.directory relative
      in
      match Backend.probe_entry_no_follow parent.capability leaf with
      | Error failure ->
          Error
            (append_issues
               (map_backend_failure Capture_regular failure)
               (cleanup_directory Capture_regular parent).issues)
      | Ok None ->
          finish (Ok Read_missing) (cleanup_directory Capture_regular parent)
      | Ok (Some observed) -> (
          match observed.kind with
          | Dir_cap.Symbolic_link ->
              finish
                (Error
                   (operation_failure
                      (contract_error
                         ~component:(Backend.Native_name.encode leaf)
                         Capture_regular Link_like "link-like")))
                (cleanup_directory Capture_regular parent)
          | Dir_cap.Directory | Dir_cap.Other_entry ->
              finish
                (Error
                   (operation_failure
                      (contract_error
                         ~component:(Backend.Native_name.encode leaf)
                         Capture_regular Not_regular "not-regular")))
                (cleanup_directory Capture_regular parent)
          | Dir_cap.Regular -> (
              match open_file parent.capability leaf with
              | Error failure ->
                  Error
                    (append_issues
                       (map_backend_failure Capture_regular failure)
                       (cleanup_directory Capture_regular parent).issues)
              | Ok file_capability -> (
                  let file = held file_capability in
                  if
                    not
                      (Backend.Identity.equal observed.identity
                         (Backend.file_identity file.capability))
                  then
                    let cleanup =
                      combine_cleanup
                        [
                          cleanup_file Capture_regular file;
                          cleanup_directory Capture_regular parent;
                        ]
                    in
                    Error
                      (append_issues
                         (operation_failure
                            (contract_error
                               ~component:(Backend.Native_name.encode leaf)
                               Capture_regular Identity_changed
                               "file-identity-changed"))
                         cleanup.issues)
                  else
                    match Backend.read_captured file.capability ~limit with
                    | Error error ->
                        let cleanup =
                          combine_cleanup
                            [
                              cleanup_file Capture_regular file;
                              cleanup_directory Capture_regular parent;
                            ]
                        in
                        Error
                          (append_issues
                             (operation_failure
                                (map_error Capture_regular error))
                             cleanup.issues)
                    | Ok read ->
                        if
                          not
                            (Backend.Identity.equal observed.identity
                               read.stat.identity)
                        then
                          let cleanup =
                            combine_cleanup
                              [
                                cleanup_file Capture_regular file;
                                cleanup_directory Capture_regular parent;
                              ]
                          in
                          Error
                            (append_issues
                               (operation_failure
                                  (contract_error
                                     ~component:
                                       (Backend.Native_name.encode leaf)
                                     Capture_regular Identity_changed
                                     "captured-read-identity-changed"))
                               cleanup.issues)
                        else
                          let captured =
                            {
                              parent;
                              name = leaf;
                              file;
                              identity = read.stat.identity;
                              owner = Backend.file_owner file.capability;
                              namespace_released = ref false;
                              captured_state = Capture_open;
                            }
                          in
                          Ok
                            (Contents
                               {
                                 contents = read.contents;
                                 stat = stat_of_backend read.stat;
                                 captured;
                               }))))

  let capture_regular root relative ~limit =
    capture_regular_with Backend.open_file_for_delete_no_follow root relative
      ~limit

  let read_regular (root : root) relative ~limit =
    match
      capture_regular_with Backend.open_file_no_follow root relative ~limit
    with
    | Error failure -> Error failure
    | Ok Read_missing -> Ok Read_missing
    | Ok (Contents read) -> (
        let value =
          Contents
            ({ contents = read.contents; stat = read.stat }
              : captured_read_without_handle)
        in
        match close_captured read.captured with
        | Teardown_complete | Teardown_local_only -> Ok value
        | Teardown_incomplete { failure; _ } -> Error failure)

  let held_state_of_backend = function
    | Dir_cap.Still_open -> Held_open
    | Dir_cap.Closed -> Held_closed
    | Dir_cap.Invalidated_unknown -> Held_invalidated_unknown

  let issues_of_failure failure = failure.primary :: failure.suppressed

  let map_deletion_incomplete operation file namespace_released
      (progress : Dir_cap.deletion_progress) failure =
    namespace_released := progress.namespace_released;
    file.held_state <- held_state_of_backend progress.local_handle_state;
    match progress.local_handle_state with
    | Dir_cap.Still_open ->
        let retry =
          {
            cleanup_operation = operation;
            cleanup_target = Cleanup_deleting_file { file; namespace_released };
          }
        in
        map_backend_failure operation ~retry failure
    | Dir_cap.Closed | Dir_cap.Invalidated_unknown ->
        map_backend_failure operation failure

  let complete_captured_unlink captured disposition advisories =
    captured.captured_state <- Capture_consumed;
    let cleanup = cleanup_captured Unlink_captured captured in
    Captured_unlink_complete
      { disposition; advisories = advisories @ cleanup.issues }

  let reject_terminal_captured_unlink captured failure =
    captured.captured_state <- Capture_consumed;
    let cleanup = cleanup_captured Unlink_captured captured in
    Captured_unlink_rejected (append_issues failure cleanup.issues)

  let unlink_captured (captured : captured_regular) =
    if captured.captured_state = Capture_consumed then
      Captured_unlink_rejected
        (operation_failure
           (contract_error Unlink_captured Closed_capability
              "captured-file-consumed"))
    else if !(captured.namespace_released) then
      complete_captured_unlink captured Unlinked []
    else
      match require_owner Unlink_captured "captured-file" captured.owner with
      | Error failure ->
          Captured_unlink_retained { live_capture = captured; failure }
      | Ok () -> (
          match
            ( require_open Unlink_captured "captured-parent" captured.parent,
              require_open Unlink_captured "captured-file" captured.file )
          with
          | Error failure, _ | _, Error failure ->
              if captured_is_live captured then
                Captured_unlink_retained { live_capture = captured; failure }
              else (
                captured.captured_state <- Capture_consumed;
                Captured_unlink_rejected failure)
          | Ok parent, Ok file -> (
              match
                Backend.unlink_captured_file_if_identity ~parent file
                  ~expected:captured.identity
              with
              | Dir_cap.Deletion_not_committed error ->
                  Captured_unlink_retained
                    {
                      live_capture = captured;
                      failure =
                        operation_failure (map_error Unlink_captured error);
                    }
              | Dir_cap.Deletion_complete Dir_cap.Unlinked ->
                  captured.namespace_released := true;
                  captured.file.held_state <- Held_closed;
                  complete_captured_unlink captured Unlinked []
              | Dir_cap.Deletion_complete Dir_cap.Identity_changed ->
                  complete_captured_unlink captured Identity_changed_entry []
              | Dir_cap.Deletion_complete Dir_cap.Absent ->
                  Captured_unlink_retained
                    {
                      live_capture = captured;
                      failure =
                        operation_failure
                          (contract_error
                             ~component:
                               (Backend.Native_name.encode captured.name)
                             Unlink_captured Backend_contract_violation
                             "captured-delete-reported-absent");
                    }
              | Dir_cap.Deletion_incomplete { progress; failure } ->
                  let failure =
                    map_deletion_incomplete Unlink_captured captured.file
                      captured.namespace_released progress failure
                  in
                  if is_open captured.file then
                    Captured_unlink_retained
                      { live_capture = captured; failure }
                  else if !(captured.namespace_released) then
                    complete_captured_unlink captured Unlinked
                      (issues_of_failure failure)
                  else reject_terminal_captured_unlink captured failure))

  let inspect_kind (root : root) relative =
    let* () = require_owner Inspect_kind "root" root.owner in
    let* _ = require_open Inspect_kind "root" root.directory in
    match Relative.components relative with
    | [] -> Ok (Some Directory)
    | _ ->
        let* parent, leaf =
          resolve_parent_from_base Inspect_kind root.directory relative
        in
        let action =
          match Backend.probe_entry_no_follow parent.capability leaf with
          | Error failure -> Error (map_backend_failure Inspect_kind failure)
          | Ok stat ->
              Ok
                (Option.map
                   (fun (stat : Backend.stat) ->
                     entry_kind_of_backend stat.kind)
                   stat)
        in
        finish action (cleanup_directory Inspect_kind parent)

  let stage_operational (staged : staged_file) =
    is_open staged.file && is_open staged.parent
    && is_open staged.authority_root

  let stage_cleanup_live (staged : staged_file) =
    is_open staged.file || is_open staged.parent
    || is_open staged.authority_root

  let cleanup_stage operation (staged : staged_file) =
    combine_cleanup
      [
        cleanup_file operation staged.file;
        cleanup_directory operation staged.parent;
        cleanup_directory operation staged.authority_root;
      ]

  let make_stage authority_root parent source_name file =
    {
      authority_root;
      parent;
      source_name;
      file;
      identity = Backend.file_identity file.capability;
      owner = Backend.file_owner file.capability;
      source_released = ref false;
      staged_state = Stage_open;
    }

  let capture_stage_for_publish (root : root) relative ~expected_contents =
    let operation = Capture_stage_for_publish in
    let* () = require_owner operation "root" root.owner in
    let* authority_root = duplicate_directory operation root.directory in
    match resolve_parent_from_base operation root.directory relative with
    | Error failure ->
        Error
          (append_issues failure
             (cleanup_directory operation authority_root).issues)
    | Ok (parent, leaf) -> (
        match
          Backend.open_file_for_publish_no_follow parent.capability leaf
        with
        | Error failure ->
            let cleanup =
              combine_cleanup
                [
                  cleanup_directory operation parent;
                  cleanup_directory operation authority_root;
                ]
            in
            Error
              (append_issues
                 (map_backend_failure operation failure)
                 cleanup.issues)
        | Ok file_capability -> (
            let file = held file_capability in
            let cleanup_failure failure =
              let cleanup =
                combine_cleanup
                  [
                    cleanup_file operation file;
                    cleanup_directory operation parent;
                    cleanup_directory operation authority_root;
                  ]
              in
              Error (append_issues failure cleanup.issues)
            in
            let limit = Int64.of_int (String.length expected_contents) in
            match Backend.read_captured file.capability ~limit with
            | Error error ->
                cleanup_failure (operation_failure (map_error operation error))
            | Ok read
              when not
                     (Backend.Identity.equal
                        (Backend.file_identity file.capability)
                        read.stat.identity) ->
                cleanup_failure
                  (operation_failure
                     (contract_error
                        ~component:(Backend.Native_name.encode leaf)
                        operation Identity_changed
                        "publication-handle-identity-changed"))
            | Ok read when not (String.equal read.contents expected_contents) ->
                cleanup_failure
                  (operation_failure
                     (contract_error
                        ~component:(Backend.Native_name.encode leaf)
                        operation Identity_changed
                        "publication-stage-contents-changed"))
            | Ok _ -> Ok (make_stage authority_root parent leaf file)))

  let close_stage (staged : staged_file) =
    match staged.staged_state with
    | Stage_published | Stage_discarded -> Teardown_complete
    | Stage_open ->
        let cleanup = cleanup_stage Close_stage staged in
        if not (stage_cleanup_live staged) then
          staged.staged_state <- Stage_discarded;
        teardown staged ~live:(fun () -> stage_cleanup_live staged) cleanup

  let residual_observation name = function
    | Backend.Creation_observed _ -> Residual_creation_observed { name }
    | Backend.Creation_may_have_committed _ ->
        Residual_creation_may_have_committed { name }

  type internal_stage_creation =
    | Internal_stage_created of staged_file
    | Internal_stage_not_created of {
        error : Dir_cap.error;
        cleanup : cleanup_summary;
      }
    | Internal_stage_failed of operation_failure
    | Internal_stage_incomplete_live of {
        stage : staged_file;
        failure : operation_failure;
      }
    | Internal_stage_incomplete_audit of {
        residual : residual_observation;
        failure : operation_failure;
      }

  let create_stage operation (root : root) relative ~contents =
    match require_owner operation "root" root.owner with
    | Error failure -> Internal_stage_failed failure
    | Ok () -> (
        match duplicate_directory operation root.directory with
        | Error failure -> Internal_stage_failed failure
        | Ok authority_root -> (
            match
              resolve_parent_from_base operation root.directory relative
            with
            | Error failure ->
                Internal_stage_failed
                  (append_issues failure
                     (cleanup_directory operation authority_root).issues)
            | Ok (parent, leaf) -> (
                match
                  Backend.create_file parent.capability leaf
                    ~permissions:private_file_permissions ~contents
                with
                | Backend.Created file_capability ->
                    Internal_stage_created
                      (make_stage authority_root parent leaf
                         (held file_capability))
                | Backend.Not_created error ->
                    Internal_stage_not_created
                      {
                        error;
                        cleanup =
                          combine_cleanup
                            [
                              cleanup_directory operation parent;
                              cleanup_directory operation authority_root;
                            ];
                      }
                | Backend.Creation_incomplete { residual; failure } -> (
                    let mapped = map_backend_failure operation failure in
                    match residual with
                    | Backend.Captured file_capability ->
                        let stage =
                          make_stage authority_root parent leaf
                            (held file_capability)
                        in
                        Internal_stage_incomplete_live
                          { stage; failure = mapped }
                    | Backend.Uncaptured evidence ->
                        let cleanup =
                          combine_cleanup
                            [
                              cleanup_directory operation parent;
                              cleanup_directory operation authority_root;
                            ]
                        in
                        Internal_stage_incomplete_audit
                          {
                            residual = residual_observation leaf evidence;
                            failure = append_issues mapped cleanup.issues;
                          }))))

  let staged_owner (staged : staged_file) = staged.owner

  let stage_file (root : root) relative ~contents =
    match create_stage Stage_file root relative ~contents with
    | Internal_stage_created stage -> Staged stage
    | Internal_stage_failed failure -> Staging_not_created failure
    | Internal_stage_not_created { error; cleanup } ->
        Staging_not_created
          (append_issues
             (operation_failure (map_error Stage_file error))
             cleanup.issues)
    | Internal_stage_incomplete_live { stage; failure } ->
        Staging_incomplete_actionable { live_stage = stage; failure }
    | Internal_stage_incomplete_audit { residual; failure } ->
        Staging_incomplete_audit_only { residual; failure }

  let create_exclusive (root : root) relative ~contents =
    match create_stage Create_exclusive root relative ~contents with
    | Internal_stage_failed failure -> File_not_created failure
    | Internal_stage_not_created { error; cleanup }
      when error.class_ = Dir_cap.Already_exists ->
        File_exists { advisories = cleanup.issues }
    | Internal_stage_not_created { error; cleanup } ->
        File_not_created
          (append_issues
             (operation_failure (map_error Create_exclusive error))
             cleanup.issues)
    | Internal_stage_incomplete_live { stage; failure } ->
        File_creation_incomplete
          { live_file = Some stage; audit_only = []; failure }
    | Internal_stage_incomplete_audit { residual; failure } ->
        File_creation_incomplete
          { live_file = None; audit_only = [ residual ]; failure }
    | Internal_stage_created stage ->
        stage.staged_state <- Stage_discarded;
        File_created
          { advisories = (cleanup_stage Create_exclusive stage).issues }

  let require_stage operation (staged : staged_file) =
    match staged.staged_state with
    | Stage_published | Stage_discarded ->
        Error
          (operation_failure
             (contract_error operation Closed_capability "stage-consumed"))
    | Stage_open ->
        let* () = require_owner operation "stage" staged.owner in
        if stage_operational staged then Ok ()
        else
          Error
            (operation_failure
               (contract_error operation Closed_capability
                  "stage-partially-closed"))

  type internal_publish_outcome =
    | Internal_publish_rejected of operation_failure
    | Internal_not_published of {
        stage : staged_file;
        failure : operation_failure;
      }
    | Internal_published of advisory list

  let publish_stage operation replacement (staged : staged_file) target =
    match require_stage operation staged with
    | Error failure -> Internal_publish_rejected failure
    | Ok () -> (
        match
          resolve_parent_from_base operation staged.authority_root target
        with
        | Error failure -> Internal_not_published { stage = staged; failure }
        | Ok (target_parent, target_name) -> (
            match
              Backend.atomic_rename staged.file.capability
                ~into:target_parent.capability ~as_:target_name ~replacement
            with
            | Dir_cap.Not_published error ->
                Internal_not_published
                  {
                    stage = staged;
                    failure =
                      append_issues
                        (operation_failure (map_error operation error))
                        (cleanup_directory operation target_parent).issues;
                  }
            | Dir_cap.Published { advisories } ->
                staged.staged_state <- Stage_published;
                let cleanup =
                  combine_cleanup
                    [
                      cleanup_directory operation target_parent;
                      cleanup_stage operation staged;
                    ]
                in
                Internal_published
                  (List.map
                     (fun error -> Operation_error (map_error operation error))
                     advisories
                  @ cleanup.issues)))

  let publish_no_replace (staged : staged_file) ~target =
    match publish_stage Publish_no_replace Dir_cap.No_replace staged target with
    | Internal_publish_rejected failure -> Stage_publish_rejected failure
    | Internal_not_published { stage; failure } ->
        Stage_not_published { live_stage = stage; failure }
    | Internal_published advisories -> Stage_published { advisories }

  type sequence_state = Next_sequence of int64 | Sequence_exhausted

  let stage_sequence_mutex = ref (Mutex.create ())
  let stage_sequence_pid = ref (Unix.getpid ())
  let stage_sequence_state = ref (Next_sequence 0L)

  let reset_stage_sequence_after_fork () =
    let process = Unix.getpid () in
    if process <> !stage_sequence_pid then (
      stage_sequence_pid := process;
      stage_sequence_state := Next_sequence 0L;
      stage_sequence_mutex := Mutex.create ())

  let next_stage_sequence () =
    reset_stage_sequence_after_fork ();
    let mutex = !stage_sequence_mutex in
    Mutex.lock mutex;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock mutex)
      (fun () ->
        match !stage_sequence_state with
        | Sequence_exhausted ->
            Error
              (operation_failure
                 (contract_error Replace_file Unsupported
                    "stage-sequence-exhausted"))
        | Next_sequence current ->
            stage_sequence_state :=
              if Int64.equal current Int64.max_int then Sequence_exhausted
              else Next_sequence (Int64.succ current);
            Ok current)

  let internal_stage_relative target sequence =
    let component =
      Printf.sprintf ".ocaml-mutants-stage-p%d-s%Ld" (Unix.getpid ()) sequence
    in
    match Name.of_string component with
    | Error _ ->
        Error
          (operation_failure
             (contract_error Replace_file Backend_contract_violation
                "generated-stage-name-invalid"))
    | Ok name -> (
        match List.rev (Relative.components target) with
        | [] ->
            Error
              (operation_failure
                 (contract_error Replace_file Invalid_name "root-has-no-parent"))
        | _target_name :: reversed_parent ->
            let parent =
              List.fold_left Relative.child Relative.root
                (List.rev reversed_parent)
            in
            Ok (Relative.child parent name))

  let replace_file (root : root) target ~contents =
    match next_stage_sequence () with
    | Error failure ->
        Replacement_not_published
          { live_stage = None; audit_only = []; failure }
    | Ok sequence -> (
        match internal_stage_relative target sequence with
        | Error failure ->
            Replacement_not_published
              { live_stage = None; audit_only = []; failure }
        | Ok stage_path -> (
            match create_stage Replace_file root stage_path ~contents with
            | Internal_stage_failed failure ->
                Replacement_not_published
                  { live_stage = None; audit_only = []; failure }
            | Internal_stage_not_created { error; cleanup } ->
                Replacement_not_published
                  {
                    live_stage = None;
                    audit_only = [];
                    failure =
                      append_issues
                        (operation_failure (map_error Replace_file error))
                        cleanup.issues;
                  }
            | Internal_stage_incomplete_live { stage; failure } ->
                Replacement_not_published
                  { live_stage = Some stage; audit_only = []; failure }
            | Internal_stage_incomplete_audit { residual; failure } ->
                Replacement_not_published
                  { live_stage = None; audit_only = [ residual ]; failure }
            | Internal_stage_created stage -> (
                match
                  publish_stage Replace_file Dir_cap.Replace stage target
                with
                | Internal_publish_rejected failure ->
                    Replacement_not_published
                      { live_stage = Some stage; audit_only = []; failure }
                | Internal_not_published { stage; failure } ->
                    Replacement_not_published
                      { live_stage = Some stage; audit_only = []; failure }
                | Internal_published advisories -> Replaced { advisories })))

  let complete_stage_discard staged advisories =
    staged.staged_state <- Stage_discarded;
    let cleanup = cleanup_stage Discard_stage staged in
    Stage_discarded { advisories = advisories @ cleanup.issues }

  let terminal_stage_discard_failure staged failure =
    staged.staged_state <- Stage_discarded;
    let cleanup = cleanup_stage Discard_stage staged in
    Stage_discard_incomplete_audit_only
      {
        residual = Residual_creation_observed { name = staged.source_name };
        failure = append_issues failure cleanup.issues;
      }

  let stage_identity_changed_advisory staged =
    Operation_error
      (contract_error
         ~component:(Backend.Native_name.encode staged.source_name)
         Discard_stage Identity_changed "identity-changed")

  let discard_stage (staged : staged_file) =
    match staged.staged_state with
    | Stage_published | Stage_discarded -> Stage_discarded { advisories = [] }
    | Stage_open -> (
        let owner_is_current =
          Backend.owner_equal staged.owner (Backend.current_owner ())
        in
        if owner_is_current && !(staged.source_released) then
          complete_stage_discard staged []
        else if not (stage_operational staged) then
          let primary =
            operation_failure
              (contract_error Discard_stage Closed_capability
                 "stage-partially-closed")
          in
          let cleanup = cleanup_stage Discard_stage staged in
          if stage_cleanup_live staged then
            Stage_discard_retained
              {
                live_stage = staged;
                failure = append_issues primary cleanup.issues;
              }
          else (
            staged.staged_state <- Stage_discarded;
            if (not owner_is_current) && cleanup.issues = [] then
              Stage_discard_local_only { advisories = [] }
            else
              Stage_discard_incomplete_audit_only
                {
                  residual =
                    Residual_creation_observed { name = staged.source_name };
                  failure = append_issues primary cleanup.issues;
                })
        else if not owner_is_current then
          let cleanup = cleanup_stage Discard_stage staged in
          if stage_cleanup_live staged then
            Stage_discard_retained
              { live_stage = staged; failure = failure_issues cleanup.issues }
          else if cleanup.issues <> [] then (
            staged.staged_state <- Stage_discarded;
            Stage_discard_incomplete_audit_only
              {
                residual =
                  Residual_creation_observed { name = staged.source_name };
                failure = failure_issues cleanup.issues;
              })
          else (
            staged.staged_state <- Stage_discarded;
            Stage_discard_local_only { advisories = [] })
        else
          match
            Backend.unlink_captured_file_if_identity
              ~parent:staged.parent.capability staged.file.capability
              ~expected:staged.identity
          with
          | Dir_cap.Deletion_not_committed error ->
              Stage_discard_retained
                {
                  live_stage = staged;
                  failure = operation_failure (map_error Discard_stage error);
                }
          | Dir_cap.Deletion_complete Dir_cap.Unlinked ->
              staged.source_released := true;
              staged.file.held_state <- Held_closed;
              complete_stage_discard staged []
          | Dir_cap.Deletion_complete Dir_cap.Identity_changed ->
              complete_stage_discard staged
                [ stage_identity_changed_advisory staged ]
          | Dir_cap.Deletion_complete Dir_cap.Absent ->
              Stage_discard_retained
                {
                  live_stage = staged;
                  failure =
                    operation_failure
                      (contract_error
                         ~component:
                           (Backend.Native_name.encode staged.source_name)
                         Discard_stage Backend_contract_violation
                         "captured-delete-reported-absent");
                }
          | Dir_cap.Deletion_incomplete { progress; failure } ->
              let failure =
                map_deletion_incomplete Discard_stage staged.file
                  staged.source_released progress failure
              in
              if is_open staged.file then
                Stage_discard_retained { live_stage = staged; failure }
              else if !(staged.source_released) then
                complete_stage_discard staged (issues_of_failure failure)
              else terminal_stage_discard_failure staged failure)

  let zero_progress = { entries = 0L; native_name_bytes = 0L; removed = 0L }

  let progress_of_consumption (consumption : Backend.enumeration_consumption) =
    {
      entries = consumption.entries;
      native_name_bytes = consumption.native_name_bytes;
      removed = 0L;
    }

  let validate_consumption operation ~max_entries ~max_native_name_bytes
      (consumption : Backend.enumeration_consumption) =
    if
      Int64.compare consumption.entries 0L < 0
      || Int64.compare consumption.native_name_bytes 0L < 0
    then
      Error
        (operation_failure
           (contract_error operation Backend_contract_violation
              "backend-enumeration-negative-consumption"))
    else if
      Int64.compare consumption.entries max_entries > 0
      || Int64.compare consumption.native_name_bytes max_native_name_bytes > 0
    then
      Error
        (operation_failure
           (contract_error operation Backend_contract_violation
              "backend-enumeration-exceeded-budget"))
    else Ok (progress_of_consumption consumption)

  let validate_enumerated_names operation names
      (consumption : Backend.enumeration_consumption) =
    if Int64.equal consumption.entries (Int64.of_int (List.length names)) then
      Ok ()
    else
      Error
        (operation_failure
           (contract_error operation Backend_contract_violation
              "backend-enumeration-count-mismatch"))

  let backend_enumeration_budget operation budget =
    match
      Backend.enumeration_budget
        ~max_entries:(Traversal_budget.max_entries budget)
        ~max_native_name_bytes:(Traversal_budget.max_native_name_bytes budget)
    with
    | Ok budget -> Ok budget
    | Error _ ->
        Error
          (operation_failure
             (contract_error operation Backend_contract_violation
                "validated-budget-rejected-by-backend"))

  let list (root : root) relative ~budget =
    match require_owner List_directory "root" root.owner with
    | Error failure -> Listing_incomplete { progress = zero_progress; failure }
    | Ok () -> (
        match duplicate_directory List_directory root.directory with
        | Error failure ->
            Listing_incomplete { progress = zero_progress; failure }
        | Ok authority_root -> (
            match
              resolve_directory_from_base List_directory root.directory relative
            with
            | Error failure ->
                Listing_incomplete
                  {
                    progress = zero_progress;
                    failure =
                      append_issues failure
                        (cleanup_directory List_directory authority_root).issues;
                  }
            | Ok directory -> (
                match backend_enumeration_budget List_directory budget with
                | Error failure ->
                    let cleanup =
                      combine_cleanup
                        [
                          cleanup_directory List_directory directory;
                          cleanup_directory List_directory authority_root;
                        ]
                    in
                    Listing_incomplete
                      {
                        progress = zero_progress;
                        failure = append_issues failure cleanup.issues;
                      }
                | Ok backend_budget -> (
                    let incomplete progress failure =
                      let cleanup =
                        combine_cleanup
                          [
                            cleanup_directory List_directory directory;
                            cleanup_directory List_directory authority_root;
                          ]
                      in
                      Listing_incomplete
                        {
                          progress;
                          failure = append_issues failure cleanup.issues;
                        }
                    in
                    match
                      Backend.enumerate_no_follow directory.capability
                        ~budget:backend_budget
                    with
                    | Backend.Enumeration_incomplete { consumption; failure }
                      -> (
                        match
                          validate_consumption List_directory
                            ~max_entries:(Traversal_budget.max_entries budget)
                            ~max_native_name_bytes:
                              (Traversal_budget.max_native_name_bytes budget)
                            consumption
                        with
                        | Error contract_failure ->
                            incomplete zero_progress contract_failure
                        | Ok progress ->
                            incomplete progress
                              (map_enumeration_failure List_directory failure))
                    | Backend.Enumerated { names; consumption } -> (
                        match
                          validate_consumption List_directory
                            ~max_entries:(Traversal_budget.max_entries budget)
                            ~max_native_name_bytes:
                              (Traversal_budget.max_native_name_bytes budget)
                            consumption
                        with
                        | Error failure -> incomplete zero_progress failure
                        | Ok progress -> (
                            match
                              validate_enumerated_names List_directory names
                                consumption
                            with
                            | Error failure -> incomplete zero_progress failure
                            | Ok () ->
                                let names =
                                  List.sort
                                    (fun left right ->
                                      String.compare
                                        (Backend.Native_name.encode left)
                                        (Backend.Native_name.encode right))
                                    names
                                in
                                let listing =
                                  {
                                    authority_root;
                                    directory;
                                    owner =
                                      Backend.dir_owner directory.capability;
                                    listing_closed = false;
                                    entries = [];
                                  }
                                in
                                listing.entries <-
                                  List.map (fun name -> { listing; name }) names;
                                Listing_complete { listing; progress }))))))

  let listing_failure operation (listing : listing) =
    if listing.listing_closed || not (is_open listing.directory) then
      Some
        (operation_failure
           (contract_error operation Closed_capability "listing-closed"))
    else
      match require_owner operation "listing" listing.owner with
      | Ok () -> None
      | Error failure -> Some failure

  let listed_entries (listing : listing) =
    match listing_failure List_directory listing with
    | Some failure -> Error failure
    | None -> Ok listing.entries

  let listed_name (entry : listed_entry) = entry.name

  let listing_is_live (listing : listing) =
    (not listing.listing_closed)
    && (is_open listing.directory || is_open listing.authority_root)

  let close_listing (listing : listing) =
    if listing.listing_closed then Teardown_complete
    else
      let cleanup =
        combine_cleanup
          [
            cleanup_directory Close_listing listing.directory;
            cleanup_directory Close_listing listing.authority_root;
          ]
      in
      if not (is_open listing.directory || is_open listing.authority_root) then
        listing.listing_closed <- true;
      teardown listing ~live:(fun () -> listing_is_live listing) cleanup

  let target_operational = function
    | Captured_directory directory -> is_open directory
    | Captured_file file -> is_open file
    | Lease_protected_atomic_binding _ -> true

  let target_cleanup_live = function
    | Captured_directory directory -> is_open directory
    | Captured_file file -> is_open file
    | Lease_protected_atomic_binding _ -> false

  let cleanup_target operation = function
    | Captured_directory directory -> cleanup_directory operation directory
    | Captured_file file -> cleanup_file operation file
    | Lease_protected_atomic_binding _ -> no_cleanup

  let target_identity = function
    | Captured_directory directory -> Backend.dir_identity directory.capability
    | Captured_file file -> Backend.file_identity file.capability
    | Lease_protected_atomic_binding _ ->
        invalid_arg "atomic-only entry has no retained target identity"

  let inspection_operational (inspection : inspection_core) =
    (not inspection.inspection_consumed)
    && is_open inspection.authority_root
    && is_open inspection.parent
    && target_operational inspection.target

  let inspection_cleanup_live (inspection : inspection_core) =
    (not inspection.inspection_consumed)
    && (is_open inspection.authority_root
       || is_open inspection.parent
       || target_cleanup_live inspection.target)

  let cleanup_inspection operation (inspection : inspection_core) =
    combine_cleanup
      [
        cleanup_target operation inspection.target;
        cleanup_directory operation inspection.parent;
        cleanup_directory operation inspection.authority_root;
      ]

  let close_inspection resource (inspection : inspection_core) =
    if inspection.inspection_consumed then Teardown_complete
    else
      let cleanup = cleanup_inspection Close_inspection inspection in
      if not (inspection_cleanup_live inspection) then
        inspection.inspection_consumed <- true;
      teardown resource
        ~live:(fun () -> inspection_cleanup_live inspection)
        cleanup

  let close_inspected_path (Inspected_path inspection as inspected) =
    close_inspection inspected inspection

  let close_inspected_entry (Inspected_entry inspection as inspected) =
    close_inspection inspected inspection

  let inspect_from_parent operation ~root_identity ~root_owner
      ~deletion_authority authority_root parent name =
    let cleanup_without_target failure =
      let cleanup =
        combine_cleanup
          [
            cleanup_directory operation parent;
            cleanup_directory operation authority_root;
          ]
      in
      Error (append_issues failure cleanup.issues)
    in
    match Backend.probe_entry_no_follow parent.capability name with
    | Error failure ->
        cleanup_without_target (map_backend_failure operation failure)
    | Ok None ->
        let cleanup =
          combine_cleanup
            [
              cleanup_directory operation parent;
              cleanup_directory operation authority_root;
            ]
        in
        finish (Ok Read_missing) cleanup
    | Ok (Some observed) -> (
        let opened =
          match observed.kind with
          | Dir_cap.Directory -> (
              match Backend.open_directory_no_follow parent.capability name with
              | Ok directory -> Ok (Captured_directory (held directory))
              | Error failure -> Error (map_backend_failure operation failure))
          | Dir_cap.Regular -> (
              match Backend.open_file_no_follow parent.capability name with
              | Ok file -> Ok (Captured_file (held file))
              | Error failure -> Error (map_backend_failure operation failure))
          | (Dir_cap.Symbolic_link | Dir_cap.Other_entry) as kind ->
              Ok (Lease_protected_atomic_binding kind)
        in
        match opened with
        | Error failure -> cleanup_without_target failure
        | Ok target ->
            if
              match target with
              | Lease_protected_atomic_binding _ -> false
              | Captured_directory _ | Captured_file _ ->
                  not
                    (Backend.Identity.equal observed.identity
                       (target_identity target))
            then
              let cleanup =
                combine_cleanup
                  [
                    cleanup_target operation target;
                    cleanup_directory operation parent;
                    cleanup_directory operation authority_root;
                  ]
              in
              Error
                (append_issues
                   (operation_failure
                      (contract_error
                         ~component:(Backend.Native_name.encode name)
                         operation Identity_changed "entry-identity-changed"))
                   cleanup.issues)
            else
              let inspection =
                {
                  authority_root;
                  parent;
                  name;
                  target;
                  stat = stat_of_backend observed;
                  root_identity;
                  root_owner;
                  owner =
                    (match target with
                    | Captured_directory directory ->
                        Backend.dir_owner directory.capability
                    | Captured_file file -> Backend.file_owner file.capability
                    | Lease_protected_atomic_binding _ ->
                        Backend.dir_owner parent.capability);
                  deletion_authority;
                  inspection_consumed = false;
                }
              in
              Ok (Contents inspection))

  let establish_authority_for_root operation authority root identity owner =
    let* root = require_open operation "authority-root" root in
    match Deletion_authority.establish authority ~root ~identity ~owner with
    | Ok () -> Ok ()
    | Error failure -> Error (map_backend_failure operation failure)

  let validate_authority_continuation operation authority root identity owner =
    let* root = require_open operation "authority-root" root in
    match
      Deletion_authority.validate_continuation authority ~root ~identity ~owner
    with
    | Ok () -> Ok ()
    | Error failure -> Error (map_backend_failure operation failure)

  let inspect_path authority (root : root) relative =
    match require_owner Inspect_path "root" root.owner with
    | Error failure -> Error failure
    | Ok () -> (
        match duplicate_directory Inspect_path root.directory with
        | Error failure -> Error failure
        | Ok authority_root -> (
            let authority_identity =
              Backend.dir_identity authority_root.capability
            in
            let authority_owner = Backend.dir_owner authority_root.capability in
            match
              establish_authority_for_root Validate_deletion_authority authority
                authority_root authority_identity authority_owner
            with
            | Error failure ->
                Error
                  (append_issues failure
                     (cleanup_directory Inspect_path authority_root).issues)
            | Ok () -> (
                match
                  resolve_parent_from_base Inspect_path root.directory relative
                with
                | Error failure ->
                    Error
                      (append_issues failure
                         (cleanup_directory Inspect_path authority_root).issues)
                | Ok (parent, name) ->
                    let* inspected =
                      inspect_from_parent Inspect_path
                        ~root_identity:authority_identity
                        ~root_owner:authority_owner
                        ~deletion_authority:authority authority_root parent name
                    in
                    Ok
                      (match inspected with
                      | Read_missing -> Read_missing
                      | Contents inspection ->
                          Contents (Inspected_path inspection)))))

  let inspect_listed authority entry =
    match listing_failure Inspect_listed entry.listing with
    | Some failure -> Error failure
    | None -> (
        match
          duplicate_directory Inspect_listed entry.listing.authority_root
        with
        | Error failure -> Error failure
        | Ok authority_root -> (
            let authority_identity =
              Backend.dir_identity authority_root.capability
            in
            let authority_owner = Backend.dir_owner authority_root.capability in
            match
              establish_authority_for_root Validate_deletion_authority authority
                authority_root authority_identity authority_owner
            with
            | Error failure ->
                Error
                  (append_issues failure
                     (cleanup_directory Inspect_listed authority_root).issues)
            | Ok () -> (
                match
                  duplicate_directory Inspect_listed entry.listing.directory
                with
                | Error failure ->
                    Error
                      (append_issues failure
                         (cleanup_directory Inspect_listed authority_root)
                           .issues)
                | Ok parent ->
                    let* inspected =
                      inspect_from_parent Inspect_listed
                        ~root_identity:authority_identity
                        ~root_owner:authority_owner
                        ~deletion_authority:authority authority_root parent
                        entry.name
                    in
                    Ok
                      (match inspected with
                      | Read_missing -> Read_missing
                      | Contents inspection ->
                          Contents (Inspected_entry inspection)))))

  let inspected_path_stat (Inspected_path inspection) = inspection.stat
  let inspected_entry_stat (Inspected_entry inspection) = inspection.stat
  let inspected_entry_name (Inspected_entry inspection) = inspection.name

  type traversal_state = {
    max_depth : int64;
    mutable remaining_entries : int64;
    mutable remaining_name_bytes : int64;
    mutable consumed_entries : int64;
    mutable consumed_name_bytes : int64;
    mutable removed : int64;
  }

  let traversal_state budget =
    {
      max_depth = Traversal_budget.max_depth budget;
      remaining_entries = Traversal_budget.max_entries budget;
      remaining_name_bytes = Traversal_budget.max_native_name_bytes budget;
      consumed_entries = 0L;
      consumed_name_bytes = 0L;
      removed = 0L;
    }

  let traversal_progress state =
    {
      entries = state.consumed_entries;
      native_name_bytes = state.consumed_name_bytes;
      removed = state.removed;
    }

  let budget_failure code =
    operation_failure (contract_error Remove_tree Budget_exhausted code)

  let consume_enumeration ?names state
      (consumption : Backend.enumeration_consumption) =
    let* _ =
      validate_consumption Remove_tree ~max_entries:state.remaining_entries
        ~max_native_name_bytes:state.remaining_name_bytes consumption
    in
    let* () =
      match names with
      | None -> Ok ()
      | Some names -> validate_enumerated_names Remove_tree names consumption
    in
    state.remaining_entries <-
      Int64.sub state.remaining_entries consumption.entries;
    state.remaining_name_bytes <-
      Int64.sub state.remaining_name_bytes consumption.native_name_bytes;
    state.consumed_entries <-
      Int64.add state.consumed_entries consumption.entries;
    state.consumed_name_bytes <-
      Int64.add state.consumed_name_bytes consumption.native_name_bytes;
    Ok ()

  let ensure_removal_count state =
    if Int64.equal state.removed Int64.max_int then
      Error (budget_failure "removed-count-exhausted")
    else Ok ()

  let record_removed state = state.removed <- Int64.succ state.removed

  let identity_changed_failure name =
    operation_failure
      (contract_error
         ~component:(Backend.Native_name.encode name)
         Remove_tree Identity_changed "identity-changed")

  let inspected_entry_depth = 0L

  let child_depth state depth =
    if Int64.equal depth Int64.max_int then
      Error (budget_failure "maximum-depth-exhausted")
    else
      let child = Int64.succ depth in
      if Int64.compare child state.max_depth > 0 then
        Error (budget_failure "maximum-depth-exhausted")
      else Ok child

  let enumerate_for_removal state directory depth =
    if Int64.compare depth state.max_depth > 0 then
      Error (budget_failure "maximum-depth-exhausted")
    else
      match
        Backend.enumeration_budget ~max_entries:state.remaining_entries
          ~max_native_name_bytes:state.remaining_name_bytes
      with
      | Error _ ->
          Error
            (operation_failure
               (contract_error Remove_tree Backend_contract_violation
                  "remaining-budget-rejected-by-backend"))
      | Ok budget -> (
          match Backend.enumerate_no_follow directory.capability ~budget with
          | Backend.Enumeration_incomplete { consumption; failure } ->
              let* () = consume_enumeration state consumption in
              Error (map_enumeration_failure Remove_tree failure)
          | Backend.Enumerated { names; consumption } ->
              let* () = consume_enumeration ~names state consumption in
              Ok
                (List.sort
                   (fun left right ->
                     String.compare
                       (Backend.Native_name.encode left)
                       (Backend.Native_name.encode right))
                   names))

  let close_after_native_removal held cleanup action =
    let cleanup = cleanup held in
    match (action, cleanup.issues) with
    | Ok (), [] -> Ok ()
    | Error failure, issues -> Error (append_issues failure issues)
    | Ok (), issues -> Error (failure_issues issues)

  let rec remove_native_entry state parent name depth =
    if Int64.compare depth state.max_depth > 0 then
      Error (budget_failure "maximum-depth-exhausted")
    else
      match Backend.probe_entry_no_follow parent.capability name with
      | Error failure -> Error (map_backend_failure Remove_tree failure)
      | Ok None -> Ok ()
      | Ok (Some observed) -> (
          match observed.kind with
          | Dir_cap.Symbolic_link -> (
              let* () = ensure_removal_count state in
              match
                Backend.unlink_link_no_follow parent.capability name
                  ~expected:observed.identity
              with
              | Error error ->
                  Error (operation_failure (map_error Remove_tree error))
              | Ok Dir_cap.Unlinked ->
                  record_removed state;
                  Ok ()
              | Ok Dir_cap.Absent -> Ok ()
              | Ok Dir_cap.Identity_changed ->
                  Error (identity_changed_failure name))
          | Dir_cap.Other_entry -> (
              let* () = ensure_removal_count state in
              match
                Backend.unlink_if_identity parent.capability name
                  ~expected:observed.identity
              with
              | Error error ->
                  Error (operation_failure (map_error Remove_tree error))
              | Ok Dir_cap.Unlinked ->
                  record_removed state;
                  Ok ()
              | Ok Dir_cap.Absent -> Ok ()
              | Ok Dir_cap.Identity_changed ->
                  Error (identity_changed_failure name))
          | Dir_cap.Regular -> (
              match Backend.open_file_no_follow parent.capability name with
              | Error failure -> Error (map_backend_failure Remove_tree failure)
              | Ok file_capability ->
                  let file = held file_capability in
                  if
                    not
                      (Backend.Identity.equal observed.identity
                         (Backend.file_identity file.capability))
                  then
                    Error
                      (append_issues
                         (identity_changed_failure name)
                         (cleanup_file Remove_tree file).issues)
                  else
                    let action =
                      let* () = ensure_removal_count state in
                      match
                        Backend.unlink_if_identity parent.capability name
                          ~expected:observed.identity
                      with
                      | Error error ->
                          Error
                            (operation_failure (map_error Remove_tree error))
                      | Ok Dir_cap.Unlinked ->
                          record_removed state;
                          Ok ()
                      | Ok Dir_cap.Absent -> Ok ()
                      | Ok Dir_cap.Identity_changed ->
                          Error (identity_changed_failure name)
                    in
                    close_after_native_removal file (cleanup_file Remove_tree)
                      action)
          | Dir_cap.Directory -> (
              match Backend.open_directory_no_follow parent.capability name with
              | Error failure -> Error (map_backend_failure Remove_tree failure)
              | Ok child_capability ->
                  let child = held child_capability in
                  if
                    not
                      (Backend.Identity.equal observed.identity
                         (Backend.dir_identity child.capability))
                  then
                    Error
                      (append_issues
                         (identity_changed_failure name)
                         (cleanup_directory Remove_tree child).issues)
                  else
                    let action =
                      let* children = enumerate_for_removal state child depth in
                      let rec remove_children = function
                        | [] -> Ok ()
                        | child_name :: rest ->
                            let* next_depth = child_depth state depth in
                            let* () =
                              remove_native_entry state child child_name
                                next_depth
                            in
                            remove_children rest
                      in
                      let* () = remove_children children in
                      let* () = ensure_removal_count state in
                      match
                        Backend.remove_empty_directory_if_identity
                          parent.capability name ~expected:observed.identity
                      with
                      | Error error ->
                          Error
                            (operation_failure (map_error Remove_tree error))
                      | Ok Dir_cap.Unlinked ->
                          record_removed state;
                          Ok ()
                      | Ok Dir_cap.Absent -> Ok ()
                      | Ok Dir_cap.Identity_changed ->
                          Error (identity_changed_failure name)
                    in
                    close_after_native_removal child
                      (cleanup_directory Remove_tree)
                      action))

  let validate_deletion_authority (inspection : inspection_core) =
    let* () = require_owner Remove_tree "inspection" inspection.owner in
    validate_authority_continuation Validate_deletion_authority
      inspection.deletion_authority inspection.authority_root
      inspection.root_identity inspection.root_owner

  let recheck_inspection (inspection : inspection_core) =
    let* parent =
      require_open Remove_tree "inspection-parent" inspection.parent
    in
    match Backend.probe_entry_no_follow parent inspection.name with
    | Error failure -> Error (map_backend_failure Remove_tree failure)
    | Ok None -> Error (identity_changed_failure inspection.name)
    | Ok (Some current) ->
        if Backend.Identity.equal current.identity inspection.stat.identity then
          Ok parent
        else Error (identity_changed_failure inspection.name)

  let remove_captured_inspection state (inspection : inspection_core) =
    let* parent = recheck_inspection inspection in
    match inspection.target with
    | Lease_protected_atomic_binding kind -> (
        let* () = ensure_removal_count state in
        let result =
          match kind with
          | Dir_cap.Symbolic_link ->
              Backend.unlink_link_no_follow parent inspection.name
                ~expected:inspection.stat.identity
          | Dir_cap.Other_entry ->
              Backend.unlink_if_identity parent inspection.name
                ~expected:inspection.stat.identity
          | Dir_cap.Directory | Dir_cap.Regular ->
              Error
                (Dir_cap.make_error ~operation:Dir_cap.Conditional_unlink
                   ~class_:Dir_cap.Other ~native_domain:Dir_cap.Contract
                   ~native_code:"captured-target-kind-mismatch" ())
        in
        match result with
        | Error error -> Error (operation_failure (map_error Remove_tree error))
        | Ok Dir_cap.Unlinked ->
            record_removed state;
            Ok ()
        | Ok Dir_cap.Absent -> Ok ()
        | Ok Dir_cap.Identity_changed ->
            Error (identity_changed_failure inspection.name))
    | Captured_file file -> (
        let* _ = require_open Remove_tree "inspection-file" file in
        let* () = ensure_removal_count state in
        match
          Backend.unlink_if_identity parent inspection.name
            ~expected:inspection.stat.identity
        with
        | Error error -> Error (operation_failure (map_error Remove_tree error))
        | Ok Dir_cap.Unlinked ->
            record_removed state;
            Ok ()
        | Ok Dir_cap.Absent -> Ok ()
        | Ok Dir_cap.Identity_changed ->
            Error (identity_changed_failure inspection.name))
    | Captured_directory captured -> (
        let* traversal = duplicate_directory Remove_tree captured in
        let action =
          let* children =
            enumerate_for_removal state traversal inspected_entry_depth
          in
          let rec remove_children = function
            | [] -> Ok ()
            | name :: rest ->
                let* depth = child_depth state inspected_entry_depth in
                let* () = remove_native_entry state traversal name depth in
                remove_children rest
          in
          remove_children children
        in
        let traversal_cleanup = cleanup_directory Remove_tree traversal in
        let* () =
          match (action, traversal_cleanup.issues) with
          | Ok (), [] -> Ok ()
          | Error failure, issues -> Error (append_issues failure issues)
          | Ok (), issues -> Error (failure_issues issues)
        in
        let* () = ensure_removal_count state in
        match
          Backend.remove_empty_directory_if_identity parent inspection.name
            ~expected:inspection.stat.identity
        with
        | Error error -> Error (operation_failure (map_error Remove_tree error))
        | Ok Dir_cap.Unlinked ->
            record_removed state;
            Ok ()
        | Ok Dir_cap.Absent -> Ok ()
        | Ok Dir_cap.Identity_changed ->
            Error (identity_changed_failure inspection.name))

  let remove_inspection (inspection : inspection_core) wrap ~budget =
    let state = traversal_state budget in
    if inspection.inspection_consumed || not (inspection_operational inspection)
    then
      let native_code =
        if inspection.inspection_consumed then "inspection-consumed"
        else "inspection-partially-closed"
      in
      let live_inspection =
        if inspection_cleanup_live inspection then Some (wrap inspection)
        else (
          inspection.inspection_consumed <- true;
          None)
      in
      Removal_incomplete
        {
          progress = traversal_progress state;
          live_inspection;
          failure =
            operation_failure
              (contract_error Remove_tree Closed_capability native_code);
        }
    else
      match validate_deletion_authority inspection with
      | Error failure ->
          Removal_incomplete
            {
              progress = traversal_progress state;
              live_inspection = Some (wrap inspection);
              failure;
            }
      | Ok () -> (
          match remove_captured_inspection state inspection with
          | Error failure ->
              Removal_incomplete
                {
                  progress = traversal_progress state;
                  live_inspection = Some (wrap inspection);
                  failure;
                }
          | Ok () ->
              inspection.inspection_consumed <- true;
              let cleanup = cleanup_inspection Remove_tree inspection in
              Removal_complete
                {
                  progress = traversal_progress state;
                  advisories = cleanup.issues;
                })

  let remove_tree (Inspected_path inspection) ~budget =
    remove_inspection inspection (fun value -> Inspected_path value) ~budget

  let remove_inspected_tree (Inspected_entry inspection) ~budget =
    remove_inspection inspection (fun value -> Inspected_entry value) ~budget

  let try_lock (root : root) relative mode =
    let* () = require_owner Try_lock "root" root.owner in
    let* parent, name =
      resolve_parent_from_base Try_lock root.directory relative
    in
    let backend_mode =
      match mode with
      | Shared -> Dir_cap.Shared
      | Exclusive -> Dir_cap.Exclusive
    in
    match Backend.try_lock parent.capability name backend_mode with
    | Error failure ->
        Error
          (append_issues
             (map_backend_failure Try_lock failure)
             (cleanup_directory Try_lock parent).issues)
    | Ok `Busy ->
        Ok
          {
            disposition = `Busy;
            advisories = (cleanup_directory Try_lock parent).issues;
          }
    | Ok (`Acquired backend_lock) ->
        let lock =
          {
            backend_lock = held backend_lock;
            owner = Backend.lock_owner backend_lock;
          }
        in
        Ok
          {
            disposition = `Acquired lock;
            advisories = (cleanup_directory Try_lock parent).issues;
          }

  let lock_owner (lock : lock) = lock.owner

  let release_lock (lock : lock) =
    let cleanup = cleanup_lock Release_lock lock.backend_lock in
    teardown lock ~live:(fun () -> is_open lock.backend_lock) cleanup
end
