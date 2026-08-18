module type NATIVE_NAME = sig
  type t

  val equal : t -> t -> bool
  val encode : t -> string
  val pp : Format.formatter -> t -> unit
end

module type IDENTITY = sig
  type t

  val equal : t -> t -> bool
  val encode : t -> string
  val pp : Format.formatter -> t -> unit
end

type operation =
  | Name_of_component
  | Probe_path
  | Relate_paths
  | Establish_separation
  | Materialize
  | Enumerate
  | Probe_entry
  | Open_directory
  | Duplicate_directory
  | Open_file
  | Open_file_for_publish
  | Read_file
  | Read_link
  | Create_directory
  | Create_file
  | Create_symlink
  | Chmod_directory
  | Chmod_file
  | Atomic_rename
  | Conditional_unlink
  | Unlink_link
  | Remove_empty_directory
  | Try_lock
  | Close_probe
  | Close_directory
  | Close_file
  | Close_separation
  | Release_lock

let operation_name = function
  | Name_of_component -> "name-of-component"
  | Probe_path -> "probe-path"
  | Relate_paths -> "relate-paths"
  | Establish_separation -> "establish-separation"
  | Materialize -> "materialize"
  | Enumerate -> "enumerate"
  | Probe_entry -> "probe-entry"
  | Open_directory -> "open-directory"
  | Duplicate_directory -> "duplicate-directory"
  | Open_file -> "open-file"
  | Open_file_for_publish -> "open-file-for-publish"
  | Read_file -> "read-file"
  | Read_link -> "read-link"
  | Create_directory -> "create-directory"
  | Create_file -> "create-file"
  | Create_symlink -> "create-symlink"
  | Chmod_directory -> "chmod-directory"
  | Chmod_file -> "chmod-file"
  | Atomic_rename -> "atomic-rename"
  | Conditional_unlink -> "conditional-unlink"
  | Unlink_link -> "unlink-link"
  | Remove_empty_directory -> "remove-empty-directory"
  | Try_lock -> "try-lock"
  | Close_probe -> "close-probe"
  | Close_directory -> "close-directory"
  | Close_file -> "close-file"
  | Close_separation -> "close-separation"
  | Release_lock -> "release-lock"

type error_class =
  | Missing
  | Already_exists
  | Not_directory
  | Not_regular
  | Not_link
  | Link_like
  | Too_large
  | Invalid_name
  | Access_denied
  | Busy
  | Unsupported
  | Wrong_process
  | Closed_capability
  | Other

type native_domain = Posix_errno | Win32 | Ntstatus | In_memory | Contract

type error = {
  operation : operation;
  class_ : error_class;
  native_domain : native_domain;
  native_code : string;
  component : string option;
}

let make_error ~operation ~class_ ~native_domain ~native_code ?component () =
  { operation; class_; native_domain; native_code; component }

type relationship =
  | Same
  | Left_contains_right
  | Right_contains_left
  | Separate

type entry_kind = Directory | Regular | Symbolic_link | Other_entry
type materialization = Already_present | Newly_created
type replacement = No_replace | Replace
type lock_mode = Shared | Exclusive

type atomic_rename_result =
  | Not_published of error
  | Published of { advisories : error list }

type conditional_unlink_result = Unlinked | Absent | Identity_changed
type local_handle_state = Still_open | Closed | Invalidated_unknown

type cleanup_problem = {
  error : error;
  local_handle_state : local_handle_state;
  namespace_released : bool;
}

type cleanup_failure = {
  primary : cleanup_problem;
  suppressed : cleanup_problem list;
}

type cleanup_result =
  | Cleanup_complete
  | Cleanup_local_only
  | Cleanup_failed of cleanup_failure

type issue = Operation_error of error | Cleanup_error of cleanup_failure
type failure = { primary : issue; suppressed : issue list }

type deletion_progress = {
  local_handle_state : local_handle_state;
  namespace_released : bool;
}

type conditional_delete_outcome =
  | Deletion_not_committed of error
  | Deletion_complete of conditional_unlink_result
  | Deletion_incomplete of { progress : deletion_progress; failure : failure }

let issue_error = function
  | Operation_error error -> error
  | Cleanup_error failure -> failure.primary.error

let failure_of_error error =
  { primary = Operation_error error; suppressed = [] }

type permission_error = Permission_out_of_range of int

module Permissions = struct
  type t = int

  let of_int value =
    if value < 0 || value > 0o7777 then Error (Permission_out_of_range value)
    else Ok value

  let to_int value = value
  let of_native value = value
  let owner_read_write = 0o600
  let owner_private_directory = 0o700
end

type enumeration_budget_error =
  | Negative_max_entries of int64
  | Negative_max_native_name_bytes of int64

let run_cleanup_in_order cleanup =
  let rec run completed = function
    | [] -> List.rev completed
    | next :: rest -> run (next () :: completed) rest
  in
  run [] cleanup

let cleanup_issues results =
  List.filter_map
    (function
      | Cleanup_complete | Cleanup_local_only -> None
      | Cleanup_failed failure -> Some (Cleanup_error failure))
    results

let resolve_cleanup action cleanup =
  let cleanup = cleanup_issues cleanup in
  match (action, cleanup) with
  | Ok value, [] -> Ok value
  | Error primary, suppressed ->
      Error { primary = Operation_error primary; suppressed }
  | Ok _, primary :: suppressed -> Error { primary; suppressed }

module type S = sig
  module Native_name : NATIVE_NAME
  module Identity : IDENTITY

  type owner
  type dir
  type file
  type path_probe
  type separation_witness
  type lock
  type enumeration_budget

  type stat = {
    identity : Identity.t;
    kind : entry_kind;
    size : int64;
    permissions : Permissions.t;
    mtime_ns : int64;
  }

  type captured_read = { contents : string; stat : stat }

  type creation_evidence =
    | Creation_observed of Native_name.t
    | Creation_may_have_committed of Native_name.t

  type materialized = {
    directory : dir;
    disposition : materialization;
    created : creation_evidence list;
    advisories : issue list;
  }

  type enumeration_consumption = { entries : int64; native_name_bytes : int64 }

  type enumeration_outcome =
    | Enumerated of {
        names : Native_name.t list;
        consumption : enumeration_consumption;
      }
    | Enumeration_incomplete of {
        consumption : enumeration_consumption;
        failure : failure;
      }

  type materialization_outcome =
    | Materialized of materialized
    | Materialization_incomplete of {
        created : creation_evidence list;
        failure : failure;
      }

  type 'cap creation_residual =
    | Captured of 'cap
    | Uncaptured of creation_evidence

  type 'cap creation_outcome =
    | Not_created of error
    | Created of 'cap
    | Creation_incomplete of {
        residual : 'cap creation_residual;
        failure : failure;
      }

  val current_owner : unit -> owner
  val owner_equal : owner -> owner -> bool
  val probe_owner : path_probe -> owner
  val dir_owner : dir -> owner
  val file_owner : file -> owner
  val lock_owner : lock -> owner
  val dir_identity : dir -> Identity.t
  val file_identity : file -> Identity.t
  val lock_directory_identity : lock -> Identity.t
  val lock_file_identity : lock -> Identity.t
  val probe_display : path_probe -> string
  val name_of_component : string -> (Native_name.t, error) result
  val probe_path : string -> (path_probe, failure) result
  val relationship : path_probe -> path_probe -> (relationship, error) result

  val establish_separation :
    forbidden:path_probe ->
    candidate:path_probe ->
    (separation_witness, failure) result

  val materialize :
    separation_witness -> permissions:Permissions.t -> materialization_outcome

  val enumeration_budget :
    max_entries:int64 ->
    max_native_name_bytes:int64 ->
    (enumeration_budget, enumeration_budget_error) result

  val enumerate_no_follow :
    dir -> budget:enumeration_budget -> enumeration_outcome

  val probe_entry_no_follow :
    dir -> Native_name.t -> (stat option, failure) result

  val open_directory_no_follow : dir -> Native_name.t -> (dir, failure) result

  val open_directory_for_delete_no_follow :
    dir -> Native_name.t -> (dir, failure) result

  val duplicate_directory : dir -> (dir, failure) result
  val open_file_no_follow : dir -> Native_name.t -> (file, failure) result

  val open_file_for_delete_no_follow :
    dir -> Native_name.t -> (file, failure) result

  val open_file_for_publish_no_follow :
    dir -> Native_name.t -> (file, failure) result

  val read_captured : file -> limit:int64 -> (captured_read, error) result

  val create_directory :
    dir -> Native_name.t -> permissions:Permissions.t -> dir creation_outcome

  val create_file :
    dir ->
    Native_name.t ->
    permissions:Permissions.t ->
    contents:string ->
    file creation_outcome

  val create_symlink :
    dir -> Native_name.t -> target:string -> (unit, error) result

  val chmod_directory : dir -> permissions:Permissions.t -> (unit, error) result
  val chmod_file : file -> permissions:Permissions.t -> (unit, error) result

  val atomic_rename :
    file ->
    into:dir ->
    as_:Native_name.t ->
    replacement:replacement ->
    atomic_rename_result

  val unlink_if_identity :
    dir ->
    Native_name.t ->
    expected:Identity.t ->
    (conditional_unlink_result, error) result

  val unlink_captured_file_if_identity :
    parent:dir -> file -> expected:Identity.t -> conditional_delete_outcome

  val unlink_link_no_follow :
    dir ->
    Native_name.t ->
    expected:Identity.t ->
    (conditional_unlink_result, error) result

  val remove_empty_directory_if_identity :
    dir ->
    Native_name.t ->
    expected:Identity.t ->
    (conditional_unlink_result, error) result

  val remove_captured_empty_directory_if_identity :
    parent:dir -> dir -> expected:Identity.t -> conditional_delete_outcome

  val try_lock :
    dir ->
    Native_name.t ->
    lock_mode ->
    ([ `Acquired of lock | `Busy ], failure) result

  val close_probe : path_probe -> cleanup_result
  val close_directory : dir -> cleanup_result
  val close_file : file -> cleanup_result
  val close_separation : separation_witness -> cleanup_result
  val release_lock : lock -> cleanup_result
end

module System = struct
  type raw_handle
  type raw_error = int * int * string
  type raw_stat = string * int * int64 * int * int64

  type raw_local_handle_state =
    | Raw_still_open
    | Raw_closed
    | Raw_invalidated_unknown

  type raw_issue =
    | Raw_operation_error of raw_error
    | Raw_cleanup_error of raw_error * raw_local_handle_state

  type raw_failure = {
    raw_primary : raw_issue;
    raw_suppressed : raw_issue list;
  }

  type raw_creation_failure =
    | Raw_not_committed of raw_error
    | Raw_may_have_committed of raw_failure

  type raw_deletion_failure =
    | Raw_delete_not_committed of raw_error
    | Raw_delete_may_have_committed of
        raw_local_handle_state * bool * raw_failure

  type raw_lock_cleanup_problem = raw_error * raw_local_handle_state * bool

  external raw_current_owner : unit -> int64 = "ocaml_mutants_dircap_owner"

  external raw_name_normalize : string -> (string, raw_error) result
    = "ocaml_mutants_dircap_name_valid"

  external raw_open_root :
    string -> (raw_handle * string list * string, raw_failure) result
    = "ocaml_mutants_dircap_open_root"

  external raw_duplicate : raw_handle -> (raw_handle, raw_error) result
    = "ocaml_mutants_dircap_duplicate"

  external raw_open_child :
    raw_handle -> string -> int -> (raw_handle * raw_stat, raw_failure) result
    = "ocaml_mutants_dircap_open_child"

  external raw_stat_handle : raw_handle -> (raw_stat, raw_error) result
    = "ocaml_mutants_dircap_stat_handle"

  external raw_probe_entry :
    raw_handle -> string -> (raw_stat option, raw_failure) result
    = "ocaml_mutants_dircap_probe_entry"

  external raw_enumerate :
    raw_handle ->
    int64 ->
    int64 ->
    (string list * int64 * int64, raw_failure * int64 * int64) result
    = "ocaml_mutants_dircap_enumerate"

  external raw_read :
    raw_handle -> int64 -> (string * raw_stat, raw_error) result
    = "ocaml_mutants_dircap_read"

  external raw_create_directory :
    raw_handle ->
    string ->
    int ->
    string ->
    (raw_handle * raw_stat, raw_creation_failure) result
    = "ocaml_mutants_dircap_create_directory"

  external raw_create_file :
    raw_handle ->
    string ->
    int ->
    string ->
    string ->
    (raw_handle * raw_stat, raw_creation_failure) result
    = "ocaml_mutants_dircap_create_file"

  external raw_delete_captured :
    raw_handle ->
    raw_handle ->
    string ->
    string ->
    string ->
    int ->
    (bool, raw_deletion_failure) result
    = "ocaml_mutants_dircap_delete_captured_byte"
      "ocaml_mutants_dircap_delete_captured"

  external raw_atomic_rename :
    raw_handle ->
    raw_handle ->
    string ->
    int ->
    string ->
    string ->
    (raw_error list, raw_error) result
    = "ocaml_mutants_dircap_atomic_rename_byte"
      "ocaml_mutants_dircap_atomic_rename"

  external raw_try_lock :
    raw_handle ->
    string ->
    int ->
    int ->
    string ->
    ((raw_handle * raw_stat) option, raw_failure) result
    = "ocaml_mutants_dircap_try_lock"

  external raw_release_lock :
    raw_handle ->
    bool ->
    (bool, raw_lock_cleanup_problem * raw_lock_cleanup_problem list) result
    = "ocaml_mutants_dircap_release_lock"

  external raw_close :
    raw_handle -> (bool, raw_error * raw_local_handle_state) result
    = "ocaml_mutants_dircap_close"

  external raw_close_terminal :
    raw_handle -> (bool, raw_error * raw_local_handle_state) result
    = "ocaml_mutants_dircap_close_terminal"

  module Native_name = struct
    type t = { raw : string; diagnostic : string }

    let ascii_hex value =
      let alphabet = "0123456789abcdef" in
      String.init
        (String.length value * 2)
        (fun index ->
          let byte = Char.code value.[index / 2] in
          if index mod 2 = 0 then alphabet.[byte lsr 4]
          else alphabet.[byte land 0x0f])

    let of_raw raw =
      let platform = if Sys.win32 then "windows-wtf8:" else "posix-bytes:" in
      if
        String.length raw > (Sys.max_string_length - String.length platform) / 2
      then None
      else Some { raw; diagnostic = platform ^ ascii_hex raw }

    let equal left right = String.equal left.raw right.raw
    let encode value = value.diagnostic
    let pp formatter value = Format.pp_print_string formatter value.diagnostic
  end

  module Identity = struct
    type t = string

    let equal = String.equal
    let encode value = value
    let pp = Format.pp_print_string
  end

  type owner = int64

  type dir = {
    handle : raw_handle;
    mutable handles : raw_handle list;
    mutable available : bool;
    owner : owner;
    identity : Identity.t;
    parent_identity : Identity.t option;
    deletion_authority : bool;
    leaf : string option;
        (* Raw capture name below the recorded parent; POSIX descriptor-verified
           deletion and publication source consumption resolve exactly this
           retained name, never a reconstructed path. *)
  }

  type file = {
    handle : raw_handle;
    mutable available : bool;
    owner : owner;
    identity : Identity.t;
    parent_identity : Identity.t;
    deletion_authority : bool;
    leaf : string option;
  }

  type path_probe = {
    mutable handles : raw_handle list;
    mutable available : bool;
    owner : owner;
    existing_identity : Identity.t;
    identity_chain : Identity.t list;
    missing : Native_name.t list;
    display : string;
  }

  type separation_basis =
    | Identity_disjoint
    | Exclusive_missing_leaf_below_forbidden_ancestor

  type separation_witness = {
    mutable forbidden_handles : raw_handle list;
    mutable candidate_handles : raw_handle list;
    owner : owner;
    forbidden_identities : Identity.t list;
    candidate_identity : Identity.t;
    candidate_missing : Native_name.t list;
    basis : separation_basis;
    mutable materialization_available : bool;
  }

  type lock = {
    handle : raw_handle;
    owner : owner;
    directory_identity : Identity.t;
    file_identity : Identity.t;
    mutable available : bool;
    mutable held : bool;
  }

  type enumeration_budget = {
    max_entries : int64;
    max_native_name_bytes : int64;
  }

  type stat = {
    identity : Identity.t;
    kind : entry_kind;
    size : int64;
    permissions : Permissions.t;
    mtime_ns : int64;
  }

  type captured_read = { contents : string; stat : stat }

  type creation_evidence =
    | Creation_observed of Native_name.t
    | Creation_may_have_committed of Native_name.t

  type materialized = {
    directory : dir;
    disposition : materialization;
    created : creation_evidence list;
    advisories : issue list;
  }

  type enumeration_consumption = { entries : int64; native_name_bytes : int64 }

  type enumeration_outcome =
    | Enumerated of {
        names : Native_name.t list;
        consumption : enumeration_consumption;
      }
    | Enumeration_incomplete of {
        consumption : enumeration_consumption;
        failure : failure;
      }

  type materialization_outcome =
    | Materialized of materialized
    | Materialization_incomplete of {
        created : creation_evidence list;
        failure : failure;
      }

  type 'cap creation_residual =
    | Captured of 'cap
    | Uncaptured of creation_evidence

  type 'cap creation_outcome =
    | Not_created of error
    | Created of 'cap
    | Creation_incomplete of {
        residual : 'cap creation_residual;
        failure : failure;
      }

  let ( let* ) value continuation = Result.bind value continuation

  let native_domain = function
    | 0 -> Posix_errno
    | 1 -> Win32
    | 2 -> Ntstatus
    | _ -> Contract

  let error_class = function
    | 0 -> Missing
    | 1 -> Already_exists
    | 2 -> Not_directory
    | 3 -> Not_regular
    | 4 -> Not_link
    | 5 -> Link_like
    | 6 -> Too_large
    | 7 -> Invalid_name
    | 8 -> Access_denied
    | 9 -> Busy
    | 10 -> Unsupported
    | 11 -> Wrong_process
    | 12 -> Closed_capability
    | _ -> Other

  let decode_error ?component operation (domain, class_, native_code) =
    make_error ~operation ~class_:(error_class class_)
      ~native_domain:(native_domain domain) ~native_code ?component ()

  let lift ?component operation = function
    | Ok value -> Ok value
    | Error problem -> Error (decode_error ?component operation problem)

  let decode_raw_handle_state = function
    | Raw_still_open -> Still_open
    | Raw_closed -> Closed
    | Raw_invalidated_unknown -> Invalidated_unknown

  let decode_raw_issue ?component ~operation ~cleanup_operation = function
    | Raw_operation_error problem ->
        Operation_error (decode_error ?component operation problem)
    | Raw_cleanup_error (problem, local_handle_state) ->
        Cleanup_error
          {
            primary =
              {
                error = decode_error ?component cleanup_operation problem;
                local_handle_state = decode_raw_handle_state local_handle_state;
                namespace_released = false;
              };
            suppressed = [];
          }

  let decode_raw_failure ?component ?cleanup_operation operation
      { raw_primary; raw_suppressed } =
    let cleanup_operation = Option.value cleanup_operation ~default:operation in
    {
      primary =
        decode_raw_issue ?component ~operation ~cleanup_operation raw_primary;
      suppressed =
        List.map
          (decode_raw_issue ?component ~operation ~cleanup_operation)
          raw_suppressed;
    }

  let lift_raw_failure ?component ?cleanup_operation operation = function
    | Ok value -> Ok value
    | Error failure ->
        Error
          (decode_raw_failure ?component ?cleanup_operation operation failure)

  let as_failure = function
    | Ok value -> Ok value
    | Error primary -> Error (failure_of_error primary)

  let current_owner = raw_current_owner
  let owner_equal = Int64.equal
  let probe_owner (value : path_probe) = value.owner
  let dir_owner (value : dir) = value.owner
  let file_owner (value : file) = value.owner
  let lock_owner (value : lock) = value.owner
  let dir_identity (value : dir) = value.identity
  let file_identity (value : file) = value.identity
  let lock_directory_identity (value : lock) = value.directory_identity
  let lock_file_identity (value : lock) = value.file_identity
  let probe_display (value : path_probe) = value.display

  let wrong_process operation =
    make_error ~operation ~class_:Wrong_process ~native_domain:Contract
      ~native_code:"capability-owner-pid-mismatch" ()

  let check_owner operation owner =
    if owner_equal owner (current_owner ()) then Ok ()
    else Error (wrong_process operation)

  let closed_capability operation code =
    make_error ~operation ~class_:Closed_capability ~native_domain:Contract
      ~native_code:code ()

  let check_directory operation (directory : dir) =
    let* () = check_owner operation directory.owner in
    if directory.available then Ok ()
    else Error (closed_capability operation "directory-cleanup-only")

  let check_file operation (file : file) =
    let* () = check_owner operation file.owner in
    if file.available then Ok ()
    else Error (closed_capability operation "file-cleanup-only")

  let check_probe operation (probe : path_probe) =
    let* () = check_owner operation probe.owner in
    if probe.available then Ok ()
    else Error (closed_capability operation "probe-capability-consumed")

  let unsupported ?component operation =
    Error
      (make_error ~operation ~class_:Unsupported ~native_domain:Contract
         ~native_code:"native-primitive-not-implemented" ?component ())

  let stat_of_raw (identity, kind, size, permissions, mtime_ns) =
    let kind =
      match kind with
      | 0 -> Directory
      | 1 -> Regular
      | 2 -> Symbolic_link
      | _ -> Other_entry
    in
    {
      identity;
      kind;
      size;
      permissions = Permissions.of_native permissions;
      mtime_ns;
    }

  let name_of_component value =
    match
      lift ~component:value Name_of_component (raw_name_normalize value)
    with
    | Ok normalized -> (
        match Native_name.of_raw normalized with
        | Some name -> Ok name
        | None ->
            Error
              (make_error ~operation:Name_of_component ~class_:Invalid_name
                 ~native_domain:Contract
                 ~native_code:"native-name-diagnostic-too-large"
                 ~component:value ()))
    | Error _ as problem -> problem

  let close_raw_using close operation handle =
    match close handle with
    | Ok false -> Cleanup_complete
    | Ok true -> Cleanup_local_only
    | Error (problem, raw_state) ->
        let local_handle_state =
          match raw_state with
          | Raw_still_open -> Still_open
          | Raw_closed -> Closed
          | Raw_invalidated_unknown -> Invalidated_unknown
        in
        Cleanup_failed
          {
            primary =
              {
                error = decode_error operation problem;
                local_handle_state;
                namespace_released = false;
              };
            suppressed = [];
          }

  let close_raw operation handle = close_raw_using raw_close operation handle

  let close_raw_terminal operation handle =
    close_raw_using raw_close_terminal operation handle

  let close_handles operation handles =
    let rec loop problems saw_local remaining = function
      | [] ->
          let result =
            match List.rev problems with
            | [] -> if saw_local then Cleanup_local_only else Cleanup_complete
            | primary :: suppressed -> Cleanup_failed { primary; suppressed }
          in
          (result, List.rev remaining)
      | handle :: rest -> (
          match close_raw operation handle with
          | Cleanup_complete -> loop problems saw_local remaining rest
          | Cleanup_local_only -> loop problems true remaining rest
          | Cleanup_failed { primary; suppressed } ->
              let ordered = primary :: suppressed in
              let retryable =
                List.exists
                  (fun (problem : cleanup_problem) ->
                    problem.local_handle_state = Still_open)
                  ordered
              in
              loop
                (List.rev_append ordered problems)
                saw_local
                (if retryable then handle :: remaining else remaining)
                rest)
    in
    loop [] false [] handles

  let close_handles_terminal operation handles =
    let terminal (problem : cleanup_problem) =
      if problem.local_handle_state = Still_open then
        { problem with local_handle_state = Invalidated_unknown }
      else problem
    in
    let rec loop problems saw_local = function
      | [] -> (
          match List.rev problems with
          | [] -> if saw_local then Cleanup_local_only else Cleanup_complete
          | primary :: suppressed -> Cleanup_failed { primary; suppressed })
      | handle :: rest -> (
          match close_raw_terminal operation handle with
          | Cleanup_complete -> loop problems saw_local rest
          | Cleanup_local_only -> loop problems true rest
          | Cleanup_failed { primary; suppressed } ->
              let ordered = List.map terminal (primary :: suppressed) in
              loop (List.rev_append ordered problems) saw_local rest)
    in
    loop [] false handles

  let close_all operation handles = fst (close_handles operation handles)

  let display_path root components =
    List.fold_left
      (fun display component ->
        Filename.concat display component.Native_name.raw)
      root components

  let probe_path path =
    let* root_handle, components, root_display =
      lift_raw_failure ~cleanup_operation:Close_probe Probe_path
        (raw_open_root path)
    in
    let owner = current_owner () in
    let fail handles failure =
      match close_handles_terminal Close_probe handles with
      | Cleanup_complete | Cleanup_local_only -> Error failure
      | Cleanup_failed cleanup ->
          Error
            {
              failure with
              suppressed = failure.suppressed @ [ Cleanup_error cleanup ];
            }
    in
    match lift Probe_path (raw_stat_handle root_handle) with
    | Error problem -> fail [ root_handle ] (failure_of_error problem)
    | Ok root_stat -> (
        let root_stat = stat_of_raw root_stat in
        let rec decode_components decoded = function
          | [] -> Ok (List.rev decoded)
          | raw :: rest -> (
              match Native_name.of_raw raw with
              | Some name -> decode_components (name :: decoded) rest
              | None ->
                  Error
                    (make_error ~operation:Probe_path ~class_:Invalid_name
                       ~native_domain:Contract
                       ~native_code:"native-name-diagnostic-too-large" ()))
        in
        let components = decode_components [] components in
        match components with
        | Error problem -> fail [ root_handle ] (failure_of_error problem)
        | Ok components ->
            let rec walk handles identities current = function
              | [] ->
                  Ok
                    {
                      handles;
                      available = true;
                      owner;
                      existing_identity = List.hd identities;
                      identity_chain = List.rev identities;
                      missing = [];
                      display = display_path root_display components;
                    }
              | component :: rest -> (
                  match
                    lift_raw_failure
                      ~component:(Native_name.encode component)
                      ~cleanup_operation:Close_probe Open_directory
                      (raw_open_child current component.Native_name.raw 0)
                  with
                  | Ok (child, child_stat) ->
                      let child_stat = stat_of_raw child_stat in
                      walk (child :: handles)
                        (child_stat.identity :: identities)
                        child rest
                  | Error
                      {
                        primary = Operation_error { class_ = Missing; _ };
                        suppressed = [];
                      } ->
                      Ok
                        {
                          handles;
                          available = true;
                          owner;
                          existing_identity = List.hd identities;
                          identity_chain = List.rev identities;
                          missing = component :: rest;
                          display = display_path root_display components;
                        }
                  | Error failure -> fail handles failure)
            in
            walk [ root_handle ] [ root_stat.identity ] root_handle components)

  let list_prefix ~equal left right =
    let rec loop left right =
      match (left, right) with
      | [], rest -> Some rest
      | _ :: _, [] -> None
      | left :: left_rest, right :: right_rest when equal left right ->
          loop left_rest right_rest
      | _ -> None
    in
    loop left right

  let identity_occurrences identity chain =
    List.fold_left
      (fun count candidate ->
        if Identity.equal identity candidate then count + 1 else count)
      0 chain

  let exclusive_missing_leaf_separation (forbidden : path_probe)
      (candidate : path_probe) =
    forbidden.missing = []
    && (match candidate.missing with
      | [ _ ] -> true
      | [] | _ :: _ :: _ -> false)
    && (not
          (Identity.equal forbidden.existing_identity
             candidate.existing_identity))
    && identity_occurrences candidate.existing_identity forbidden.identity_chain
       = 1
    && identity_occurrences forbidden.existing_identity candidate.identity_chain
       = 0

  let relationship (left : path_probe) (right : path_probe) =
    let* () = check_probe Relate_paths left in
    let* () = check_probe Relate_paths right in
    let left_deepest = left.existing_identity in
    let right_deepest = right.existing_identity in
    let left_in_right =
      identity_occurrences left_deepest right.identity_chain
    in
    let right_in_left =
      identity_occurrences right_deepest left.identity_chain
    in
    let same_deepest = Identity.equal left_deepest right_deepest in
    let ambiguous () =
      Error
        (make_error ~operation:Relate_paths ~class_:Unsupported
           ~native_domain:Contract
           ~native_code:"ambiguous-identity-relationship" ())
    in
    if
      left_in_right > 1 || right_in_left > 1
      || ((not same_deepest) && left_in_right = 1 && right_in_left = 1)
    then ambiguous ()
    else if same_deepest then
      match (left.missing, right.missing) with
      | [], [] -> Ok Same
      | [], _ -> Ok Left_contains_right
      | _, [] -> Ok Right_contains_left
      | _ -> (
          match
            ( list_prefix
                ~equal:(fun left right ->
                  String.equal left.Native_name.raw right.Native_name.raw)
                left.missing right.missing,
              list_prefix
                ~equal:(fun left right ->
                  String.equal left.Native_name.raw right.Native_name.raw)
                right.missing left.missing )
          with
          | Some [], Some [] -> Ok Same
          | Some _, None -> Ok Left_contains_right
          | None, Some _ -> Ok Right_contains_left
          | None, None when Sys.win32 ->
              Error
                (make_error ~operation:Relate_paths ~class_:Unsupported
                   ~native_domain:Contract
                   ~native_code:"windows-unresolved-name-alias-unproven" ())
          | _ -> Ok Separate)
    else if left.missing = [] && left_in_right = 1 then Ok Left_contains_right
    else if right.missing = [] && right_in_left = 1 then Ok Right_contains_left
    else if left_in_right = 1 || right_in_left = 1 then ambiguous ()
    else Ok Separate

  let establish_separation ~(forbidden : path_probe) ~(candidate : path_probe) =
    let result =
      let* () = check_probe Establish_separation forbidden in
      let* () = check_probe Establish_separation candidate in
      let* basis =
        if exclusive_missing_leaf_separation forbidden candidate then
          Ok Exclusive_missing_leaf_below_forbidden_ancestor
        else
          let* relationship = relationship forbidden candidate in
          if relationship = Separate then Ok Identity_disjoint
          else
            Error
              (make_error ~operation:Establish_separation ~class_:Unsupported
                 ~native_domain:Contract
                 ~native_code:"paths-not-proven-separate" ())
      in
      Ok
        {
          forbidden_handles = forbidden.handles;
          candidate_handles = candidate.handles;
          owner = candidate.owner;
          forbidden_identities = forbidden.identity_chain;
          candidate_identity = candidate.existing_identity;
          candidate_missing = candidate.missing;
          basis;
          materialization_available = true;
        }
    in
    match result with
    | Error error -> Error (failure_of_error error)
    | Ok witness ->
        forbidden.handles <- [];
        candidate.handles <- [];
        forbidden.available <- false;
        candidate.available <- false;
        Ok witness

  let materialize (witness : separation_witness) ~permissions =
    let close_witness () =
      let candidate_result, candidate_remaining =
        close_handles Close_separation witness.candidate_handles
      in
      witness.candidate_handles <- candidate_remaining;
      let forbidden_result, forbidden_remaining =
        close_handles Close_separation witness.forbidden_handles
      in
      witness.forbidden_handles <- forbidden_remaining;
      [ candidate_result; forbidden_result ]
    in
    let candidate_is_forbidden () =
      List.exists
        (Identity.equal witness.candidate_identity)
        witness.forbidden_identities
    in
    let forbidden_component_failure name handle =
      let failure =
        failure_of_error
          (make_error ~operation:Materialize ~class_:Unsupported
             ~native_domain:Contract
             ~native_code:"created-component-entered-forbidden-identity-chain"
             ~component:(Native_name.encode name) ())
      in
      let cleanup = close_raw_terminal Close_directory handle in
      {
        failure with
        suppressed = failure.suppressed @ cleanup_issues [ cleanup ];
      }
    in
    let action =
      match check_owner Materialize witness.owner with
      | Error error -> Error ([], failure_of_error error)
      | Ok () ->
          if not witness.materialization_available then
            Error
              ( [],
                failure_of_error
                  (closed_capability Materialize
                     "separation-witness-materialization-consumed") )
          else (
            witness.materialization_available <- false;
            match witness.candidate_handles with
            | [] ->
                Error
                  ( [],
                    failure_of_error
                      (closed_capability Materialize
                         "separation-witness-candidate-handles-missing") )
            | existing :: _ -> (
                if
                  candidate_is_forbidden () && witness.basis = Identity_disjoint
                then
                  Error
                    ( [],
                      failure_of_error
                        (make_error ~operation:Materialize ~class_:Unsupported
                           ~native_domain:Contract
                           ~native_code:
                             "candidate-entered-forbidden-identity-chain"
                           ()) )
                else
                  match witness.candidate_missing with
                  | [] -> (
                      match lift Materialize (raw_duplicate existing) with
                      | Error error -> Error ([], failure_of_error error)
                      | Ok handle ->
                          Ok
                            ( {
                                handle;
                                handles = [ handle ];
                                available = true;
                                owner = witness.owner;
                                identity = witness.candidate_identity;
                                parent_identity = None;
                                deletion_authority = false;
                                leaf = None;
                              },
                              Already_present,
                              [] ))
                  | [ name ] -> (
                      let component = Native_name.encode name in
                      match
                        raw_create_directory existing name.Native_name.raw
                          (Permissions.to_int permissions)
                          witness.candidate_identity
                      with
                      | Error (Raw_not_committed problem) ->
                          Error
                            ( [],
                              failure_of_error
                                (decode_error ~component Materialize problem) )
                      | Error (Raw_may_have_committed failure) ->
                          Error
                            ( [ Creation_may_have_committed name ],
                              decode_raw_failure ~component
                                ~cleanup_operation:Close_directory Materialize
                                failure )
                      | Ok (handle, raw_stat) ->
                          let stat = stat_of_raw raw_stat in
                          if
                            List.exists
                              (Identity.equal stat.identity)
                              witness.forbidden_identities
                          then
                            Error
                              ( [ Creation_observed name ],
                                forbidden_component_failure name handle )
                          else
                            Ok
                              ( {
                                  handle;
                                  handles = [ handle ];
                                  available = true;
                                  owner = witness.owner;
                                  identity = stat.identity;
                                  parent_identity =
                                    Some witness.candidate_identity;
                                  deletion_authority = true;
                                  leaf = Some name.Native_name.raw;
                                },
                                Newly_created,
                                [ Creation_observed name ] ))
                  | _ :: _ :: _ ->
                      Error
                        ( [],
                          failure_of_error
                            (make_error ~operation:Materialize
                               ~class_:Unsupported ~native_domain:Contract
                               ~native_code:
                                 "multi-component-materialization-unsupported"
                               ()) )))
    in
    let cleanup = close_witness () in
    match action with
    | Ok (directory, disposition, created) ->
        Materialized
          {
            directory;
            disposition;
            created;
            advisories = cleanup_issues cleanup;
          }
    | Error (created, failure) ->
        Materialization_incomplete
          {
            created;
            failure =
              {
                failure with
                suppressed = failure.suppressed @ cleanup_issues cleanup;
              };
          }

  let enumeration_budget ~max_entries ~max_native_name_bytes =
    if Int64.compare max_entries 0L < 0 then
      Error (Negative_max_entries max_entries)
    else if Int64.compare max_native_name_bytes 0L < 0 then
      Error (Negative_max_native_name_bytes max_native_name_bytes)
    else Ok { max_entries; max_native_name_bytes }

  let enumerate_no_follow (directory : dir) ~budget =
    let empty = { entries = 0L; native_name_bytes = 0L } in
    let incomplete consumption failure =
      Enumeration_incomplete { consumption; failure }
    in
    match check_directory Enumerate directory with
    | Error error -> incomplete empty (failure_of_error error)
    | Ok () -> (
        match
          raw_enumerate directory.handle budget.max_entries
            budget.max_native_name_bytes
        with
        | Error (failure, entries, native_name_bytes) ->
            incomplete
              { entries; native_name_bytes }
              (decode_raw_failure Enumerate failure)
        | Ok (raw_names, raw_entries, raw_native_name_bytes) ->
            let rec convert converted consumption = function
              | [] ->
                  if
                    consumption.entries <> raw_entries
                    || consumption.native_name_bytes <> raw_native_name_bytes
                  then
                    incomplete consumption
                      (failure_of_error
                         (make_error ~operation:Enumerate ~class_:Other
                            ~native_domain:Contract
                            ~native_code:
                              "native-enumeration-consumption-mismatch"
                            ()))
                  else Enumerated { names = List.rev converted; consumption }
              | raw :: rest -> (
                  let native_name_bytes = Int64.of_int (String.length raw) in
                  if
                    Int64.compare consumption.entries budget.max_entries >= 0
                    || Int64.compare native_name_bytes
                         (Int64.sub budget.max_native_name_bytes
                            consumption.native_name_bytes)
                       > 0
                  then
                    incomplete consumption
                      (failure_of_error
                         (make_error ~operation:Enumerate ~class_:Too_large
                            ~native_domain:Contract
                            ~native_code:"enumeration-budget-exhausted" ()))
                  else
                    match Native_name.of_raw raw with
                    | None ->
                        incomplete consumption
                          (failure_of_error
                             (make_error ~operation:Enumerate
                                ~class_:Invalid_name ~native_domain:Contract
                                ~native_code:"native-name-diagnostic-too-large"
                                ()))
                    | Some name ->
                        convert (name :: converted)
                          {
                            entries = Int64.succ consumption.entries;
                            native_name_bytes =
                              Int64.add consumption.native_name_bytes
                                native_name_bytes;
                          }
                          rest)
            in
            convert [] empty raw_names)

  let probe_entry_no_follow (directory : dir) name =
    let* () = as_failure (check_directory Probe_entry directory) in
    let* stat =
      lift_raw_failure ~component:(Native_name.encode name) Probe_entry
        (raw_probe_entry directory.handle name.Native_name.raw)
    in
    Ok (Option.map stat_of_raw stat)

  let open_directory_no_follow (directory : dir) name =
    let* () = as_failure (check_directory Open_directory directory) in
    let* handle, stat =
      lift_raw_failure ~component:(Native_name.encode name)
        ~cleanup_operation:Close_directory Open_directory
        (raw_open_child directory.handle name.Native_name.raw 0)
    in
    let stat = stat_of_raw stat in
    Ok
      {
        handle;
        handles = [ handle ];
        available = true;
        owner = directory.owner;
        identity = stat.identity;
        parent_identity = Some directory.identity;
        deletion_authority = false;
        leaf = None;
      }

  let open_directory_for_delete_no_follow (directory : dir) name =
    let* () = as_failure (check_directory Open_directory directory) in
    let* handle, stat =
      lift_raw_failure ~component:(Native_name.encode name)
        ~cleanup_operation:Close_directory Open_directory
        (raw_open_child directory.handle name.Native_name.raw 5)
    in
    let stat = stat_of_raw stat in
    Ok
      {
        handle;
        handles = [ handle ];
        available = true;
        owner = directory.owner;
        identity = stat.identity;
        parent_identity = Some directory.identity;
        deletion_authority = true;
        leaf = Some name.Native_name.raw;
      }

  let duplicate_directory (directory : dir) =
    as_failure
      (let* () = check_directory Duplicate_directory directory in
       let* handle =
         lift Duplicate_directory (raw_duplicate directory.handle)
       in
       Ok
         {
           handle;
           handles = [ handle ];
           available = true;
           owner = directory.owner;
           identity = directory.identity;
           parent_identity = directory.parent_identity;
           deletion_authority = directory.deletion_authority;
           leaf = directory.leaf;
         })

  let open_file_no_follow (directory : dir) name =
    let* () = as_failure (check_directory Open_file directory) in
    let* handle, stat =
      lift_raw_failure ~component:(Native_name.encode name)
        ~cleanup_operation:Close_file Open_file
        (raw_open_child directory.handle name.Native_name.raw 1)
    in
    let stat = stat_of_raw stat in
    Ok
      {
        handle;
        available = true;
        owner = directory.owner;
        identity = stat.identity;
        parent_identity = directory.identity;
        deletion_authority = false;
        leaf = None;
      }

  let open_file_for_delete_no_follow (directory : dir) name =
    let* () = as_failure (check_directory Open_file directory) in
    let* handle, stat =
      lift_raw_failure ~component:(Native_name.encode name)
        ~cleanup_operation:Close_file Open_file
        (raw_open_child directory.handle name.Native_name.raw 4)
    in
    let stat = stat_of_raw stat in
    Ok
      {
        handle;
        available = true;
        owner = directory.owner;
        identity = stat.identity;
        parent_identity = directory.identity;
        deletion_authority = true;
        leaf = Some name.Native_name.raw;
      }

  let open_file_for_publish_no_follow (directory : dir) name =
    let* () = as_failure (check_directory Open_file_for_publish directory) in
    let* handle, stat =
      lift_raw_failure ~component:(Native_name.encode name)
        ~cleanup_operation:Close_file Open_file_for_publish
        (raw_open_child directory.handle name.Native_name.raw 3)
    in
    let stat = stat_of_raw stat in
    Ok
      {
        handle;
        available = true;
        owner = directory.owner;
        identity = stat.identity;
        parent_identity = directory.identity;
        deletion_authority = false;
        leaf = Some name.Native_name.raw;
      }

  let read_captured (file : file) ~limit =
    let* () = check_file Read_file file in
    if Int64.compare limit 0L < 0 then
      Error
        (make_error ~operation:Read_file ~class_:Invalid_name
           ~native_domain:Contract ~native_code:"negative-read-limit" ())
    else
      let* contents, stat = lift Read_file (raw_read file.handle limit) in
      Ok { contents; stat = stat_of_raw stat }

  let create_directory (directory : dir) name ~permissions =
    match check_directory Create_directory directory with
    | Error problem -> Not_created problem
    | Ok () -> (
        let component = Native_name.encode name in
        match
          raw_create_directory directory.handle name.Native_name.raw
            (Permissions.to_int permissions)
            directory.identity
        with
        | Ok (handle, raw_stat) ->
            let stat = stat_of_raw raw_stat in
            Created
              {
                handle;
                handles = [ handle ];
                available = true;
                owner = directory.owner;
                identity = stat.identity;
                parent_identity = Some directory.identity;
                deletion_authority = true;
                leaf = Some name.Native_name.raw;
              }
        | Error (Raw_not_committed problem) ->
            Not_created (decode_error ~component Create_directory problem)
        | Error (Raw_may_have_committed failure) ->
            Creation_incomplete
              {
                residual = Uncaptured (Creation_may_have_committed name);
                failure =
                  decode_raw_failure ~component
                    ~cleanup_operation:Close_directory Create_directory failure;
              })

  let create_file (directory : dir) name ~permissions ~contents =
    match check_directory Create_file directory with
    | Error problem -> Not_created problem
    | Ok () -> (
        let component = Native_name.encode name in
        match
          raw_create_file directory.handle name.Native_name.raw
            (Permissions.to_int permissions)
            contents directory.identity
        with
        | Ok (handle, raw_stat) ->
            let stat = stat_of_raw raw_stat in
            Created
              {
                handle;
                available = true;
                owner = directory.owner;
                identity = stat.identity;
                parent_identity = directory.identity;
                deletion_authority = true;
                leaf = Some name.Native_name.raw;
              }
        | Error (Raw_not_committed problem) ->
            Not_created (decode_error ~component Create_file problem)
        | Error (Raw_may_have_committed failure) ->
            Creation_incomplete
              {
                residual = Uncaptured (Creation_may_have_committed name);
                failure =
                  decode_raw_failure ~component ~cleanup_operation:Close_file
                    Create_file failure;
              })

  let create_symlink (directory : dir) name ~target:_ =
    let* () = check_directory Create_symlink directory in
    unsupported ~component:(Native_name.encode name) Create_symlink

  let chmod_directory (directory : dir) ~permissions:_ =
    let* () = check_directory Chmod_directory directory in
    unsupported Chmod_directory

  let chmod_file (file : file) ~permissions:_ =
    let* () = check_file Chmod_file file in
    unsupported Chmod_file

  let atomic_rename (file : file) ~(into : dir) ~as_ ~replacement =
    match check_file Atomic_rename file with
    | Error problem -> Not_published problem
    | Ok () -> (
        match check_directory Atomic_rename into with
        | Error problem -> Not_published problem
        | Ok () when not (Identity.equal file.parent_identity into.identity) ->
            Not_published
              (make_error ~operation:Atomic_rename ~class_:Unsupported
                 ~native_domain:Contract
                 ~native_code:"cross-directory-publication-not-supported"
                 ~component:(Native_name.encode as_) ())
        | Ok () -> (
            let raw_replacement =
              match replacement with No_replace -> 0 | Replace -> 1
            in
            match
              raw_atomic_rename file.handle into.handle as_.Native_name.raw
                raw_replacement into.identity
                (Option.value file.leaf ~default:"")
            with
            | Error problem ->
                Not_published
                  (decode_error ~component:(Native_name.encode as_)
                     Atomic_rename problem)
            | Ok advisories ->
                Published
                  {
                    advisories =
                      List.map
                        (decode_error ~component:(Native_name.encode as_)
                           Atomic_rename)
                        advisories;
                  }))

  let unlink_if_identity (directory : dir) name ~expected:_ =
    let* () = check_directory Conditional_unlink directory in
    unsupported ~component:(Native_name.encode name) Conditional_unlink

  let deletion_contract_error operation code =
    make_error ~operation ~class_:Unsupported ~native_domain:Contract
      ~native_code:code ()

  let cleanup_failure_with_namespace namespace_released
      (cleanup : cleanup_failure) =
    let update (problem : cleanup_problem) =
      { problem with namespace_released }
    in
    ({
       primary = update cleanup.primary;
       suppressed = List.map update cleanup.suppressed;
     }
      : cleanup_failure)

  let failure_with_namespace namespace_released (failure : failure) =
    let update = function
      | Operation_error _ as issue -> issue
      | Cleanup_error cleanup ->
          Cleanup_error
            (cleanup_failure_with_namespace namespace_released cleanup)
    in
    {
      primary = update failure.primary;
      suppressed = List.map update failure.suppressed;
    }

  let decode_deletion operation ~cleanup_operation = function
    | Ok unlinked ->
        if unlinked then `Complete Unlinked else `Complete Identity_changed
    | Error (Raw_delete_not_committed problem) ->
        `Not_committed (decode_error operation problem)
    | Error
        (Raw_delete_may_have_committed
           (raw_state, namespace_released, raw_failure)) ->
        let progress : deletion_progress =
          {
            local_handle_state = decode_raw_handle_state raw_state;
            namespace_released;
          }
        in
        let failure =
          decode_raw_failure ~cleanup_operation operation raw_failure
          |> failure_with_namespace namespace_released
        in
        `Incomplete (progress, failure)

  let unlink_captured_file_if_identity ~(parent : dir) (file : file) ~expected =
    let operation = Conditional_unlink in
    match check_directory operation parent with
    | Error error -> Deletion_not_committed error
    | Ok () -> (
        match check_file operation file with
        | Error error -> Deletion_not_committed error
        | Ok () when not file.deletion_authority ->
            Deletion_not_committed
              (deletion_contract_error operation
                 "file-delete-authority-not-captured")
        | Ok () when not (Identity.equal file.parent_identity parent.identity)
          ->
            Deletion_not_committed
              (deletion_contract_error operation
                 "captured-file-parent-identity-mismatch")
        | Ok () when not (Identity.equal file.identity expected) ->
            Deletion_complete Identity_changed
        | Ok () -> (
            match
              raw_delete_captured parent.handle file.handle parent.identity
                expected
                (Option.value file.leaf ~default:"")
                0
              |> decode_deletion operation ~cleanup_operation:Close_file
            with
            | `Not_committed error -> Deletion_not_committed error
            | `Complete Identity_changed -> Deletion_complete Identity_changed
            | `Complete result ->
                file.available <- false;
                Deletion_complete result
            | `Incomplete (progress, failure) ->
                file.available <- progress.local_handle_state = Still_open;
                Deletion_incomplete { progress; failure }))

  let unlink_link_no_follow (directory : dir) name ~expected:_ =
    let* () = check_directory Unlink_link directory in
    unsupported ~component:(Native_name.encode name) Unlink_link

  let remove_empty_directory_if_identity (directory : dir) name ~expected:_ =
    let* () = check_directory Remove_empty_directory directory in
    unsupported ~component:(Native_name.encode name) Remove_empty_directory

  let remove_captured_empty_directory_if_identity ~(parent : dir) (target : dir)
      ~expected =
    let operation = Remove_empty_directory in
    match check_directory operation parent with
    | Error error -> Deletion_not_committed error
    | Ok () -> (
        match check_directory operation target with
        | Error error -> Deletion_not_committed error
        | Ok () when not target.deletion_authority ->
            Deletion_not_committed
              (deletion_contract_error operation
                 "directory-delete-authority-not-captured")
        | Ok ()
          when not
                 (match target.parent_identity with
                 | Some identity -> Identity.equal identity parent.identity
                 | None -> false) ->
            Deletion_not_committed
              (deletion_contract_error operation
                 "captured-directory-parent-identity-mismatch")
        | Ok () when not (Identity.equal target.identity expected) ->
            Deletion_complete Identity_changed
        | Ok ()
          when not
                 (match target.handles with
                 | [ handle ] -> handle == target.handle
                 | [] | _ :: _ :: _ -> false) ->
            Deletion_not_committed
              (deletion_contract_error operation
                 "directory-delete-requires-one-retained-handle")
        | Ok () -> (
            match
              raw_delete_captured parent.handle target.handle parent.identity
                expected
                (Option.value target.leaf ~default:"")
                1
              |> decode_deletion operation ~cleanup_operation:Close_directory
            with
            | `Not_committed error -> Deletion_not_committed error
            | `Complete Identity_changed -> Deletion_complete Identity_changed
            | `Complete result ->
                target.available <- false;
                target.handles <- [];
                Deletion_complete result
            | `Incomplete (progress, failure) ->
                target.available <- progress.local_handle_state = Still_open;
                if not target.available then target.handles <- [];
                Deletion_incomplete { progress; failure }))

  let try_lock (directory : dir) name mode =
    let* () = as_failure (check_directory Try_lock directory) in
    let raw_mode = match mode with Shared -> 0 | Exclusive -> 1 in
    match
      raw_try_lock directory.handle name.Native_name.raw raw_mode
        (Permissions.to_int Permissions.owner_read_write)
        directory.identity
    with
    | Error failure ->
        Error
          (decode_raw_failure ~component:(Native_name.encode name)
             ~cleanup_operation:Release_lock Try_lock failure)
    | Ok None -> Ok `Busy
    | Ok (Some (handle, raw_stat)) ->
        let stat = stat_of_raw raw_stat in
        Ok
          (`Acquired
             {
               handle;
               owner = directory.owner;
               directory_identity = directory.identity;
               file_identity = stat.identity;
               available = true;
               held = true;
             })

  let close_probe (value : path_probe) =
    let original_count = List.length value.handles in
    let result, remaining = close_handles Close_probe value.handles in
    value.handles <- remaining;
    value.available <-
      (match result with
      | Cleanup_failed { primary; suppressed } ->
          List.length remaining = original_count
          && List.for_all
               (fun (problem : cleanup_problem) ->
                 problem.local_handle_state = Still_open)
               (primary :: suppressed)
      | Cleanup_complete | Cleanup_local_only -> false);
    result

  let close_directory (value : dir) =
    let original_count = List.length value.handles in
    let result, remaining = close_handles Close_directory value.handles in
    value.handles <- remaining;
    value.available <-
      (match result with
      | Cleanup_failed { primary; suppressed } ->
          List.length remaining = original_count
          && List.for_all
               (fun (problem : cleanup_problem) ->
                 problem.local_handle_state = Still_open)
               (primary :: suppressed)
      | Cleanup_complete | Cleanup_local_only -> false);
    result

  let close_file (value : file) =
    let result = close_raw Close_file value.handle in
    value.available <-
      (match result with
      | Cleanup_failed { primary; suppressed } ->
          List.for_all
            (fun (problem : cleanup_problem) ->
              problem.local_handle_state = Still_open)
            (primary :: suppressed)
      | Cleanup_complete | Cleanup_local_only -> false);
    result

  let close_separation (value : separation_witness) =
    value.materialization_available <- false;
    let candidate, candidate_remaining =
      close_handles Close_separation value.candidate_handles
    in
    value.candidate_handles <- candidate_remaining;
    let forbidden, forbidden_remaining =
      close_handles Close_separation value.forbidden_handles
    in
    value.forbidden_handles <- forbidden_remaining;
    let problems =
      List.concat_map
        (function
          | Cleanup_complete | Cleanup_local_only -> []
          | Cleanup_failed { primary; suppressed } -> primary :: suppressed)
        [ candidate; forbidden ]
    in
    match problems with
    | [] ->
        if candidate = Cleanup_local_only || forbidden = Cleanup_local_only then
          Cleanup_local_only
        else Cleanup_complete
    | primary :: suppressed -> Cleanup_failed { primary; suppressed }

  let release_lock (value : lock) =
    let decode_problem (problem, raw_state, namespace_released) =
      {
        error = decode_error Release_lock problem;
        local_handle_state = decode_raw_handle_state raw_state;
        namespace_released;
      }
    in
    let result =
      match raw_release_lock value.handle value.held with
      | Ok false -> Cleanup_complete
      | Ok true -> Cleanup_local_only
      | Error (primary, suppressed) ->
          Cleanup_failed
            {
              primary = decode_problem primary;
              suppressed = List.map decode_problem suppressed;
            }
    in
    (match result with
    | Cleanup_complete | Cleanup_local_only ->
        value.available <- false;
        value.held <- false
    | Cleanup_failed { primary; suppressed } ->
        let problems = primary :: suppressed in
        value.available <-
          List.exists
            (fun (problem : cleanup_problem) ->
              problem.local_handle_state = Still_open)
            problems;
        if
          List.exists
            (fun (problem : cleanup_problem) -> problem.namespace_released)
            problems
        then value.held <- false);
    result
end
