open Util

type kind = Directory | Regular of string | Symlink of string
type entry = { relative : string; kind : kind; permissions : int }

type t = {
  source_root : string;
  root : string;
  manifest_digest : string;
  owner_marker : string;
  mutable lease : Unix.file_descr option;
}

type 'a bracket_outcome =
  | Acquisition_failed of Error.t
  | Action_returned of 'a * (unit, Error.t) result
  | Action_raised of exn * Printexc.raw_backtrace * (unit, Error.t) result

let source_root snapshot = snapshot.source_root
let root snapshot = snapshot.root
let manifest_digest snapshot = snapshot.manifest_digest

let excluded_names =
  [
    ".git";
    ".hg";
    ".svn";
    "_build";
    "_opam";
    ".ocaml-mutants";
    "node_modules";
    ".direnv";
    ".ocaml-mutants-snapshot-owner-v2";
    ".ocaml-mutants-snapshot-lease-v1";
  ]

let component_equal ~case_sensitive left right =
  if case_sensitive then String.equal left right
  else String.equal (String.lowercase_ascii left) (String.lowercase_ascii right)

let path_components path =
  Ocaml_mutants_core.Mutant.normalize_path path
  |> String.split_on_char '/'
  |> List.filter (fun component -> component <> "" && component <> ".")

let default_skip_for_platform ~case_sensitive relative =
  List.exists
    (fun component ->
      List.exists
        (fun excluded -> component_equal ~case_sensitive component excluded)
        excluded_names)
    (path_components relative)

let default_skip relative =
  default_skip_for_platform ~case_sensitive:(not Sys.win32) relative

let normalize_compare path =
  let normalized = Ocaml_mutants_core.Mutant.normalize_path path in
  if Sys.win32 then String.lowercase_ascii normalized else normalized

let within ~root path =
  let root = normalize_compare root in
  let path = normalize_compare path in
  String.equal root path || string_starts_with ~prefix:(root ^ "/") path

let relative_from_root ~root path =
  let normalized_root = Ocaml_mutants_core.Mutant.normalize_path root in
  let normalized_path = Ocaml_mutants_core.Mutant.normalize_path path in
  if String.equal (normalize_compare root) (normalize_compare path) then ""
  else
    String.sub normalized_path
      (String.length normalized_root + 1)
      (String.length normalized_path - String.length normalized_root - 1)

let rec drop_common left right =
  match (left, right) with
  | left_head :: left_tail, right_head :: right_tail
    when String.equal
           (normalize_compare left_head)
           (normalize_compare right_head) ->
      drop_common left_tail right_tail
  | _ -> (left, right)

let rewritten_link_target ~link_relative ~target_relative =
  let from = path_components (Filename.dirname link_relative) in
  let target = path_components target_relative in
  let from, target = drop_common from target in
  let upwards = List.map (fun _ -> "..") from in
  match upwards @ target with [] -> "." | parts -> String.concat "/" parts

let snapshot_error ~path format =
  Format.kasprintf
    (fun message ->
      Error
        (Error.create ~phase:Error.Snapshot ~cause:Error.Workspace_violation
           ~context:[ ("path", path) ]
           "%s" message))
    format

let build_manifest source_root =
  let rec visit relative accumulated =
    if relative <> "" && default_skip relative then Ok accumulated
    else
      let absolute =
        if relative = "" then source_root
        else Filename.concat source_root relative
      in
      let stats = Unix.lstat absolute in
      match stats.st_kind with
      | Unix.S_DIR ->
          let resolved = Unix.realpath absolute in
          if not (within ~root:source_root resolved) then
            snapshot_error ~path:relative
              "directory junction escapes the workspace (%s)" resolved
          else if
            relative <> ""
            && not
                 (String.equal
                    (normalize_compare absolute)
                    (normalize_compare resolved))
          then
            snapshot_error ~path:relative
              "directory junctions are not supported (%s)" resolved
          else
            let accumulated =
              if relative = "" then accumulated
              else
                { relative; kind = Directory; permissions = stats.st_perm }
                :: accumulated
            in
            Sys.readdir absolute |> Array.to_list |> List.sort String.compare
            |> List.fold_left
                 (fun result name ->
                   match result with
                   | Error _ as error -> error
                   | Ok accumulated ->
                       let child =
                         if relative = "" then name
                         else Filename.concat relative name
                       in
                       visit child accumulated)
                 (Ok accumulated)
      | Unix.S_REG -> (
          match read_file absolute with
          | Error message ->
              Error
                (Error.create ~phase:Error.Snapshot ~cause:Error.Io_failure
                   ~context:[ ("path", relative) ]
                   "cannot read manifest entry: %s" message)
          | Ok bytes ->
              Ok
                ({ relative; kind = Regular bytes; permissions = stats.st_perm }
                :: accumulated))
      | Unix.S_LNK ->
          let target = Unix.readlink absolute in
          let resolved = Unix.realpath absolute in
          if not (within ~root:source_root resolved) then
            snapshot_error ~path:relative
              "symbolic link escapes the workspace (%s -> %s)" target resolved
          else
            let target_relative =
              relative_from_root ~root:source_root resolved
            in
            let target =
              rewritten_link_target ~link_relative:relative ~target_relative
            in
            Ok
              ({ relative; kind = Symlink target; permissions = stats.st_perm }
              :: accumulated)
      | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO | Unix.S_SOCK ->
          snapshot_error ~path:relative "unsupported special file in workspace"
  in
  try Result.map List.rev (visit "" []) with
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (Error.create ~phase:Error.Snapshot ~cause:Error.Io_failure
           ~context:[ ("path", argument); ("operation", function_name) ]
           "%s" (Unix.error_message error))
  | Sys_error message ->
      Error
        (Error.create ~phase:Error.Snapshot ~cause:Error.Io_failure "%s" message)

let digest_manifest entries =
  let buffer = Buffer.create 4096 in
  let field value =
    Buffer.add_string buffer (string_of_int (String.length value));
    Buffer.add_char buffer ':';
    Buffer.add_string buffer value
  in
  List.iter
    (fun entry ->
      field (Ocaml_mutants_core.Mutant.normalize_path entry.relative);
      field (Printf.sprintf "%o" entry.permissions);
      match entry.kind with
      | Directory -> field "directory"
      | Regular bytes ->
          field "regular";
          field (sha256 bytes)
      | Symlink target ->
          field "symlink";
          field target)
    entries;
  sha256 (Buffer.contents buffer)

let copy_entry ~source_root ~destination_root entry =
  let source = Filename.concat source_root entry.relative in
  let destination = Filename.concat destination_root entry.relative in
  match entry.kind with
  | Directory -> (
      match mkdir_p destination with
      | Error _ as error -> error
      | Ok () ->
          (try Unix.chmod destination (entry.permissions lor 0o200)
           with Unix.Unix_error _ -> ());
          Ok ())
  | Regular bytes ->
      let* () = mkdir_p (Filename.dirname destination) in
      let* () = write_file destination bytes in
      (try Unix.chmod destination (entry.permissions lor 0o200)
       with Unix.Unix_error _ -> ());
      let current =
        match read_file source with Ok value -> value | Error _ -> ""
      in
      if not (String.equal current bytes) then
        Error
          (Printf.sprintf "workspace changed while snapshotting %s"
             entry.relative)
      else Ok ()
  | Symlink target -> (
      let* () = mkdir_p (Filename.dirname destination) in
      try
        Unix.symlink target destination;
        Ok ()
      with Unix.Unix_error (error, _, _) -> Error (Unix.error_message error))

let snapshot_prefix = "ocaml-mutants-snapshot-"
let marker_name = ".ocaml-mutants-snapshot-owner-v2"
let lease_name = ".ocaml-mutants-snapshot-lease-v1"
let token_hex_length = 64
let owner_private_directory_permissions = 0o700

module type TEMP_DIRECTORY_FACTORY = sig
  val create_exclusive :
    temp_dir:string ->
    permissions:int ->
    prefix:string ->
    suffix:string ->
    (string, string) result
end

module System_temporary_directory : TEMP_DIRECTORY_FACTORY = struct
  let create_exclusive ~temp_dir ~permissions ~prefix ~suffix =
    try Ok (Filename.temp_dir ~temp_dir ~perms:permissions prefix suffix)
    with Sys_error message -> Error message
end

let allocate_temporary_root (module Factory : TEMP_DIRECTORY_FACTORY) ~temp_dir
    =
  Factory.create_exclusive ~temp_dir
    ~permissions:owner_private_directory_permissions ~prefix:snapshot_prefix
    ~suffix:""

let marker_contents ~token =
  Printf.sprintf "owner=ocaml-mutants\nschema=2\ntoken=%s\n" token

let valid_token token =
  String.length token = token_hex_length
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
       token

let valid_marker contents =
  match split_lines contents with
  | [ "owner=ocaml-mutants"; "schema=2"; token; "" ]
    when string_starts_with ~prefix:"token=" token ->
      let token = String.sub token 6 (String.length token - 6) in
      valid_token token
  | _ -> false

(* POSIX record locks are owned by a process, rather than an individual file
   descriptor. The registry prevents another thread in this process from
   mistaking one of our own leases for an abandoned cross-process lease. *)
let lease_registry_mutex = Mutex.create ()
let live_leases = Hashtbl.create 17

let with_lease_registry action =
  Mutex.lock lease_registry_mutex;
  Fun.protect action ~finally:(fun () -> Mutex.unlock lease_registry_mutex)

let lease_key root = normalize_compare root

let close_lease descriptor =
  (try Unix.lockf descriptor Unix.F_ULOCK 0 with Unix.Unix_error _ -> ());
  try
    Unix.close descriptor;
    Ok ()
  with Unix.Unix_error (error, function_name, argument) ->
    Error
      (Printf.sprintf "%s(%s): %s" function_name argument
         (Unix.error_message error))

let acquire_new_lease path =
  try
    let descriptor =
      Unix.openfile path Unix.[ O_RDWR; O_CREAT; O_EXCL ] 0o600
    in
    try
      Unix.lockf descriptor Unix.F_LOCK 0;
      Ok descriptor
    with Unix.Unix_error (error, function_name, argument) ->
      (try Unix.close descriptor with Unix.Unix_error _ -> ());
      Error
        (Printf.sprintf "%s(%s): %s" function_name argument
           (Unix.error_message error))
  with Unix.Unix_error (error, function_name, argument) ->
    Error
      (Printf.sprintf "%s(%s): %s" function_name argument
         (Unix.error_message error))

let canonical_temp_child ~temporary candidate =
  try
    let temporary = Unix.realpath temporary in
    let lexical_candidate = normalize_compare candidate in
    let stats = Unix.lstat candidate in
    let canonical_candidate = Unix.realpath candidate in
    if
      stats.st_kind = Unix.S_DIR
      && String.equal lexical_candidate (normalize_compare canonical_candidate)
      && String.equal
           (normalize_compare (Filename.dirname canonical_candidate))
           (normalize_compare temporary)
      && string_starts_with ~prefix:snapshot_prefix
           (Filename.basename canonical_candidate)
    then Some canonical_candidate
    else None
  with Unix.Unix_error _ | Sys_error _ -> None

let recover_candidate ~temporary candidate =
  match canonical_temp_child ~temporary candidate with
  | None -> ()
  | Some root -> (
      let marker = Filename.concat root marker_name in
      let lease_path = Filename.concat root lease_name in
      match read_file marker with
      | Error _ -> ()
      | Ok contents when not (valid_marker contents) -> ()
      | Ok _ ->
          with_lease_registry (fun () ->
              if not (Hashtbl.mem live_leases (lease_key root)) then
                try
                  let descriptor = Unix.openfile lease_path Unix.[ O_RDWR ] 0 in
                  let abandoned =
                    try
                      Unix.lockf descriptor Unix.F_TLOCK 0;
                      true
                    with
                    | Unix.Unix_error (Unix.EACCES, _, _)
                    | Unix.Unix_error (Unix.EAGAIN, _, _) ->
                        false
                    | Unix.Unix_error _ -> false
                  in
                  if abandoned then (
                    ignore (close_lease descriptor);
                    ignore (remove_tree root))
                  else
                    (* Closing another descriptor for an actively locked inode
                       can release this process's POSIX locks. The registry
                       check above ensures this descriptor can only belong to
                       another process. *)
                    try Unix.close descriptor with Unix.Unix_error _ -> ()
                with Unix.Unix_error _ -> ()))

let recover_stale_snapshots () =
  let temporary = Filename.get_temp_dir_name () in
  try
    Sys.readdir temporary
    |> Array.iter (fun name ->
        if string_starts_with ~prefix:snapshot_prefix name then
          let root = Filename.concat temporary name in
          recover_candidate ~temporary root)
  with Sys_error _ -> ()

let release_and_remove ~root descriptor =
  with_lease_registry (fun () ->
      let release = close_lease descriptor in
      let removal = remove_tree root in
      Hashtbl.remove live_leases (lease_key root);
      match (release, removal) with
      | Ok (), Ok () -> Ok ()
      | Error message, Ok () | Ok (), Error message -> Error message
      | Error release, Error removal ->
          Error (Printf.sprintf "%s; removal also failed: %s" release removal))

let abandon_snapshot ~root descriptor =
  ignore (release_and_remove ~root descriptor)

let create source_root =
  try
    let source_root = Unix.realpath source_root in
    recover_stale_snapshots ();
    let* manifest = build_manifest source_root in
    let digest = digest_manifest manifest in
    let* temporary =
      match
        allocate_temporary_root
          (module System_temporary_directory)
          ~temp_dir:(Filename.get_temp_dir_name ())
      with
      | Ok temporary -> Ok temporary
      | Error message ->
          Error
            (Error.create ~phase:Error.Snapshot ~cause:Error.Io_failure
               "could not allocate workspace snapshot root: %s" message)
    in
    let token =
      sha256
        (Printf.sprintf "%f:%d:%s" (Unix.gettimeofday ()) (Unix.getpid ())
           temporary)
    in
    let owner_marker = marker_contents ~token in
    match acquire_new_lease (Filename.concat temporary lease_name) with
    | Error message ->
        ignore (remove_tree temporary);
        Error
          (Error.create ~phase:Error.Snapshot ~cause:Error.Io_failure
             "could not acquire workspace snapshot lease: %s" message)
    | Ok lease -> (
        with_lease_registry (fun () ->
            Hashtbl.replace live_leases (lease_key temporary) ());
        match
          atomic_write (Filename.concat temporary marker_name) owner_marker
        with
        | Error message ->
            abandon_snapshot ~root:temporary lease;
            Error
              (Error.create ~phase:Error.Snapshot ~cause:Error.Io_failure
                 "could not mark workspace snapshot ownership: %s" message)
        | Ok () -> (
            let copied =
              List.fold_left
                (fun result entry ->
                  match result with
                  | Error _ as error -> error
                  | Ok () ->
                      copy_entry ~source_root ~destination_root:temporary entry)
                (Ok ()) manifest
            in
            match copied with
            | Ok () ->
                Ok
                  {
                    source_root;
                    root = temporary;
                    manifest_digest = digest;
                    owner_marker;
                    lease = Some lease;
                  }
            | Error message ->
                abandon_snapshot ~root:temporary lease;
                Error
                  (Error.create ~phase:Error.Snapshot ~cause:Error.Io_failure
                     "could not create workspace snapshot: %s" message)))
  with
  | Sys_error message ->
      Error
        (Error.create ~phase:Error.Snapshot ~cause:Error.Io_failure
           "could not snapshot workspace: %s" message)
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (Error.create ~phase:Error.Snapshot ~cause:Error.Io_failure
           ~context:[ ("operation", function_name); ("path", argument) ]
           "%s" (Unix.error_message error))

let destroy snapshot =
  match snapshot.lease with
  | None ->
      Error
        (Error.create ~phase:Error.Cleanup ~cause:Error.Invariant_violation
           "workspace snapshot lease was already released")
  | Some descriptor -> (
      snapshot.lease <- None;
      let verified_root =
        try
          let temporary = Filename.get_temp_dir_name () |> Unix.realpath in
          let root = Unix.realpath snapshot.root in
          let within_temp =
            within ~root:temporary root && not (String.equal temporary root)
          in
          let owned =
            match read_file (Filename.concat root marker_name) with
            | Ok contents -> String.equal contents snapshot.owner_marker
            | Error _ -> false
          in
          if
            within_temp && owned
            && string_starts_with ~prefix:snapshot_prefix
                 (Filename.basename root)
          then Ok root
          else Error root
        with Unix.Unix_error _ | Sys_error _ -> Error snapshot.root
      in
      match verified_root with
      | Error root ->
          with_lease_registry (fun () ->
              ignore (close_lease descriptor);
              Hashtbl.remove live_leases (lease_key snapshot.root));
          Error
            (Error.create ~phase:Error.Cleanup ~cause:Error.Workspace_violation
               "refusing to remove unexpected snapshot path %s" root)
      | Ok root -> (
          match release_and_remove ~root descriptor with
          | Ok () -> Ok ()
          | Error message ->
              Error
                (Error.create ~phase:Error.Cleanup ~cause:Error.Io_failure
                   "could not remove snapshot: %s" message)))

let bracket source_root action =
  match create source_root with
  | Error error -> Acquisition_failed error
  | Ok snapshot -> (
      let action_result =
        try `Returned (action snapshot)
        with exn -> `Raised (exn, Printexc.get_raw_backtrace ())
      in
      let cleanup =
        try destroy snapshot
        with exception_ ->
          Error
            (Error.create ~phase:Error.Cleanup ~cause:Error.Io_failure
               ~context:[ ("exception", Printexc.to_string exception_) ]
               "workspace snapshot cleanup raised")
      in
      match action_result with
      | `Returned value -> Action_returned (value, cleanup)
      | `Raised (exn, backtrace) -> Action_raised (exn, backtrace, cleanup))

let with_snapshot source_root action =
  match bracket source_root action with
  | Acquisition_failed error -> Error error
  | Action_returned (Ok value, Ok ()) -> Ok value
  | Action_returned (Ok _, Error cleanup) -> Error cleanup
  | Action_returned (Error primary, Ok ()) -> Error primary
  | Action_returned (Error primary, Error cleanup) ->
      Error (Error.suppress primary cleanup)
  | Action_raised (exn, backtrace, _) ->
      Printexc.raise_with_backtrace exn backtrace

module For_testing = struct
  module type TEMP_DIRECTORY = TEMP_DIRECTORY_FACTORY

  let allocate_temporary_root = allocate_temporary_root
  let default_skip_for_platform = default_skip_for_platform
end
