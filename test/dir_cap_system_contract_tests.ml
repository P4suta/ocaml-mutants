module Engine = Ocaml_mutants_engine
module C = Engine.Dir_cap
module S = C.System

external inject_native_internal_fault : string -> bool -> bool -> unit
  = "ocaml_mutants_dircap_test_inject"

let root_fault_site = "root"
let child_fault_site = "child"
let probe_fault_site = "probe"
let enumerate_fault_site = "enumerate"
let lock_open_fault_site = "lock-open"
let lock_acquire_fault_site = "lock-acquire"
let lock_release_fault_site = "lock-release"
let publish_fault_site = "publish"
let create_before_commit_fault_site = "create-directory-before-commit"
let create_after_commit_fault_site = "create-directory-after-commit"
let create_file_before_commit_fault_site = "create-file-before-commit"
let create_file_after_commit_fault_site = "create-file-after-commit"
let delete_before_commit_fault_site = "delete-before-commit"
let delete_after_commit_fault_site = "delete-after-commit"
let reset_native_internal_fault () = inject_native_internal_fault "" false false

let fail_error (error : C.error) =
  Alcotest.failf "%s failed (%s)"
    (C.operation_name error.operation)
    error.native_code

let get_ok = function Ok value -> value | Error error -> fail_error error

let get_probe = function
  | Ok value -> value
  | Error (failure : C.failure) -> fail_error (C.issue_error failure.primary)

let get_failure = function
  | Ok value -> value
  | Error (failure : C.failure) -> fail_error (C.issue_error failure.primary)

let acquire_lock = function
  | Ok (`Acquired lock) -> lock
  | Ok `Busy -> Alcotest.fail "lock unexpectedly busy"
  | Error (failure : C.failure) ->
      let error = C.issue_error failure.primary in
      if error.class_ = C.Unsupported then Alcotest.skip ()
      else fail_error error

let expect_lock_busy = function
  | Ok `Busy -> ()
  | Ok (`Acquired lock) ->
      ignore (S.release_lock lock);
      Alcotest.fail "conflicting lock unexpectedly acquired"
  | Error (failure : C.failure) -> fail_error (C.issue_error failure.primary)

let expect_error expected = function
  | Error (error : C.error) ->
      Alcotest.(check bool) "error class" true (error.class_ = expected)
  | Ok _ -> Alcotest.fail "operation unexpectedly succeeded"

let expect_probe_error expected path =
  match S.probe_path path with
  | Error (failure : C.failure) ->
      let error = C.issue_error failure.primary in
      Alcotest.(check bool) "probe error class" true (error.class_ = expected)
  | Ok probe ->
      ignore (S.close_probe probe);
      Alcotest.failf "probe unexpectedly accepted %S" path

let close_quietly close value = ignore (close value)

let expect_cleanup_complete label = function
  | C.Cleanup_complete -> ()
  | C.Cleanup_local_only ->
      Alcotest.failf "%s unexpectedly reported local-only cleanup" label
  | C.Cleanup_failed failure ->
      Alcotest.failf "%s cleanup failed: %s" label
        failure.primary.error.native_code

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
  | Unix.S_REG | Unix.S_LNK | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO
  | Unix.S_SOCK ->
      Unix.unlink path

let with_temp_directory action =
  (* Resolved: the OS temp prefix may itself be a symlink (macOS /var, /tmp),
     which the capability walk refuses to follow. *)
  let root = Filename.temp_dir "ocaml-mutants-dir-cap-" "" |> Unix.realpath in
  Fun.protect
    (fun () -> action root)
    ~finally:(fun () -> if Sys.file_exists root then remove_tree root)

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    (fun () -> output_string channel contents)
    ~finally:(fun () -> close_out_noerr channel)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    (fun () -> really_input_string channel (in_channel_length channel))
    ~finally:(fun () -> close_in_noerr channel)

let rename label source destination =
  try Sys.rename source destination
  with Sys_error message -> Alcotest.failf "%s: %s" label message

let native_name value = get_ok (S.name_of_component value)

let permissions value =
  match C.Permissions.of_int value with
  | Ok permissions -> permissions
  | Error _ -> Alcotest.failf "invalid permission literal: %o" value

let enumeration_budget ~entries ~bytes =
  match
    S.enumeration_budget ~max_entries:entries ~max_native_name_bytes:bytes
  with
  | Ok budget -> budget
  | Error _ -> Alcotest.fail "invalid enumeration budget"

let check_action_then_cleanup ?(action_class = C.Other) ~label ~action_operation
    ~cleanup_operation = function
  | {
      C.primary = C.Operation_error action;
      suppressed =
        [
          C.Cleanup_error { primary = cleanup; suppressed = cleanup_suppressed };
        ];
    } ->
      Alcotest.(check bool)
        (label ^ " action operation")
        true
        (action.operation = action_operation);
      Alcotest.(check bool)
        (label ^ " action classification")
        true
        (action.native_domain = C.Contract && action.class_ = action_class);
      Alcotest.(check int)
        (label ^ " cleanup has no hidden aggregation")
        0
        (List.length cleanup_suppressed);
      Alcotest.(check bool)
        (label ^ " cleanup operation")
        true
        (cleanup.error.operation = cleanup_operation);
      Alcotest.(check bool)
        (label ^ " terminal local state")
        true
        (cleanup.local_handle_state = C.Invalidated_unknown);
      Alcotest.(check bool)
        (label ^ " namespace not released")
        false cleanup.namespace_released
  | failure ->
      Alcotest.failf "%s: unexpected failure shape, primary=%s" label
        (C.operation_name (C.issue_error failure.primary).operation)

let expect_action_then_cleanup ?action_class ~label ~action_operation
    ~cleanup_operation = function
  | Error failure ->
      check_action_then_cleanup ?action_class ~label ~action_operation
        ~cleanup_operation failure
  | Ok _ -> Alcotest.failf "%s: injected operation unexpectedly succeeded" label

let check_cleanup_primary ~label ~cleanup_operation = function
  | {
      C.primary =
        C.Cleanup_error { primary = cleanup; suppressed = cleanup_suppressed };
      suppressed = [];
    } ->
      Alcotest.(check int)
        (label ^ " cleanup has no hidden aggregation")
        0
        (List.length cleanup_suppressed);
      Alcotest.(check bool)
        (label ^ " cleanup operation")
        true
        (cleanup.error.operation = cleanup_operation);
      Alcotest.(check bool)
        (label ^ " terminal local state")
        true
        (cleanup.local_handle_state = C.Invalidated_unknown);
      Alcotest.(check bool)
        (label ^ " namespace not released")
        false cleanup.namespace_released
  | failure ->
      Alcotest.failf "%s: unexpected failure shape, primary=%s" label
        (C.operation_name (C.issue_error failure.primary).operation)

let expect_cleanup_primary ~label ~cleanup_operation = function
  | Error failure -> check_cleanup_primary ~label ~cleanup_operation failure
  | Ok _ -> Alcotest.failf "%s: injected cleanup unexpectedly succeeded" label

let existing_directory path =
  let forbidden_path =
    Filename.temp_dir "ocaml-mutants-dir-cap-forbidden-" "" |> Unix.realpath
  in
  Fun.protect
    (fun () ->
      let forbidden = get_probe (S.probe_path forbidden_path) in
      let candidate = get_probe (S.probe_path path) in
      match S.establish_separation ~forbidden ~candidate with
      | Error failure ->
          close_quietly S.close_probe candidate;
          close_quietly S.close_probe forbidden;
          fail_error (C.issue_error failure.primary)
      | Ok witness -> (
          match S.materialize witness ~permissions:(permissions 0o700) with
          | S.Materialized materialized -> materialized.directory
          | S.Materialization_incomplete { failure; _ } ->
              close_quietly S.close_separation witness;
              fail_error (C.issue_error failure.primary)))
    ~finally:(fun () ->
      if Sys.file_exists forbidden_path then remove_tree forbidden_path)

let separated_candidate ~forbidden_path candidate_path =
  let forbidden = get_probe (S.probe_path forbidden_path) in
  let candidate = get_probe (S.probe_path candidate_path) in
  match S.establish_separation ~forbidden ~candidate with
  | Ok witness -> witness
  | Error failure ->
      close_quietly S.close_probe candidate;
      close_quietly S.close_probe forbidden;
      fail_error (C.issue_error failure.primary)

let test_native_names_and_captured_reads () =
  with_temp_directory (fun root ->
      let cache_path = Filename.concat root "cache with spaces 雪" in
      let moved_path = Filename.concat root "captured-renamed" in
      Unix.mkdir cache_path 0o700;
      let original_name = "original data 雪.txt" in
      let original_path = Filename.concat cache_path original_name in
      write_file original_path "captured-content";
      let additional_names = if Sys.win32 then [] else [ "raw-\255-name" ] in
      List.iter
        (fun name -> write_file (Filename.concat cache_path name) "raw")
        additional_names;
      let directory = existing_directory cache_path in
      Fun.protect
        (fun () ->
          let arbitrary = native_name "report with spaces 雪.json" in
          let diagnostic = S.Native_name.encode arbitrary in
          Alcotest.(check bool)
            "diagnostic is ASCII" true
            (String.for_all (fun byte -> Char.code byte < 0x80) diagnostic);
          let enumerate () =
            match
              S.enumerate_no_follow directory
                ~budget:(enumeration_budget ~entries:32L ~bytes:65536L)
            with
            | S.Enumerated { names; consumption } ->
                `Names
                  ( List.map S.Native_name.encode names
                    |> List.sort String.compare,
                    consumption )
            | S.Enumeration_incomplete { consumption; failure } ->
                `Failure (consumption, C.issue_error failure.primary)
          in
          let expected_names = original_name :: additional_names in
          let expected =
            List.map
              (fun name -> S.Native_name.encode (native_name name))
              expected_names
            |> List.sort String.compare
          in
          let expected_entries = Int64.of_int (List.length expected_names) in
          let expected_bytes =
            List.fold_left
              (fun total name ->
                Int64.add total (Int64.of_int (String.length name)))
              0L expected_names
          in
          let first_enumeration = enumerate () in
          let second_enumeration = enumerate () in
          (match (first_enumeration, second_enumeration) with
          | ( `Names (first, first_consumption),
              `Names (second, second_consumption) ) -> (
              Alcotest.(check (list string))
                "enumeration is complete" expected first;
              Alcotest.(check (list string))
                "enumeration restarts on the same capability" first second;
              Alcotest.(check int64)
                "exact first consumption" expected_entries
                first_consumption.entries;
              Alcotest.(check int64)
                "exact repeated consumption" first_consumption.entries
                second_consumption.entries;
              Alcotest.(check int64)
                "exact canonical native-name bytes" expected_bytes
                first_consumption.native_name_bytes;
              match
                S.enumerate_no_follow directory
                  ~budget:(enumeration_budget ~entries:0L ~bytes:0L)
              with
              | S.Enumeration_incomplete { consumption; failure } ->
                  Alcotest.(check int64)
                    "exhausted budget consumes nothing" 0L consumption.entries;
                  Alcotest.(check bool)
                    "native budget enforced before accumulation" true
                    ((C.issue_error failure.primary).class_ = C.Too_large)
              | S.Enumerated _ ->
                  Alcotest.fail "zero enumeration budget was ignored")
          | `Failure (_, error), _ | _, `Failure (_, error) -> fail_error error);
          rename "rename captured directory" cache_path moved_path;
          let file =
            get_failure
              (S.open_file_no_follow directory (native_name original_name))
          in
          Fun.protect
            (fun () ->
              let moved_original = Filename.concat moved_path original_name in
              rename "swap captured file" moved_original
                (Filename.concat moved_path "old-entry");
              write_file moved_original "replacement";
              Alcotest.(check string)
                "captured file survives entry swap" "captured-content"
                (get_ok (S.read_captured file ~limit:64L)).contents;
              expect_error C.Too_large (S.read_captured file ~limit:3L);
              let replacement =
                get_failure
                  (S.open_file_no_follow directory (native_name original_name))
              in
              Fun.protect
                (fun () ->
                  Alcotest.(check string)
                    "directory handle survives namespace rename" "replacement"
                    (get_ok (S.read_captured replacement ~limit:64L)).contents)
                ~finally:(fun () -> close_quietly S.close_file replacement))
            ~finally:(fun () -> close_quietly S.close_file file))
        ~finally:(fun () -> close_quietly S.close_directory directory))

let test_no_follow_reparse () =
  with_temp_directory (fun root ->
      let cache_path = Filename.concat root "cache" in
      let outside_path = Filename.concat root "outside" in
      let link_path = Filename.concat cache_path "redirect" in
      Unix.mkdir cache_path 0o700;
      Unix.mkdir outside_path 0o700;
      write_file (Filename.concat outside_path "sentinel") "unchanged";
      (try Unix.symlink ~to_dir:true outside_path link_path
       with
       | Unix.Unix_error ((Unix.EPERM | Unix.EACCES | Unix.ENOSYS), _, _) ->
         Alcotest.skip ());
      let directory = existing_directory cache_path in
      Fun.protect
        (fun () ->
          let link = native_name "redirect" in
          let stat =
            match get_failure (S.probe_entry_no_follow directory link) with
            | Some stat -> stat
            | None -> Alcotest.fail "link disappeared"
          in
          Alcotest.(check bool)
            "reparse is classified without traversal" true
            (stat.kind = C.Symbolic_link);
          (match S.open_directory_no_follow directory link with
          | Error failure ->
              Alcotest.(check bool)
                "link open rejected" true
                ((C.issue_error failure.primary).class_ = C.Link_like)
          | Ok child ->
              close_quietly S.close_directory child;
              Alcotest.fail "link directory was opened");
          Alcotest.(check string)
            "target remains untouched" "unchanged"
            (let channel =
               open_in_bin (Filename.concat outside_path "sentinel")
             in
             Fun.protect
               (fun () ->
                 really_input_string channel (in_channel_length channel))
               ~finally:(fun () -> close_in_noerr channel)))
        ~finally:(fun () -> close_quietly S.close_directory directory))

let test_materialization_progress () =
  with_temp_directory (fun root ->
      let forbidden_path = Filename.concat root "forbidden" in
      Unix.mkdir forbidden_path 0o700;
      let safe_path = Filename.concat root "safe" in
      Unix.mkdir safe_path 0o700;
      let requested = Filename.concat safe_path "cache" in
      let forbidden = get_probe (S.probe_path forbidden_path) in
      let candidate = get_probe (S.probe_path requested) in
      let witness =
        match S.establish_separation ~forbidden ~candidate with
        | Ok witness -> witness
        | Error failure ->
            close_quietly S.close_probe candidate;
            close_quietly S.close_probe forbidden;
            fail_error (C.issue_error failure.primary)
      in
      Fun.protect
        (fun () ->
          let outcome =
            S.materialize witness
              ~permissions:C.Permissions.owner_private_directory
          in
          (match outcome with
          | S.Materialization_incomplete { failure; _ } ->
              fail_error (C.issue_error failure.primary)
          | S.Materialized { directory; disposition; created; advisories } ->
              let live_directory = ref (Some directory) in
              Fun.protect
                (fun () ->
                  Alcotest.(check bool)
                    "one missing leaf is newly created" true
                    (disposition = C.Newly_created);
                  (match created with
                  | [ S.Creation_observed name ] ->
                      Alcotest.(check bool)
                        "created evidence names the leaf" true
                        (S.Native_name.equal name (native_name "cache"))
                  | [ S.Creation_may_have_committed _ ] ->
                      Alcotest.fail
                        "successful materialization reported uncertain commit"
                  | _ ->
                      Alcotest.fail
                        "successful one-leaf materialization lost evidence");
                  Alcotest.(check int)
                    "witness teardown is clean" 0 (List.length advisories);
                  Alcotest.(check bool)
                    "created root is visible" true
                    (Sys.file_exists requested);

                  let marker_name = native_name "reservation.marker" in
                  let marker_path =
                    Filename.concat requested "reservation.marker"
                  in
                  let marker =
                    match
                      S.create_file directory marker_name
                        ~permissions:C.Permissions.owner_read_write
                        ~contents:"owner-v2"
                    with
                    | S.Created file -> file
                    | S.Not_created error -> fail_error error
                    | S.Creation_incomplete { residual; failure } ->
                        (match residual with
                        | S.Captured file -> close_quietly S.close_file file
                        | S.Uncaptured _ -> ());
                        fail_error (C.issue_error failure.primary)
                  in
                  let live_marker = ref (Some marker) in
                  Fun.protect
                    (fun () ->
                      match
                        S.unlink_captured_file_if_identity ~parent:directory
                          marker ~expected:(S.file_identity marker)
                      with
                      | C.Deletion_complete C.Unlinked ->
                          live_marker := None;
                          Alcotest.(check bool)
                            "exact captured marker is gone" false
                            (Sys.file_exists marker_path)
                      | C.Deletion_not_committed error -> fail_error error
                      | C.Deletion_complete _ ->
                          Alcotest.fail "exact captured marker was not unlinked"
                      | C.Deletion_incomplete { failure; _ } ->
                          fail_error (C.issue_error failure.primary))
                    ~finally:(fun () ->
                      Option.iter (close_quietly S.close_file) !live_marker);

                  let replacement = Filename.concat safe_path "released" in
                  (* Windows sharing pins the live root's name; POSIX keeps only
                     the inode authority, so the rename is allowed. *)
                  let replacement_blocked =
                    try
                      Sys.rename requested replacement;
                      false
                    with Sys_error _ | Unix.Unix_error _ -> true
                  in
                  Alcotest.(check bool)
                    "materialized-root namespace pin follows the platform"
                    Sys.win32 replacement_blocked;
                  expect_cleanup_complete "materialized root close"
                    (S.close_directory directory);
                  live_directory := None;
                  if Sys.win32 then
                    rename "rename materialized root after release" requested
                      replacement)
                ~finally:(fun () ->
                  Option.iter (close_quietly S.close_directory) !live_directory));
          match
            S.materialize witness
              ~permissions:C.Permissions.owner_private_directory
          with
          | S.Materialized materialized ->
              close_quietly S.close_directory materialized.directory;
              Alcotest.fail "one-shot separation witness was reused"
          | S.Materialization_incomplete { failure; _ } ->
              Alcotest.(check bool)
                "materialization cannot be retried" true
                ((C.issue_error failure.primary).class_ = C.Closed_capability))
        ~finally:(fun () -> close_quietly S.close_separation witness))

let test_materialization_missing_sibling_proof () =
  with_temp_directory (fun root ->
      let workspace_path = Filename.concat root "workspace" in
      let workspace_sentinel = Filename.concat workspace_path "sentinel" in
      let cache_path = Filename.concat root "cache" in
      Unix.mkdir workspace_path 0o700;
      write_file workspace_sentinel "unchanged";
      let forbidden = get_probe (S.probe_path workspace_path) in
      let candidate = get_probe (S.probe_path cache_path) in
      (match S.relationship forbidden candidate with
      | Error error ->
          Alcotest.(check string)
            "generic relationship stays ambiguous"
            "ambiguous-identity-relationship" error.native_code
      | Ok _ ->
          Alcotest.fail
            "missing sibling was promoted to a generic path relationship");
      let witness =
        match S.establish_separation ~forbidden ~candidate with
        | Ok witness -> witness
        | Error failure ->
            close_quietly S.close_probe candidate;
            close_quietly S.close_probe forbidden;
            fail_error (C.issue_error failure.primary)
      in
      Fun.protect
        (fun () ->
          match
            S.materialize witness
              ~permissions:C.Permissions.owner_private_directory
          with
          | S.Materialization_incomplete { failure; _ } ->
              fail_error (C.issue_error failure.primary)
          | S.Materialized { directory; created; advisories; _ } ->
              Fun.protect
                (fun () ->
                  (match created with
                  | [ S.Creation_observed name ] ->
                      Alcotest.(check bool)
                        "sibling evidence names cache" true
                        (S.Native_name.equal name (native_name "cache"))
                  | _ ->
                      Alcotest.fail
                        "exclusive sibling creation lost exact evidence");
                  Alcotest.(check int)
                    "sibling witness cleanup is clean" 0
                    (List.length advisories);
                  Alcotest.(check bool)
                    "missing sibling is created" true
                    (Sys.file_exists cache_path);
                  Alcotest.(check string)
                    "workspace sibling remains unchanged" "unchanged"
                    (read_file workspace_sentinel))
                ~finally:(fun () -> close_quietly S.close_directory directory))
        ~finally:(fun () -> close_quietly S.close_separation witness);

      let raced_path = Filename.concat root "raced-cache" in
      let raced_forbidden = get_probe (S.probe_path workspace_path) in
      let raced_candidate = get_probe (S.probe_path raced_path) in
      let raced_witness =
        match
          S.establish_separation ~forbidden:raced_forbidden
            ~candidate:raced_candidate
        with
        | Ok witness -> witness
        | Error failure ->
            close_quietly S.close_probe raced_candidate;
            close_quietly S.close_probe raced_forbidden;
            fail_error (C.issue_error failure.primary)
      in
      Fun.protect
        (fun () ->
          (try Unix.symlink ~to_dir:true workspace_path raced_path
           with
           | Unix.Unix_error ((Unix.EPERM | Unix.EACCES | Unix.ENOSYS), _, _) ->
             Alcotest.skip ());
          match
            S.materialize raced_witness
              ~permissions:C.Permissions.owner_private_directory
          with
          | S.Materialized materialized ->
              close_quietly S.close_directory materialized.directory;
              Alcotest.fail "exclusive sibling proof joined a raced reparse"
          | S.Materialization_incomplete { created; failure } ->
              Alcotest.(check int)
                "raced sibling commits nothing" 0 (List.length created);
              Alcotest.(check bool)
                "raced sibling collision is exact" true
                ((C.issue_error failure.primary).class_ = C.Already_exists);
              Alcotest.(check string)
                "raced workspace remains unchanged" "unchanged"
                (read_file workspace_sentinel))
        ~finally:(fun () -> close_quietly S.close_separation raced_witness))

let test_materialization_rebinding_and_collision () =
  with_temp_directory (fun root ->
      let workspace_path = Filename.concat root "workspace" in
      let workspace_sentinel = Filename.concat workspace_path "sentinel" in
      let safe_path = Filename.concat root "safe" in
      let moved_safe_path = Filename.concat root "captured-safe" in
      Unix.mkdir workspace_path 0o700;
      Unix.mkdir safe_path 0o700;
      write_file workspace_sentinel "unchanged";
      let requested = Filename.concat safe_path "cache" in
      let witness =
        separated_candidate ~forbidden_path:workspace_path requested
      in
      Fun.protect
        (fun () ->
          rename "move probed materialization parent" safe_path moved_safe_path;
          (try Unix.symlink ~to_dir:true workspace_path safe_path
           with
           | Unix.Unix_error ((Unix.EPERM | Unix.EACCES | Unix.ENOSYS), _, _) ->
             Alcotest.skip ());
          match
            S.materialize witness
              ~permissions:C.Permissions.owner_private_directory
          with
          | S.Materialization_incomplete { failure; _ } ->
              fail_error (C.issue_error failure.primary)
          | S.Materialized { directory; created; _ } ->
              Fun.protect
                (fun () ->
                  (match created with
                  | [ S.Creation_observed name ] ->
                      Alcotest.(check bool)
                        "rebinding evidence names the leaf" true
                        (S.Native_name.equal name (native_name "cache"))
                  | _ ->
                      Alcotest.fail
                        "rebinding materialization lost exact evidence");
                  Alcotest.(check bool)
                    "creation follows captured parent" true
                    (Sys.file_exists (Filename.concat moved_safe_path "cache"));
                  Alcotest.(check bool)
                    "replacement reparse is not traversed" false
                    (Sys.file_exists (Filename.concat workspace_path "cache"));
                  Alcotest.(check string)
                    "forbidden workspace remains unchanged" "unchanged"
                    (read_file workspace_sentinel))
                ~finally:(fun () -> close_quietly S.close_directory directory))
        ~finally:(fun () -> close_quietly S.close_separation witness);

      let collision_parent = Filename.concat root "collision-parent" in
      Unix.mkdir collision_parent 0o700;
      let collision_path = Filename.concat collision_parent "cache" in
      let collision_witness =
        separated_candidate ~forbidden_path:workspace_path collision_path
      in
      Fun.protect
        (fun () ->
          Unix.symlink ~to_dir:true workspace_path collision_path;
          match
            S.materialize collision_witness
              ~permissions:C.Permissions.owner_private_directory
          with
          | S.Materialized materialized ->
              close_quietly S.close_directory materialized.directory;
              Alcotest.fail "materialization joined a colliding reparse"
          | S.Materialization_incomplete { created; failure } ->
              Alcotest.(check int)
                "collision commits nothing" 0 (List.length created);
              Alcotest.(check bool)
                "collision is exact" true
                ((C.issue_error failure.primary).class_ = C.Already_exists);
              Alcotest.(check string)
                "collision target remains unchanged" "unchanged"
                (read_file workspace_sentinel))
        ~finally:(fun () -> close_quietly S.close_separation collision_witness);

      let multi_parent = Filename.concat root "multi-parent" in
      Unix.mkdir multi_parent 0o700;
      let first = Filename.concat multi_parent "first" in
      let multi_requested = Filename.concat first "second" in
      let multi_witness =
        separated_candidate ~forbidden_path:workspace_path multi_requested
      in
      Fun.protect
        (fun () ->
          match
            S.materialize multi_witness
              ~permissions:C.Permissions.owner_private_directory
          with
          | S.Materialized materialized ->
              close_quietly S.close_directory materialized.directory;
              Alcotest.fail "multi-component materialization was claimed"
          | S.Materialization_incomplete { created; failure } ->
              Alcotest.(check int)
                "multi-component path commits nothing" 0 (List.length created);
              Alcotest.(check string)
                "bounded gap is explicit"
                "multi-component-materialization-unsupported"
                (C.issue_error failure.primary).native_code;
              Alcotest.(check bool)
                "no partial prefix was created" false (Sys.file_exists first))
        ~finally:(fun () -> close_quietly S.close_separation multi_witness))

let test_materialization_fault_evidence () =
  with_temp_directory (fun root ->
      let forbidden_path = Filename.concat root "forbidden" in
      let safe_path = Filename.concat root "safe" in
      Unix.mkdir forbidden_path 0o700;
      Unix.mkdir safe_path 0o700;
      let run name =
        let requested = Filename.concat safe_path name in
        let witness = separated_candidate ~forbidden_path requested in
        let outcome =
          Fun.protect
            (fun () ->
              S.materialize witness
                ~permissions:C.Permissions.owner_private_directory)
            ~finally:(fun () -> close_quietly S.close_separation witness)
        in
        (requested, outcome)
      in
      let expect_uncertain expected = function
        | [ S.Creation_may_have_committed name ] ->
            Alcotest.(check bool)
              "uncertain evidence names the attempted leaf" true
              (S.Native_name.equal name (native_name expected))
        | [ S.Creation_observed _ ] ->
            Alcotest.fail "post-commit fault was reported fully observed"
        | _ -> Alcotest.fail "post-commit evidence was lost"
      in
      Fun.protect
        (fun () ->
          inject_native_internal_fault create_before_commit_fault_site true
            false;
          let before_path, before = run "before" in
          (match before with
          | S.Materialized materialized ->
              close_quietly S.close_directory materialized.directory;
              Alcotest.fail "pre-commit fault was hidden"
          | S.Materialization_incomplete { created; failure } ->
              Alcotest.(check int)
                "pre-commit evidence is empty" 0 (List.length created);
              Alcotest.(check string)
                "pre-commit failure is primary" "injected-create-before-commit"
                (C.issue_error failure.primary).native_code);
          Alcotest.(check bool)
            "pre-commit namespace is unchanged" false
            (Sys.file_exists before_path);

          inject_native_internal_fault create_after_commit_fault_site true false;
          let after_path, after = run "after" in
          (match after with
          | S.Materialized materialized ->
              close_quietly S.close_directory materialized.directory;
              Alcotest.fail "post-commit fault was hidden"
          | S.Materialization_incomplete { created; failure } ->
              expect_uncertain "after" created;
              Alcotest.(check string)
                "post-commit failure is primary" "injected-create-after-commit"
                (C.issue_error failure.primary).native_code;
              Alcotest.(check int)
                "successful terminal close has no issue" 0
                (List.length failure.suppressed));
          Alcotest.(check bool)
            "post-commit residual is real" true
            (Sys.file_exists after_path);
          Unix.rmdir after_path;

          inject_native_internal_fault create_after_commit_fault_site true true;
          let cleanup_path, cleanup = run "after-close-fault" in
          (match cleanup with
          | S.Materialized materialized ->
              close_quietly S.close_directory materialized.directory;
              Alcotest.fail "post-commit cleanup fault was hidden"
          | S.Materialization_incomplete { created; failure } ->
              expect_uncertain "after-close-fault" created;
              check_action_then_cleanup
                ~label:"materialize action then terminal child cleanup"
                ~action_operation:C.Materialize
                ~cleanup_operation:C.Close_directory failure);
          Alcotest.(check bool)
            "cleanup-fault residual remains audit-only" true
            (Sys.file_exists cleanup_path);
          Unix.rmdir cleanup_path)
        ~finally:reset_native_internal_fault)

let test_owner_private_directory_creation_is_capability_relative () =
  with_temp_directory (fun root ->
      let workspace_path = Filename.concat root "workspace" in
      let parent_path = Filename.concat root "snapshot-parent" in
      let moved_parent_path = Filename.concat root "captured-parent" in
      Unix.mkdir workspace_path 0o700;
      Unix.mkdir parent_path 0o700;
      let forbidden = get_probe (S.probe_path workspace_path) in
      let candidate = get_probe (S.probe_path parent_path) in
      let witness =
        match S.establish_separation ~forbidden ~candidate with
        | Ok witness -> witness
        | Error failure ->
            close_quietly S.close_probe candidate;
            close_quietly S.close_probe forbidden;
            fail_error (C.issue_error failure.primary)
      in
      let parent =
        match
          S.materialize witness
            ~permissions:C.Permissions.owner_private_directory
        with
        | S.Materialized { directory; disposition = C.Already_present; _ } ->
            directory
        | S.Materialized { directory; disposition = C.Newly_created; _ } ->
            close_quietly S.close_directory directory;
            Alcotest.fail "existing temp parent was reported newly created"
        | S.Materialization_incomplete { failure; _ } ->
            close_quietly S.close_separation witness;
            fail_error (C.issue_error failure.primary)
      in
      Fun.protect
        (fun () ->
          let child_name = native_name "snapshot-child" in
          rename "move captured temp parent" parent_path moved_parent_path;
          (try Unix.symlink ~to_dir:true workspace_path parent_path
           with
           | Unix.Unix_error ((Unix.EPERM | Unix.EACCES | Unix.ENOSYS), _, _) ->
             Alcotest.skip ());
          let weak_name = native_name "weak-child" in
          (match
             S.create_directory parent weak_name
               ~permissions:(permissions 0o750)
           with
          | S.Not_created error ->
              Alcotest.(check bool)
                "non-private policy rejected before commit" true
                (error.class_ = C.Unsupported)
          | S.Created child ->
              close_quietly S.close_directory child;
              Alcotest.fail "non-private Windows policy was accepted"
          | S.Creation_incomplete _ ->
              Alcotest.fail "policy rejection reported a possible commit");
          Alcotest.(check bool)
            "rejected policy creates no child" false
            (Sys.file_exists (Filename.concat moved_parent_path "weak-child"));
          let child =
            match
              S.create_directory parent child_name
                ~permissions:C.Permissions.owner_private_directory
            with
            | S.Created child -> child
            | S.Not_created error -> fail_error error
            | S.Creation_incomplete { failure; _ } ->
                fail_error (C.issue_error failure.primary)
          in
          let live_child = ref (Some child) in
          Fun.protect
            (fun () ->
              let captured_child =
                Filename.concat moved_parent_path "snapshot-child"
              in
              let replacement =
                Filename.concat moved_parent_path "replacement"
              in
              Alcotest.(check bool)
                "create stays below captured parent" true
                (Sys.file_exists captured_child);
              Alcotest.(check bool)
                "ambient reparse target is untouched" false
                (Sys.file_exists
                   (Filename.concat workspace_path "snapshot-child"));
              let replacement_blocked =
                try
                  Sys.rename captured_child replacement;
                  false
                with Sys_error _ | Unix.Unix_error _ -> true
              in
              (* Windows sharing pins the live child's name; POSIX has no
                 sharing violations, so the rename is allowed and the capability
                 keeps only the inode, not the name. *)
              Alcotest.(check bool)
                "live-child namespace replacement follows the platform"
                Sys.win32 replacement_blocked;
              expect_cleanup_complete "created directory close"
                (S.close_directory child);
              live_child := None;
              if Sys.win32 then
                rename "rename child after capability release" captured_child
                  replacement;
              let link_name = "raced-link" in
              let link_path = Filename.concat moved_parent_path link_name in
              Unix.symlink ~to_dir:true workspace_path link_path;
              (match
                 S.create_directory parent (native_name link_name)
                   ~permissions:C.Permissions.owner_private_directory
               with
              | S.Not_created error ->
                  Alcotest.(check bool)
                    "winning reparse entry is never opened" true
                    (error.class_ = C.Already_exists)
              | S.Created raced ->
                  close_quietly S.close_directory raced;
                  Alcotest.fail "create traversed or replaced a raced link"
              | S.Creation_incomplete _ ->
                  Alcotest.fail "name collision reported possible commit");
              Alcotest.(check bool)
                "raced link target remains untouched" false
                (Sys.file_exists (Filename.concat workspace_path link_name)))
            ~finally:(fun () ->
              Option.iter (close_quietly S.close_directory) !live_child))
        ~finally:(fun () -> close_quietly S.close_directory parent))

let test_owner_private_directory_creation_fault_evidence () =
  with_temp_directory (fun root ->
      let parent = existing_directory root in
      let create name =
        S.create_directory parent (native_name name)
          ~permissions:C.Permissions.owner_private_directory
      in
      let check_residual label expected_name = function
        | S.Uncaptured (S.Creation_may_have_committed name) ->
            Alcotest.(check bool)
              label true
              (S.Native_name.equal name (native_name expected_name))
        | S.Uncaptured (S.Creation_observed _) ->
            Alcotest.failf "%s: uncertain commit was reported observed" label
        | S.Captured directory ->
            close_quietly S.close_directory directory;
            Alcotest.failf "%s: terminal native cleanup returned live cap" label
      in
      Fun.protect
        (fun () ->
          inject_native_internal_fault create_before_commit_fault_site true
            false;
          (match create "before" with
          | S.Not_created error ->
              Alcotest.(check string)
                "pre-commit fault evidence" "injected-create-before-commit"
                error.native_code
          | S.Created child ->
              close_quietly S.close_directory child;
              Alcotest.fail "pre-commit fault created a directory"
          | S.Creation_incomplete _ ->
              Alcotest.fail "pre-commit fault became commit-uncertain");
          Alcotest.(check bool)
            "pre-commit namespace unchanged" false
            (Sys.file_exists (Filename.concat root "before"));

          inject_native_internal_fault create_after_commit_fault_site true false;
          (match create "after" with
          | S.Creation_incomplete { residual; failure } ->
              check_residual "post-commit residual" "after" residual;
              Alcotest.(check string)
                "post-commit action evidence" "injected-create-after-commit"
                (C.issue_error failure.primary).native_code;
              Alcotest.(check int)
                "successful terminal close has no cleanup issue" 0
                (List.length failure.suppressed)
          | S.Not_created _ ->
              Alcotest.fail "post-commit fault was reported not-created"
          | S.Created child ->
              close_quietly S.close_directory child;
              Alcotest.fail "post-commit fault was hidden");
          Alcotest.(check bool)
            "post-commit namespace evidence is real" true
            (Sys.file_exists (Filename.concat root "after"));
          Unix.rmdir (Filename.concat root "after");

          inject_native_internal_fault create_after_commit_fault_site true true;
          (match create "after-close-fault" with
          | S.Creation_incomplete { residual; failure } ->
              check_residual "cleanup-fault residual" "after-close-fault"
                residual;
              check_action_then_cleanup ~label:"post-create action plus close"
                ~action_operation:C.Create_directory
                ~cleanup_operation:C.Close_directory failure
          | S.Not_created _ ->
              Alcotest.fail "post-create cleanup fault lost commit evidence"
          | S.Created child ->
              close_quietly S.close_directory child;
              Alcotest.fail "post-create cleanup fault was hidden");
          Alcotest.(check bool)
            "cleanup fault retains namespace uncertainty" true
            (Sys.file_exists (Filename.concat root "after-close-fault"));
          Unix.rmdir (Filename.concat root "after-close-fault"))
        ~finally:(fun () ->
          reset_native_internal_fault ();
          close_quietly S.close_directory parent))

let test_owner_private_file_creation_is_capability_relative () =
  with_temp_directory (fun root ->
      let workspace_path = Filename.concat root "workspace" in
      let workspace_sentinel = Filename.concat workspace_path "sentinel" in
      let parent_path = Filename.concat root "snapshot-parent" in
      let moved_parent_path = Filename.concat root "captured-parent" in
      Unix.mkdir workspace_path 0o700;
      Unix.mkdir parent_path 0o700;
      write_file workspace_sentinel "unchanged";
      let forbidden = get_probe (S.probe_path workspace_path) in
      let candidate = get_probe (S.probe_path parent_path) in
      let witness =
        match S.establish_separation ~forbidden ~candidate with
        | Ok witness -> witness
        | Error failure ->
            close_quietly S.close_probe candidate;
            close_quietly S.close_probe forbidden;
            fail_error (C.issue_error failure.primary)
      in
      let parent =
        match
          S.materialize witness
            ~permissions:C.Permissions.owner_private_directory
        with
        | S.Materialized { directory; disposition = C.Already_present; _ } ->
            directory
        | S.Materialized { directory; disposition = C.Newly_created; _ } ->
            close_quietly S.close_directory directory;
            Alcotest.fail "existing file parent was reported newly created"
        | S.Materialization_incomplete { failure; _ } ->
            close_quietly S.close_separation witness;
            fail_error (C.issue_error failure.primary)
      in
      Fun.protect
        (fun () ->
          rename "move captured file parent" parent_path moved_parent_path;
          (try Unix.symlink ~to_dir:true workspace_path parent_path
           with
           | Unix.Unix_error ((Unix.EPERM | Unix.EACCES | Unix.ENOSYS), _, _) ->
             Alcotest.skip ());
          let create filename contents permissions =
            S.create_file parent (native_name filename) ~permissions ~contents
          in
          (match create "weak-file" "secret" (permissions 0o640) with
          | S.Not_created error ->
              Alcotest.(check bool)
                "non-private file policy rejected before commit" true
                (error.class_ = C.Unsupported)
          | S.Created file ->
              close_quietly S.close_file file;
              Alcotest.fail "non-private file policy was accepted"
          | S.Creation_incomplete _ ->
              Alcotest.fail "file policy rejection reported a possible commit");
          Alcotest.(check bool)
            "rejected policy creates no captured child" false
            (Sys.file_exists (Filename.concat moved_parent_path "weak-file"));
          Alcotest.(check bool)
            "rejected policy never enters workspace" false
            (Sys.file_exists (Filename.concat workspace_path "weak-file"));

          let contents = "head\000snow-雪-tail" in
          let file =
            match
              create "snapshot-file" contents C.Permissions.owner_read_write
            with
            | S.Created file -> file
            | S.Not_created error -> fail_error error
            | S.Creation_incomplete { failure; _ } ->
                fail_error (C.issue_error failure.primary)
          in
          let live_file = ref (Some file) in
          Fun.protect
            (fun () ->
              let captured_file =
                Filename.concat moved_parent_path "snapshot-file"
              in
              Alcotest.(check bool)
                "file create stays below captured parent" true
                (Sys.file_exists captured_file);
              Alcotest.(check bool)
                "ambient parent reparse target is untouched" false
                (Sys.file_exists
                   (Filename.concat workspace_path "snapshot-file"));
              Alcotest.(check string)
                "returned capability reads exact bytes" contents
                (get_ok
                   (S.read_captured file
                      ~limit:(Int64.of_int (String.length contents))))
                  .contents;
              let observed =
                match
                  get_failure
                    (S.probe_entry_no_follow parent
                       (native_name "snapshot-file"))
                with
                | Some stat -> stat
                | None -> Alcotest.fail "created file disappeared"
              in
              Alcotest.(check bool)
                "live file identity matches root-relative entry" true
                (S.Identity.equal (S.file_identity file) observed.identity);
              if Sys.win32 then (
                Alcotest.(check bool)
                  "live file denies concurrent writers" true
                  (try
                     write_file captured_file "attacker";
                     false
                   with Sys_error _ | Unix.Unix_error _ -> true);
                Alcotest.(check bool)
                  "live file denies namespace replacement" true
                  (try
                     Sys.rename captured_file
                       (Filename.concat moved_parent_path "renamed-file");
                     false
                   with Sys_error _ | Unix.Unix_error _ -> true));
              expect_cleanup_complete "created file close" (S.close_file file);
              live_file := None;
              Alcotest.(check string)
                "ambient file is exact after release" contents
                (read_file captured_file);
              if not Sys.win32 then
                Alcotest.(check int)
                  "POSIX file is exactly owner read-write" 0o600
                  (Unix.stat captured_file).st_perm;

              let collision = Filename.concat moved_parent_path "collision" in
              write_file collision "foreign";
              (match
                 create "collision" "replacement" C.Permissions.owner_read_write
               with
              | S.Not_created error ->
                  Alcotest.(check bool)
                    "existing file wins without replacement" true
                    (error.class_ = C.Already_exists)
              | S.Created raced ->
                  close_quietly S.close_file raced;
                  Alcotest.fail "existing file was replaced"
              | S.Creation_incomplete _ ->
                  Alcotest.fail "collision reported a possible commit");
              Alcotest.(check string)
                "collision preserves foreign bytes" "foreign"
                (read_file collision);

              let link_path = Filename.concat moved_parent_path "raced-link" in
              Unix.symlink ~to_dir:false workspace_sentinel link_path;
              (match
                 create "raced-link" "replacement"
                   C.Permissions.owner_read_write
               with
              | S.Not_created error ->
                  Alcotest.(check bool)
                    "winning link entry is never opened" true
                    (error.class_ = C.Already_exists)
              | S.Created raced ->
                  close_quietly S.close_file raced;
                  Alcotest.fail "file create traversed or replaced a link"
              | S.Creation_incomplete _ ->
                  Alcotest.fail "link collision reported a possible commit");
              Alcotest.(check string)
                "link target bytes remain untouched" "unchanged"
                (read_file workspace_sentinel))
            ~finally:(fun () ->
              Option.iter (close_quietly S.close_file) !live_file))
        ~finally:(fun () -> close_quietly S.close_directory parent))

let test_owner_private_file_creation_fault_evidence () =
  with_temp_directory (fun root ->
      let parent = existing_directory root in
      let create filename contents =
        S.create_file parent (native_name filename)
          ~permissions:C.Permissions.owner_read_write ~contents
      in
      let check_residual label expected_name = function
        | S.Uncaptured (S.Creation_may_have_committed name) ->
            Alcotest.(check bool)
              label true
              (S.Native_name.equal name (native_name expected_name))
        | S.Uncaptured (S.Creation_observed _) ->
            Alcotest.failf "%s: uncertain commit was reported observed" label
        | S.Captured file ->
            close_quietly S.close_file file;
            Alcotest.failf "%s: terminal native cleanup returned live cap" label
      in
      Fun.protect
        (fun () ->
          inject_native_internal_fault create_file_before_commit_fault_site true
            false;
          (match create "before" "must-not-exist" with
          | S.Not_created error ->
              Alcotest.(check string)
                "file pre-commit fault evidence"
                "injected-create-file-before-commit" error.native_code
          | S.Created file ->
              close_quietly S.close_file file;
              Alcotest.fail "pre-commit fault created a file"
          | S.Creation_incomplete _ ->
              Alcotest.fail "pre-commit file fault became commit-uncertain");
          Alcotest.(check bool)
            "file pre-commit namespace unchanged" false
            (Sys.file_exists (Filename.concat root "before"));

          write_file (Filename.concat root "collision") "foreign";
          inject_native_internal_fault create_file_after_commit_fault_site true
            false;
          (match create "collision" "replacement" with
          | S.Not_created error ->
              Alcotest.(check bool)
                "collision occurs before post-commit fault site" true
                (error.class_ = C.Already_exists)
          | S.Created file ->
              close_quietly S.close_file file;
              Alcotest.fail "post-commit fault setup replaced a collision"
          | S.Creation_incomplete _ ->
              Alcotest.fail "collision consumed a post-commit fault");
          Alcotest.(check string)
            "collision remains foreign" "foreign"
            (read_file (Filename.concat root "collision"));
          (match create "after" "must-not-be-written" with
          | S.Creation_incomplete { residual; failure } ->
              check_residual "file post-commit residual" "after" residual;
              Alcotest.(check string)
                "file post-commit action evidence"
                "injected-create-file-after-commit"
                (C.issue_error failure.primary).native_code;
              Alcotest.(check int)
                "successful terminal file close has no cleanup issue" 0
                (List.length failure.suppressed)
          | S.Not_created _ ->
              Alcotest.fail "post-commit file fault was reported not-created"
          | S.Created file ->
              close_quietly S.close_file file;
              Alcotest.fail "post-commit file fault was hidden");
          Alcotest.(check string)
            "post-commit fault is before content write" ""
            (read_file (Filename.concat root "after"));
          Unix.unlink (Filename.concat root "after");

          inject_native_internal_fault create_file_after_commit_fault_site true
            true;
          (match create "after-close-fault" "must-not-be-written" with
          | S.Creation_incomplete { residual; failure } ->
              check_residual "file cleanup-fault residual" "after-close-fault"
                residual;
              check_action_then_cleanup
                ~label:"post-create-file action plus close"
                ~action_operation:C.Create_file ~cleanup_operation:C.Close_file
                failure
          | S.Not_created _ ->
              Alcotest.fail
                "post-create-file cleanup fault lost commit evidence"
          | S.Created file ->
              close_quietly S.close_file file;
              Alcotest.fail "post-create-file cleanup fault was hidden");
          Alcotest.(check string)
            "cleanup fault is also before content write" ""
            (read_file (Filename.concat root "after-close-fault"));
          Unix.unlink (Filename.concat root "after-close-fault"))
        ~finally:(fun () ->
          reset_native_internal_fault ();
          close_quietly S.close_directory parent))

let test_captured_handle_deletion_and_fault_evidence () =
  with_temp_directory (fun root ->
      let other_path = Filename.concat root "other-parent" in
      Unix.mkdir other_path 0o700;
      let parent = existing_directory root in
      let other = existing_directory other_path in
      let file_path name = Filename.concat root name in
      let with_delete_file name action =
        let file =
          get_failure
            (S.open_file_for_delete_no_follow parent (native_name name))
        in
        Fun.protect
          (fun () -> action file)
          ~finally:(fun () -> close_quietly S.close_file file)
      in
      let with_delete_directory name action =
        let directory =
          get_failure
            (S.open_directory_for_delete_no_follow parent (native_name name))
        in
        Fun.protect
          (fun () -> action directory)
          ~finally:(fun () -> close_quietly S.close_directory directory)
      in
      let expect_unlinked label = function
        | C.Deletion_complete C.Unlinked -> ()
        | C.Deletion_not_committed error ->
            Alcotest.failf "%s was not committed: %s" label error.native_code
        | C.Deletion_complete C.Absent ->
            Alcotest.failf "%s unexpectedly reported absent" label
        | C.Deletion_complete C.Identity_changed ->
            Alcotest.failf "%s unexpectedly reported identity change" label
        | C.Deletion_incomplete { failure; _ } ->
            Alcotest.failf "%s was incomplete: %s" label
              (C.issue_error failure.primary).native_code
      in
      Fun.protect
        (fun () ->
          write_file (file_path "ordinary") "ordinary";
          let ordinary =
            get_failure (S.open_file_no_follow parent (native_name "ordinary"))
          in
          Fun.protect
            (fun () ->
              match
                S.unlink_captured_file_if_identity ~parent ordinary
                  ~expected:(S.file_identity ordinary)
              with
              | C.Deletion_not_committed error ->
                  Alcotest.(check string)
                    "ordinary handle has no deletion authority"
                    "file-delete-authority-not-captured" error.native_code;
                  Alcotest.(check string)
                    "ordinary handle remains live" "ordinary"
                    (get_ok (S.read_captured ordinary ~limit:64L)).contents
              | C.Deletion_complete _ | C.Deletion_incomplete _ ->
                  Alcotest.fail "ordinary file handle authorized deletion")
            ~finally:(fun () -> close_quietly S.close_file ordinary);

          let link_path = file_path "delete-link" in
          (try
             Unix.symlink ~to_dir:false (file_path "ordinary") link_path;
             match
               S.open_file_for_delete_no_follow parent
                 (native_name "delete-link")
             with
             | Error failure ->
                 Alcotest.(check bool)
                   "delete capture rejects reparse" true
                   ((C.issue_error failure.primary).class_ = C.Link_like)
             | Ok file ->
                 close_quietly S.close_file file;
                 Alcotest.fail "delete capture followed a reparse point"
           with
           | Unix.Unix_error ((Unix.EPERM | Unix.EACCES | Unix.ENOSYS), _, _) ->
             ());

          write_file (file_path "target") "owned";
          with_delete_file "target" (fun file ->
              let expected = S.file_identity file in
              (match
                 S.unlink_captured_file_if_identity ~parent:other file ~expected
               with
              | C.Deletion_not_committed error ->
                  Alcotest.(check string)
                    "wrong parent is pre-commit"
                    "captured-file-parent-identity-mismatch" error.native_code
              | C.Deletion_complete _ | C.Deletion_incomplete _ ->
                  Alcotest.fail "wrong parent authorized deletion");
              (match
                 S.unlink_captured_file_if_identity ~parent file
                   ~expected:(S.dir_identity other)
               with
              | C.Deletion_complete C.Identity_changed -> ()
              | C.Deletion_not_committed _ | C.Deletion_complete _
              | C.Deletion_incomplete _ ->
                  Alcotest.fail "wrong target identity was not refused");
              (* Windows delete-sharing pins the captured name; POSIX has no
                 sharing violation, so the rename succeeds and is undone so the
                 verified-name commit below still binds. *)
              let replacement_blocked =
                try
                  Sys.rename (file_path "target") (file_path "moved-target");
                  false
                with Sys_error _ | Unix.Unix_error _ -> true
              in
              Alcotest.(check bool)
                "delete-handle namespace pin follows the platform" Sys.win32
                replacement_blocked;
              if not replacement_blocked then
                rename "restore the POSIX delete target"
                  (file_path "moved-target") (file_path "target");
              Alcotest.(check string)
                "refusals leave target live" "owned"
                (get_ok (S.read_captured file ~limit:64L)).contents;
              expect_unlinked "captured file deletion"
                (S.unlink_captured_file_if_identity ~parent file ~expected);
              Alcotest.(check bool)
                "captured file namespace released" false
                (Sys.file_exists (file_path "target"));
              expect_error C.Closed_capability (S.read_captured file ~limit:64L));

          write_file (file_path "before") "retryable";
          with_delete_file "before" (fun file ->
              inject_native_internal_fault delete_before_commit_fault_site true
                false;
              (match
                 S.unlink_captured_file_if_identity ~parent file
                   ~expected:(S.file_identity file)
               with
              | C.Deletion_not_committed error ->
                  Alcotest.(check string)
                    "delete pre-commit fault is exact"
                    "injected-delete-before-commit" error.native_code
              | C.Deletion_complete _ | C.Deletion_incomplete _ ->
                  Alcotest.fail
                    "delete pre-commit fault crossed commit boundary");
              Alcotest.(check string)
                "pre-commit delete target remains live" "retryable"
                (get_ok (S.read_captured file ~limit:64L)).contents;
              expect_unlinked "retried captured file deletion"
                (S.unlink_captured_file_if_identity ~parent file
                   ~expected:(S.file_identity file)));

          write_file (file_path "after") "committed";
          with_delete_file "after" (fun file ->
              inject_native_internal_fault delete_after_commit_fault_site true
                false;
              match
                S.unlink_captured_file_if_identity ~parent file
                  ~expected:(S.file_identity file)
              with
              | C.Deletion_incomplete
                  {
                    progress =
                      {
                        local_handle_state = C.Closed;
                        namespace_released = true;
                      };
                    failure =
                      { primary = C.Operation_error action; suppressed = [] };
                  } ->
                  Alcotest.(check string)
                    "delete post-commit action is exact"
                    "injected-delete-after-commit" action.native_code;
                  Alcotest.(check bool)
                    "post-commit file namespace released" false
                    (Sys.file_exists (file_path "after"));
                  expect_error C.Closed_capability
                    (S.read_captured file ~limit:64L)
              | C.Deletion_not_committed _ | C.Deletion_complete _
              | C.Deletion_incomplete _ ->
                  Alcotest.fail "delete post-commit evidence was not exact");

          write_file (file_path "double-fault") "committed";
          with_delete_file "double-fault" (fun file ->
              inject_native_internal_fault delete_after_commit_fault_site true
                true;
              match
                S.unlink_captured_file_if_identity ~parent file
                  ~expected:(S.file_identity file)
              with
              | C.Deletion_incomplete
                  {
                    progress =
                      {
                        local_handle_state = C.Invalidated_unknown;
                        namespace_released;
                      };
                    failure =
                      {
                        primary = C.Operation_error action;
                        suppressed =
                          [
                            C.Cleanup_error
                              { primary = cleanup; suppressed = [] };
                          ];
                      };
                  } ->
                  (* Windows proves release only at close, so a close fault
                     leaves it unknown; the POSIX unlinkat committed and
                     verified the release before the faulting close. *)
                  Alcotest.(check bool)
                    "double-fault release evidence follows the platform"
                    (not Sys.win32) namespace_released;
                  Alcotest.(check bool)
                    "delete action then cleanup stay ordered" true
                    (action.native_code = "injected-delete-after-commit"
                    && cleanup.error.native_code
                       = "injected-internal-close-failure"
                    && cleanup.local_handle_state = C.Invalidated_unknown
                    && Bool.equal cleanup.namespace_released (not Sys.win32))
              | C.Deletion_not_committed _ | C.Deletion_complete _
              | C.Deletion_incomplete _ ->
                  Alcotest.fail
                    "delete action and cleanup evidence was flattened");

          write_file (file_path "close-fault") "committed";
          with_delete_file "close-fault" (fun file ->
              inject_native_internal_fault delete_after_commit_fault_site false
                true;
              match
                S.unlink_captured_file_if_identity ~parent file
                  ~expected:(S.file_identity file)
              with
              | C.Deletion_incomplete
                  {
                    progress =
                      {
                        local_handle_state = C.Invalidated_unknown;
                        namespace_released;
                      };
                    failure =
                      {
                        primary =
                          C.Cleanup_error { primary = cleanup; suppressed = [] };
                        suppressed = [];
                      };
                  } ->
                  Alcotest.(check bool)
                    "close-fault release evidence follows the platform"
                    (not Sys.win32) namespace_released;
                  Alcotest.(check bool)
                    "delete cleanup-primary is exact" true
                    (cleanup.error.native_code
                     = "injected-internal-close-failure"
                    && cleanup.local_handle_state = C.Invalidated_unknown
                    && Bool.equal cleanup.namespace_released (not Sys.win32))
              | C.Deletion_not_committed _ | C.Deletion_complete _
              | C.Deletion_incomplete _ ->
                  Alcotest.fail "delete cleanup-primary evidence was lost");

          Unix.mkdir (file_path "nonempty") 0o700;
          write_file (Filename.concat (file_path "nonempty") "child") "x";
          with_delete_directory "nonempty" (fun directory ->
              match
                S.remove_captured_empty_directory_if_identity ~parent directory
                  ~expected:(S.dir_identity directory)
              with
              | C.Deletion_not_committed error ->
                  Alcotest.(check bool)
                    "nonempty removal is pre-commit Busy" true
                    (error.class_ = C.Busy);
                  let names, _ =
                    match
                      S.enumerate_no_follow directory
                        ~budget:(enumeration_budget ~entries:4L ~bytes:128L)
                    with
                    | S.Enumerated { names; consumption } -> (names, consumption)
                    | S.Enumeration_incomplete { failure; _ } ->
                        fail_error (C.issue_error failure.primary)
                  in
                  Alcotest.(check (list string))
                    "nonempty target remains live"
                    [ S.Native_name.encode (native_name "child") ]
                    (List.map S.Native_name.encode names)
              | C.Deletion_complete _ | C.Deletion_incomplete _ ->
                  Alcotest.fail "nonempty directory deletion committed");

          Unix.mkdir (file_path "empty-delete") 0o700;
          with_delete_directory "empty-delete" (fun directory ->
              expect_unlinked "captured empty-directory deletion"
                (S.remove_captured_empty_directory_if_identity ~parent directory
                   ~expected:(S.dir_identity directory));
              Alcotest.(check bool)
                "captured directory namespace released" false
                (Sys.file_exists (file_path "empty-delete"))))
        ~finally:(fun () ->
          reset_native_internal_fault ();
          close_quietly S.close_directory other;
          close_quietly S.close_directory parent))

let test_identity_relationship () =
  with_temp_directory (fun root ->
      let parent_path = Filename.concat root "parent" in
      let child_path = Filename.concat parent_path "child" in
      Unix.mkdir parent_path 0o700;
      Unix.mkdir child_path 0o700;
      let left = get_probe (S.probe_path parent_path) in
      let same = get_probe (S.probe_path parent_path) in
      let child = get_probe (S.probe_path child_path) in
      Fun.protect
        (fun () ->
          Alcotest.(check bool)
            "same deepest identity" true
            (get_ok (S.relationship left same) = C.Same);
          Alcotest.(check bool)
            "containment follows identity chain" true
            (get_ok (S.relationship left child) = C.Left_contains_right))
        ~finally:(fun () ->
          close_quietly S.close_probe child;
          close_quietly S.close_probe same;
          close_quietly S.close_probe left))

let test_invalid_names_and_unsupported_mutation () =
  expect_error C.Invalid_name (S.name_of_component "embedded\000nul");
  expect_error C.Invalid_name (S.name_of_component "..");
  expect_probe_error C.Invalid_name ".";
  with_temp_directory (fun root ->
      expect_probe_error C.Invalid_name (root ^ "\000truncated"));
  if Sys.win32 then (
    List.iter
      (expect_probe_error C.Invalid_name)
      [ "\\\\server"; "\\\\server\\"; "\\\\server\\\\share" ];
    let unicode_string_max_bytes = (1 lsl 16) - 1 in
    let windows_wchar_bytes = 2 in
    expect_error C.Invalid_name
      (S.name_of_component
         (String.make
            ((unicode_string_max_bytes / windows_wchar_bytes) + 1)
            'x')));
  with_temp_directory (fun root ->
      let directory = existing_directory root in
      Fun.protect
        (fun () ->
          match
            S.create_file directory (native_name "new")
              ~permissions:(permissions 0o640) ~contents:"data"
          with
          | S.Not_created error ->
              Alcotest.(check bool)
                "non-private create is fail-closed" true
                (error.class_ = C.Unsupported);
              Alcotest.(check bool)
                "policy rejection creates no entry" false
                (Sys.file_exists (Filename.concat root "new"))
          | S.Created file ->
              close_quietly S.close_file file;
              Alcotest.fail "non-private create policy changed namespace"
          | S.Creation_incomplete _ ->
              Alcotest.fail "policy rejection reported a possible commit")
        ~finally:(fun () -> close_quietly S.close_directory directory))

let test_fork_owner () =
  if Sys.win32 then Alcotest.skip ();
  with_temp_directory (fun root ->
      let file_path = Filename.concat root "owned" in
      write_file file_path "parent";
      let directory = existing_directory root in
      let file =
        get_failure (S.open_file_no_follow directory (native_name "owned"))
      in
      Fun.protect
        (fun () ->
          match Unix.fork () with
          | 0 ->
              let wrong_process =
                match S.read_captured file ~limit:64L with
                | Error error -> error.class_ = C.Wrong_process
                | Ok _ -> false
              in
              let local = S.close_file file = C.Cleanup_local_only in
              Unix._exit (if wrong_process && local then 0 else 1)
          | child ->
              let _, status = Unix.waitpid [] child in
              Alcotest.(check bool)
                "child sees owner boundary" true (status = Unix.WEXITED 0);
              Alcotest.(check string)
                "parent capability remains live" "parent"
                (get_ok (S.read_captured file ~limit:64L)).contents)
        ~finally:(fun () ->
          close_quietly S.close_file file;
          close_quietly S.close_directory directory))

let test_duplicate_directory_is_independent () =
  with_temp_directory (fun root ->
      write_file (Filename.concat root "payload") "independent";
      let original = existing_directory root in
      let duplicate = get_failure (S.duplicate_directory original) in
      Fun.protect
        (fun () ->
          close_quietly S.close_directory original;
          (match S.open_file_no_follow original (native_name "payload") with
          | Error failure ->
              Alcotest.(check bool)
                "closed original is cleanup-only" true
                ((C.issue_error failure.primary).class_ = C.Closed_capability)
          | Ok file ->
              close_quietly S.close_file file;
              Alcotest.fail "closed original remained active");
          let file =
            get_failure
              (S.open_file_no_follow duplicate (native_name "payload"))
          in
          Fun.protect
            (fun () ->
              Alcotest.(check string)
                "duplicate owns an independent handle" "independent"
                (get_ok (S.read_captured file ~limit:64L)).contents)
            ~finally:(fun () -> close_quietly S.close_file file))
        ~finally:(fun () -> close_quietly S.close_directory duplicate))

let test_internal_close_evidence_is_lossless () =
  with_temp_directory (fun root ->
      let child_path = Filename.concat root "child" in
      let payload_path = Filename.concat root "payload" in
      Unix.mkdir child_path 0o700;
      write_file payload_path "evidence";
      Fun.protect
        (fun () ->
          if Sys.win32 then (
            inject_native_internal_fault root_fault_site true true;
            expect_action_then_cleanup ~label:"root action plus close"
              ~action_operation:C.Probe_path ~cleanup_operation:C.Close_probe
              (S.probe_path root));

          inject_native_internal_fault child_fault_site true true;
          expect_action_then_cleanup ~label:"probe child action plus close"
            ~action_operation:C.Open_directory ~cleanup_operation:C.Close_probe
            (S.probe_path child_path);

          let directory = existing_directory root in
          Fun.protect
            (fun () ->
              inject_native_internal_fault child_fault_site true true;
              expect_action_then_cleanup
                ~label:"open directory action plus close"
                ~action_operation:C.Open_directory
                ~cleanup_operation:C.Close_directory
                (S.open_directory_no_follow directory (native_name "child"));

              inject_native_internal_fault child_fault_site true true;
              expect_action_then_cleanup ~label:"open file action plus close"
                ~action_operation:C.Open_file ~cleanup_operation:C.Close_file
                (S.open_file_no_follow directory (native_name "payload"));

              if Sys.win32 then (
                inject_native_internal_fault probe_fault_site true true;
                expect_action_then_cleanup
                  ~label:"probe entry action plus close"
                  ~action_operation:C.Probe_entry
                  ~cleanup_operation:C.Probe_entry
                  (S.probe_entry_no_follow directory (native_name "payload"));

                inject_native_internal_fault probe_fault_site false true;
                expect_cleanup_primary ~label:"probe entry close after success"
                  ~cleanup_operation:C.Probe_entry
                  (S.probe_entry_no_follow directory (native_name "payload")));

              match
                S.probe_entry_no_follow directory (native_name "payload")
              with
              | Ok (Some stat) ->
                  Alcotest.(check bool)
                    "fault injection is one-shot" true (stat.kind = C.Regular)
              | Ok None -> Alcotest.fail "payload disappeared after fault tests"
              | Error failure -> fail_error (C.issue_error failure.primary))
            ~finally:(fun () -> close_quietly S.close_directory directory))
        ~finally:reset_native_internal_fault)

let test_enumeration_resource_evidence () =
  with_temp_directory (fun root ->
      let payload = "enumerated payload" in
      write_file (Filename.concat root payload) "bounded";
      let directory = existing_directory root in
      let generous = enumeration_budget ~entries:16L ~bytes:65536L in
      let enumerate budget = S.enumerate_no_follow directory ~budget in
      let expect_payload label = function
        | S.Enumerated { names; consumption } ->
            Alcotest.(check bool)
              (label ^ " contains payload")
              true
              (List.exists
                 (fun candidate ->
                   S.Native_name.equal candidate (native_name payload))
                 names);
            Alcotest.(check int64)
              (label ^ " exact entries") 1L consumption.entries;
            Alcotest.(check int64)
              (label ^ " exact bytes")
              (Int64.of_int (String.length payload))
              consumption.native_name_bytes
        | S.Enumeration_incomplete { failure; _ } ->
            fail_error (C.issue_error failure.primary)
      in
      Fun.protect
        (fun () ->
          inject_native_internal_fault enumerate_fault_site true false;
          (match enumerate generous with
          | S.Enumeration_incomplete
              {
                consumption;
                failure =
                  { primary = C.Operation_error action; suppressed = [] };
              } ->
              Alcotest.(check int64)
                "injected enumeration exact consumption"
                (if Sys.win32 then 1L else 0L)
                consumption.entries;
              Alcotest.(check int64)
                "injected enumeration exact native bytes"
                (if Sys.win32 then Int64.of_int (String.length payload) else 0L)
                consumption.native_name_bytes;
              Alcotest.(check bool)
                "injected enumeration action is explicit" true
                (action.operation = C.Enumerate
                && action.native_domain = C.Contract
                && action.class_ = C.Other)
          | S.Enumeration_incomplete { failure; _ } ->
              Alcotest.failf "unexpected enumeration failure shape: %s"
                (C.operation_name (C.issue_error failure.primary).operation)
          | S.Enumerated _ ->
              Alcotest.fail "injected enumeration unexpectedly succeeded");
          expect_payload "post-injection enumeration" (enumerate generous);

          if not Sys.win32 then (
            inject_native_internal_fault enumerate_fault_site true true;
            (match enumerate generous with
            | S.Enumeration_incomplete { consumption; failure } ->
                Alcotest.(check int64)
                  "fd owner failure consumes nothing" 0L consumption.entries;
                check_action_then_cleanup
                  ~label:"enumeration action plus fd close"
                  ~action_operation:C.Enumerate ~cleanup_operation:C.Enumerate
                  failure
            | S.Enumerated _ ->
                Alcotest.fail "enumeration fd double failure was lost");

            inject_native_internal_fault enumerate_fault_site false true;
            (match enumerate (enumeration_budget ~entries:0L ~bytes:0L) with
            | S.Enumeration_incomplete { consumption; failure } ->
                Alcotest.(check int64)
                  "budget failure consumes nothing" 0L consumption.entries;
                check_action_then_cleanup ~action_class:C.Too_large
                  ~label:"enumeration budget plus closedir"
                  ~action_operation:C.Enumerate ~cleanup_operation:C.Enumerate
                  failure
            | S.Enumerated _ ->
                Alcotest.fail "budget plus closedir failure was lost");

            inject_native_internal_fault enumerate_fault_site false true;
            (match enumerate generous with
            | S.Enumeration_incomplete { consumption; failure } ->
                Alcotest.(check int64)
                  "cleanup-primary retains full consumption" 1L
                  consumption.entries;
                check_cleanup_primary
                  ~label:"successful read plus closedir failure"
                  ~cleanup_operation:C.Enumerate failure
            | S.Enumerated _ ->
                Alcotest.fail "closedir cleanup failure was lost");
            expect_payload "post-closedir-fault enumeration"
              (enumerate generous)))
        ~finally:(fun () ->
          reset_native_internal_fault ();
          close_quietly S.close_directory directory))

let publication_handle directory name =
  match S.open_file_for_publish_no_follow directory (native_name name) with
  | Ok file -> file
  | Error failure ->
      let error = C.issue_error failure.primary in
      if error.class_ = C.Unsupported then Alcotest.skip ()
      else fail_error error

let expect_not_published expected = function
  | C.Not_published error ->
      Alcotest.(check bool)
        "publication error class" true (error.class_ = expected)
  | C.Published _ -> Alcotest.fail "publication unexpectedly committed"

let publication_target root role =
  Printf.sprintf "ocaml-mutants-dircap-%s-%s.json"
    (Digest.to_hex (Digest.string root))
    role

let cwd_publication_target target = Filename.concat (Sys.getcwd ()) target

let assert_no_cwd_publication_target label target =
  Alcotest.(check bool)
    label false
    (Sys.file_exists (cwd_publication_target target))

let remove_cwd_publication_target target =
  let path = cwd_publication_target target in
  if Sys.file_exists path then try Sys.remove path with Sys_error _ -> ()

let close_publication_handle label live =
  match !live with
  | None -> ()
  | Some file -> (
      match S.close_file file with
      | C.Cleanup_complete -> live := None
      | cleanup -> expect_cleanup_complete label cleanup)

let close_live_publication_handle live =
  Option.iter (close_quietly S.close_file) !live

let test_atomic_no_replace_publication_and_fault_evidence () =
  with_temp_directory (fun root ->
      let directory = existing_directory root in
      Fun.protect
        (fun () ->
          let final_target = publication_target root "final" in
          let pre_fault_target = publication_target root "pre-fault" in
          let post_fault_target = publication_target root "post-fault" in
          let targets = [ final_target; pre_fault_target; post_fault_target ] in
          List.iter
            (assert_no_cwd_publication_target
               "unique publication target starts absent from cwd")
            targets;
          let publish source target payload =
            write_file (Filename.concat root source) payload;
            let file = publication_handle directory source in
            ( file,
              S.atomic_rename file ~into:directory ~as_:(native_name target)
                ~replacement:C.No_replace )
          in
          Fun.protect
            (fun () ->
              let first, first_result =
                publish "first.pending" final_target "first"
              in
              let first_live = ref (Some first) in
              Fun.protect
                (fun () ->
                  (match first_result with
                  | C.Not_published error -> fail_error error
                  | C.Published { advisories } ->
                      Alcotest.(check int)
                        "ordinary commit has no advisory" 0
                        (List.length advisories));
                  close_publication_handle "winning publication handle"
                    first_live;
                  assert_no_cwd_publication_target
                    "ordinary commit never escapes to cwd" final_target;
                  Alcotest.(check bool)
                    "winning source was renamed" false
                    (Sys.file_exists (Filename.concat root "first.pending"));
                  Alcotest.(check string)
                    "published bytes" "first"
                    (read_file (Filename.concat root final_target)))
                ~finally:(fun () -> close_live_publication_handle first_live);

              let conflict, conflict_result =
                publish "conflict.pending" final_target "foreign"
              in
              let conflict_live = ref (Some conflict) in
              Fun.protect
                (fun () ->
                  expect_not_published C.Already_exists conflict_result;
                  Alcotest.(check string)
                    "losing captured stage remains exact" "foreign"
                    (get_ok (S.read_captured conflict ~limit:64L)).contents;
                  close_publication_handle "losing publication handle"
                    conflict_live;
                  assert_no_cwd_publication_target
                    "conflict never escapes to cwd" final_target;
                  Alcotest.(check string)
                    "existing final was not replaced" "first"
                    (read_file (Filename.concat root final_target));
                  Alcotest.(check string)
                    "losing stage remains exact" "foreign"
                    (read_file (Filename.concat root "conflict.pending")))
                ~finally:(fun () -> close_live_publication_handle conflict_live);

              write_file (Filename.concat root "pre-fault.pending") "pre-fault";
              let pre_fault =
                publication_handle directory "pre-fault.pending"
              in
              let pre_fault_live = ref (Some pre_fault) in
              Fun.protect
                (fun () ->
                  inject_native_internal_fault publish_fault_site true false;
                  (match
                     S.atomic_rename pre_fault ~into:directory
                       ~as_:(native_name pre_fault_target)
                       ~replacement:C.No_replace
                   with
                  | C.Not_published error ->
                      Alcotest.(check string)
                        "pre-commit fault evidence"
                        "injected-publish-before-commit" error.native_code
                  | C.Published _ ->
                      Alcotest.fail "pre-commit fault published the stage");
                  Alcotest.(check string)
                    "pre-fault captured stage remains exact" "pre-fault"
                    (get_ok (S.read_captured pre_fault ~limit:64L)).contents;
                  close_publication_handle "pre-fault publication handle"
                    pre_fault_live;
                  assert_no_cwd_publication_target
                    "pre-commit failure never escapes to cwd" pre_fault_target;
                  Alcotest.(check bool)
                    "pre-fault source remains" true
                    (Sys.file_exists (Filename.concat root "pre-fault.pending"));
                  Alcotest.(check bool)
                    "pre-fault target absent" false
                    (Sys.file_exists (Filename.concat root pre_fault_target)))
                ~finally:(fun () ->
                  close_live_publication_handle pre_fault_live);

              write_file
                (Filename.concat root "post-fault.pending")
                "post-fault";
              let post_fault =
                publication_handle directory "post-fault.pending"
              in
              let post_fault_live = ref (Some post_fault) in
              Fun.protect
                (fun () ->
                  inject_native_internal_fault publish_fault_site false true;
                  (match
                     S.atomic_rename post_fault ~into:directory
                       ~as_:(native_name post_fault_target)
                       ~replacement:C.No_replace
                   with
                  | C.Not_published error -> fail_error error
                  | C.Published { advisories = [ advisory ] } ->
                      Alcotest.(check string)
                        "post-commit advisory evidence"
                        "injected-post-publish-advisory" advisory.native_code
                  | C.Published { advisories } ->
                      Alcotest.failf "expected one post-commit advisory, got %d"
                        (List.length advisories));
                  close_publication_handle "post-fault publication handle"
                    post_fault_live;
                  assert_no_cwd_publication_target
                    "post-commit advisory never escapes to cwd"
                    post_fault_target;
                  Alcotest.(check bool)
                    "post-fault source consumed" false
                    (Sys.file_exists
                       (Filename.concat root "post-fault.pending"));
                  Alcotest.(check string)
                    "post-fault target authoritative" "post-fault"
                    (read_file (Filename.concat root post_fault_target)))
                ~finally:(fun () ->
                  close_live_publication_handle post_fault_live))
            ~finally:(fun () -> List.iter remove_cwd_publication_target targets))
        ~finally:(fun () ->
          reset_native_internal_fault ();
          close_quietly S.close_directory directory))

let test_atomic_replace_publication_and_fault_evidence () =
  with_temp_directory (fun root ->
      let directory = existing_directory root in
      let path name = Filename.concat root name in
      let with_stage name contents action =
        write_file (path name) contents;
        let file = publication_handle directory name in
        Fun.protect
          (fun () -> action file)
          ~finally:(fun () -> close_quietly S.close_file file)
      in
      let expect_published label = function
        | C.Published { advisories = [] } -> ()
        | C.Published { advisories } ->
            Alcotest.failf "%s had %d unexpected advisories" label
              (List.length advisories)
        | C.Not_published error ->
            Alcotest.failf "%s was not published: %s" label error.native_code
      in
      Fun.protect
        (fun () ->
          write_file (path "replace.final") "old";
          with_stage "replace.pending" "new" (fun file ->
              let conflict =
                S.atomic_rename file ~into:directory
                  ~as_:(native_name "replace.final")
                  ~replacement:C.No_replace
              in
              expect_not_published C.Already_exists conflict;
              Alcotest.(check string)
                "conflicting stage stays live" "new"
                (get_ok (S.read_captured file ~limit:64L)).contents;
              Alcotest.(check string)
                "no-replace keeps old destination" "old"
                (read_file (path "replace.final"));
              expect_published "same-handle replacement"
                (S.atomic_rename file ~into:directory
                   ~as_:(native_name "replace.final")
                   ~replacement:C.Replace);
              Alcotest.(check string)
                "published source handle remains exact" "new"
                (get_ok (S.read_captured file ~limit:64L)).contents);
          Alcotest.(check bool)
            "replace consumes source binding" false
            (Sys.file_exists (path "replace.pending"));
          Alcotest.(check string)
            "replace atomically installs new bytes" "new"
            (read_file (path "replace.final"));

          write_file (path "replace-pre.final") "pre-old";
          with_stage "replace-pre.pending" "pre-new" (fun file ->
              inject_native_internal_fault publish_fault_site true false;
              (match
                 S.atomic_rename file ~into:directory
                   ~as_:(native_name "replace-pre.final")
                   ~replacement:C.Replace
               with
              | C.Not_published error ->
                  Alcotest.(check string)
                    "replace pre-commit fault is exact"
                    "injected-publish-before-commit" error.native_code
              | C.Published _ ->
                  Alcotest.fail "replace pre-commit fault was hidden");
              Alcotest.(check string)
                "pre-commit replace keeps stage live" "pre-new"
                (get_ok (S.read_captured file ~limit:64L)).contents;
              Alcotest.(check string)
                "pre-commit replace keeps destination" "pre-old"
                (read_file (path "replace-pre.final")));
          Alcotest.(check bool)
            "pre-commit replace keeps source binding" true
            (Sys.file_exists (path "replace-pre.pending"));

          write_file (path "replace-post.final") "post-old";
          with_stage "replace-post.pending" "post-new" (fun file ->
              inject_native_internal_fault publish_fault_site false true;
              (match
                 S.atomic_rename file ~into:directory
                   ~as_:(native_name "replace-post.final")
                   ~replacement:C.Replace
               with
              | C.Published { advisories = [ advisory ] } ->
                  Alcotest.(check string)
                    "replace post-commit advisory is exact"
                    "injected-post-publish-advisory" advisory.native_code
              | C.Published { advisories } ->
                  Alcotest.failf
                    "replace expected one post-commit advisory, got %d"
                    (List.length advisories)
              | C.Not_published error -> fail_error error);
              Alcotest.(check string)
                "post-commit source handle remains exact" "post-new"
                (get_ok (S.read_captured file ~limit:64L)).contents);
          Alcotest.(check bool)
            "post-commit replace consumes source" false
            (Sys.file_exists (path "replace-post.pending"));
          Alcotest.(check string)
            "post-commit advisory does not undo replacement" "post-new"
            (read_file (path "replace-post.final")))
        ~finally:(fun () ->
          reset_native_internal_fault ();
          close_quietly S.close_directory directory))

let close_descriptor_noerr descriptor =
  try Unix.close descriptor with Unix.Unix_error _ -> ()

let with_child_input action =
  let null_device = if Sys.win32 then "NUL" else "/dev/null" in
  let descriptor = Unix.openfile null_device [ Unix.O_RDONLY ] 0 in
  Fun.protect
    (fun () -> action descriptor)
    ~finally:(fun () -> close_descriptor_noerr descriptor)

let with_child_output path action =
  let descriptor =
    Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] 0o600
  in
  Fun.protect
    (fun () -> action descriptor)
    ~finally:(fun () -> close_descriptor_noerr descriptor)

let create_ready_child_process ~diagnostics executable environment =
  with_child_input (fun child_input ->
      with_child_output diagnostics (fun child_error_output ->
          let ready_input, ready_output = Unix.pipe ~cloexec:true () in
          try
            let process =
              Unix.create_process_env executable [| executable |] environment
                child_input ready_output child_error_output
            in
            close_descriptor_noerr ready_output;
            (process, ready_input)
          with exception_ ->
            close_descriptor_noerr ready_output;
            close_descriptor_noerr ready_input;
            raise exception_))

type diagnostic_paths = { stdout_path : string; stderr_path : string }

let create_diagnostic_child_process ~diagnostics executable environment =
  with_child_input (fun child_input ->
      with_child_output diagnostics.stdout_path (fun child_output ->
          with_child_output diagnostics.stderr_path (fun child_error_output ->
              Unix.create_process_env executable [| executable |] environment
                child_input child_output child_error_output)))

let child_diagnostics paths =
  let one label path =
    if not (Sys.file_exists path) then
      Printf.sprintf "\n%s diagnostics unavailable" label
    else
      try
        match read_file path with
        | "" -> ""
        | contents -> Printf.sprintf "\nchild %s:\n%s" label contents
      with Sys_error message ->
        Printf.sprintf "\n%s diagnostics unavailable: %s" label message
  in
  String.concat "" (List.map (fun (label, path) -> one label path) paths)

let read_child_ready descriptor =
  let channel = Unix.in_channel_of_descr descriptor in
  Fun.protect
    (fun () ->
      match input_line channel with
      | message ->
          let length = String.length message in
          let message =
            if length > 0 && message.[length - 1] = '\r' then
              String.sub message 0 (length - 1)
            else message
          in
          if String.equal message "ready" then Ok ()
          else Error (Printf.sprintf "unexpected readiness %S" message)
      | exception End_of_file -> Error "readiness pipe closed before handshake")
    ~finally:(fun () -> close_in_noerr channel)

let standard_input_is_closed () =
  match Unix.fstat Unix.stdin with
  | exception Unix.Unix_error (Unix.EBADF, _, _) -> true
  | _ -> false

let publication_handshake_timeout_seconds = 10.0
let publication_handshake_poll_seconds = 0.01

let wait_for_path path =
  let deadline =
    Unix.gettimeofday () +. publication_handshake_timeout_seconds
  in
  let rec loop () =
    if Sys.file_exists path then true
    else if Unix.gettimeofday () >= deadline then false
    else (
      ignore (Unix.select [] [] [] publication_handshake_poll_seconds);
      loop ())
  in
  loop ()

let publication_child_exit_code root source target _ready release =
  try
    let directory = existing_directory root in
    Fun.protect
      (fun () ->
        match
          S.open_file_for_publish_no_follow directory (native_name source)
        with
        | Error _ -> 20
        | Ok file ->
            Fun.protect
              (fun () ->
                output_string stdout "ready\n";
                flush stdout;
                if not (wait_for_path release) then 21
                else
                  match
                    S.atomic_rename file ~into:directory
                      ~as_:(native_name target) ~replacement:C.No_replace
                  with
                  | C.Published _ -> 0
                  | C.Not_published { class_ = C.Already_exists; _ } -> 10
                  | C.Not_published _ -> 22)
              ~finally:(fun () -> close_quietly S.close_file file))
      ~finally:(fun () -> close_quietly S.close_directory directory)
  with _ -> 23

type ready_child_process = {
  process : int;
  ready_input : Unix.file_descr;
  diagnostics : string;
  mutable readiness_open : bool;
  mutable status : Unix.process_status option;
}

let spawn_publication_child ~root ~source ~target ~ready ~release =
  let executable =
    if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name
  in
  let environment =
    Array.append (Unix.environment ())
      [|
        "OCAML_MUTANTS_DIRCAP_PUBLISH_CHILD=" ^ root;
        "OCAML_MUTANTS_DIRCAP_PUBLISH_SOURCE=" ^ source;
        "OCAML_MUTANTS_DIRCAP_PUBLISH_TARGET=" ^ target;
        "OCAML_MUTANTS_DIRCAP_PUBLISH_READY=" ^ ready;
        "OCAML_MUTANTS_DIRCAP_PUBLISH_RELEASE=" ^ release;
      |]
  in
  let diagnostics = ready ^ ".stderr.log" in
  let process, ready_input =
    create_ready_child_process ~diagnostics executable environment
  in
  { process; ready_input; diagnostics; readiness_open = true; status = None }

let close_child_readiness child =
  if child.readiness_open then (
    child.readiness_open <- false;
    close_descriptor_noerr child.ready_input)

let await_publication_child child =
  child.readiness_open <- false;
  match read_child_ready child.ready_input with
  | Ok () -> ()
  | Error message ->
      Alcotest.failf "publication child handshake failed: %s%s" message
        (child_diagnostics [ ("stderr", child.diagnostics) ])

let reap_child child =
  match child.status with
  | Some status -> status
  | None ->
      let _, status = Unix.waitpid [] child.process in
      child.status <- Some status;
      status

let executable_path () =
  if Filename.is_relative Sys.executable_name then
    Filename.concat (Sys.getcwd ()) Sys.executable_name
  else Sys.executable_name

let spawn_closed_stdin_probe root =
  let environment =
    Array.append (Unix.environment ())
      [| "OCAML_MUTANTS_DIRCAP_CLOSED_STDIN_PROBE=" ^ root |]
  in
  let diagnostics = Filename.concat root "closed-stdin-probe.stderr.log" in
  let process, ready_input =
    create_ready_child_process ~diagnostics (executable_path ()) environment
  in
  { process; ready_input; diagnostics; readiness_open = true; status = None }

let closed_stdin_leaf_exit_code () =
  output_string stdout "ready\n";
  flush stdout;
  0

let closed_stdin_probe_exit_code root =
  try
    Unix.close Unix.stdin;
    if not (standard_input_is_closed ()) then 30
    else
      let environment =
        Array.append (Unix.environment ())
          [| "OCAML_MUTANTS_DIRCAP_CLOSED_STDIN_LEAF=1" |]
      in
      let diagnostics = Filename.concat root "closed-stdin-leaf.stderr.log" in
      let process, ready_input =
        create_ready_child_process ~diagnostics (executable_path ()) environment
      in
      let readiness = read_child_ready ready_input in
      let _, status = Unix.waitpid [] process in
      if not (standard_input_is_closed ()) then 31
      else
        match (readiness, status) with
        | Ok (), Unix.WEXITED 0 ->
            output_string stdout "ready\n";
            flush stdout;
            0
        | _ ->
            prerr_string (child_diagnostics [ ("leaf stderr", diagnostics) ]);
            32
  with exception_ ->
    prerr_endline (Printexc.to_string exception_);
    33

let assert_child_spawn_survives_closed_stdin root =
  let child = spawn_closed_stdin_probe root in
  Fun.protect
    (fun () ->
      child.readiness_open <- false;
      let readiness = read_child_ready child.ready_input in
      let status = reap_child child in
      match (readiness, status) with
      | Ok (), Unix.WEXITED 0 -> ()
      | Error message, _ ->
          Alcotest.failf "closed-stdin launch handshake failed: %s%s" message
            (child_diagnostics [ ("stderr", child.diagnostics) ])
      | Ok (), status ->
          Alcotest.failf "closed-stdin launch exited abnormally (%s)%s"
            (match status with
            | Unix.WEXITED code -> Printf.sprintf "exit %d" code
            | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
            | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal)
            (child_diagnostics [ ("stderr", child.diagnostics) ]))
    ~finally:(fun () ->
      close_child_readiness child;
      ignore (reap_child child))

let test_cross_process_atomic_no_replace_conflict () =
  with_temp_directory (fun root ->
      let target = publication_target root "contended" in
      let left_source = "left.pending" in
      let right_source = "right.pending" in
      let left_ready = Filename.concat root "left.ready" in
      let right_ready = Filename.concat root "right.ready" in
      let release = Filename.concat root "release" in
      write_file (Filename.concat root left_source) "left";
      write_file (Filename.concat root right_source) "right";
      assert_no_cwd_publication_target
        "cross-process target starts absent from cwd" target;
      Fun.protect
        (fun () ->
          assert_child_spawn_survives_closed_stdin root;
          let children = ref [] in
          Fun.protect
            (fun () ->
              let left =
                spawn_publication_child ~root ~source:left_source ~target
                  ~ready:left_ready ~release
              in
              children := left :: !children;
              let right =
                spawn_publication_child ~root ~source:right_source ~target
                  ~ready:right_ready ~release
              in
              children := right :: !children;
              await_publication_child left;
              await_publication_child right;
              write_file release "release";
              let left_status = reap_child left in
              let right_status = reap_child right in
              let exit_code = function
                | Unix.WEXITED code -> code
                | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> 255
              in
              let actual =
                List.sort Int.compare
                  [ exit_code left_status; exit_code right_status ]
              in
              if actual <> [ 0; 10 ] then
                Alcotest.failf "publication exit codes were [%s]%s%s"
                  (String.concat ";" (List.map string_of_int actual))
                  (child_diagnostics [ ("left stderr", left.diagnostics) ])
                  (child_diagnostics [ ("right stderr", right.diagnostics) ]);
              Alcotest.(check (list int))
                "exactly one cross-process commit wins" [ 0; 10 ] actual;
              assert_no_cwd_publication_target
                "cross-process commit never escapes to cwd" target;
              let final = read_file (Filename.concat root target) in
              Alcotest.(check bool)
                "winner bytes are one complete stage" true
                (String.equal final "left" || String.equal final "right");
              Alcotest.(check int)
                "exactly one losing pending remains" 1
                (List.length
                   (List.filter Sys.file_exists
                      [
                        Filename.concat root left_source;
                        Filename.concat root right_source;
                      ])))
            ~finally:(fun () ->
              if !children <> [] && not (Sys.file_exists release) then
                write_file release "release";
              List.iter close_child_readiness !children;
              List.iter (fun child -> ignore (reap_child child)) !children))
        ~finally:(fun () -> remove_cwd_publication_target target))

let test_root_relative_lock_modes_and_identity () =
  with_temp_directory (fun root ->
      let original = Filename.concat root "lock-root" in
      let moved = Filename.concat root "renamed-lock-root" in
      Unix.mkdir original 0o700;
      let directory = existing_directory original in
      let name = native_name "coordination.lock" in
      Fun.protect
        (fun () ->
          rename "rename captured lock root" original moved;
          let first = acquire_lock (S.try_lock directory name C.Shared) in
          let first_identity = S.lock_file_identity first in
          Alcotest.(check bool)
            "lock owner is captured from the directory" true
            (S.owner_equal (S.lock_owner first) (S.dir_owner directory));
          Alcotest.(check bool)
            "lock records captured directory identity" true
            (S.Identity.equal
               (S.lock_directory_identity first)
               (S.dir_identity directory));
          let observed =
            match get_failure (S.probe_entry_no_follow directory name) with
            | Some stat -> stat
            | None -> Alcotest.fail "opened lock file is not name-bound"
          in
          Alcotest.(check bool)
            "live lock handle identity matches root-relative entry" true
            (S.Identity.equal first_identity observed.identity);
          if Sys.win32 then
            Alcotest.(check bool)
              "held Windows lock refuses lock-file rename sharing" true
              (try
                 Sys.rename
                   (Filename.concat moved "coordination.lock")
                   (Filename.concat moved "renamed.lock");
                 false
               with Sys_error _ -> true);
          let second = acquire_lock (S.try_lock directory name C.Shared) in
          expect_lock_busy (S.try_lock directory name C.Exclusive);
          expect_cleanup_complete "first shared release" (S.release_lock first);
          expect_lock_busy (S.try_lock directory name C.Exclusive);
          expect_cleanup_complete "second shared release"
            (S.release_lock second);
          let exclusive =
            acquire_lock (S.try_lock directory name C.Exclusive)
          in
          Alcotest.(check bool)
            "persistent lock identity is stable while name is retained" true
            (S.Identity.equal first_identity (S.lock_file_identity exclusive));
          expect_lock_busy (S.try_lock directory name C.Shared);
          expect_cleanup_complete "exclusive release" (S.release_lock exclusive);
          Alcotest.(check bool)
            "lock file was created relative to renamed captured root" true
            (Sys.file_exists (Filename.concat moved "coordination.lock"));
          if not Sys.win32 then
            Alcotest.(check int)
              "lock creation grants no group or other mode bits" 0
              ((Unix.stat (Filename.concat moved "coordination.lock")).st_perm
             land 0o077);
          Alcotest.(check bool)
            "lock release never unlinks persistent entry" true
            (match get_failure (S.probe_entry_no_follow directory name) with
            | Some stat ->
                stat.kind = C.Regular
                && S.Identity.equal first_identity stat.identity
            | None -> false))
        ~finally:(fun () -> close_quietly S.close_directory directory))

let lock_child_exit_code root mode expected =
  try
    let directory = existing_directory root in
    Fun.protect
      (fun () ->
        let result = S.try_lock directory (native_name "process.lock") mode in
        match (expected, result) with
        | "busy", Ok `Busy -> 0
        | "acquired", Ok (`Acquired lock) ->
            if S.release_lock lock = C.Cleanup_complete then 0 else 1
        | _, Ok (`Acquired lock) ->
            ignore (S.release_lock lock);
            1
        | _, Ok `Busy | _, Error _ -> 1)
      ~finally:(fun () -> close_quietly S.close_directory directory)
  with _ -> 1

let run_lock_child root mode expected =
  let executable =
    if Filename.is_relative Sys.executable_name then
      Filename.concat (Sys.getcwd ()) Sys.executable_name
    else Sys.executable_name
  in
  let environment =
    Array.append (Unix.environment ())
      [|
        "OCAML_MUTANTS_DIRCAP_LOCK_CHILD=" ^ root;
        ("OCAML_MUTANTS_DIRCAP_LOCK_MODE="
        ^ match mode with C.Shared -> "shared" | C.Exclusive -> "exclusive");
        "OCAML_MUTANTS_DIRCAP_LOCK_EXPECT=" ^ expected;
      |]
  in
  let mode_name =
    match mode with C.Shared -> "shared" | C.Exclusive -> "exclusive"
  in
  let prefix =
    Filename.concat root ("lock-child-" ^ mode_name ^ "-" ^ expected)
  in
  let diagnostics =
    {
      stdout_path = prefix ^ ".stdout.log";
      stderr_path = prefix ^ ".stderr.log";
    }
  in
  let child =
    create_diagnostic_child_process ~diagnostics executable environment
  in
  let _, status = Unix.waitpid [] child in
  if status <> Unix.WEXITED 0 then
    Alcotest.failf "cross-process %s failed%s" expected
      (child_diagnostics
         [
           ("stdout", diagnostics.stdout_path);
           ("stderr", diagnostics.stderr_path);
         ]);
  Alcotest.(check bool)
    ("cross-process " ^ expected)
    true (status = Unix.WEXITED 0)

let test_lock_fork_ownership_and_cross_process_contention () =
  with_temp_directory (fun root ->
      let directory = existing_directory root in
      let name = native_name "process.lock" in
      let parent_shared = acquire_lock (S.try_lock directory name C.Shared) in
      Fun.protect
        (fun () ->
          let exercise_contention () =
            run_lock_child root C.Shared "acquired";
            run_lock_child root C.Exclusive "busy";
            if not Sys.win32 then
              match Unix.fork () with
              | 0 ->
                  Unix._exit
                    (if S.release_lock parent_shared = C.Cleanup_local_only then
                       0
                     else 1)
              | child ->
                  let _, status = Unix.waitpid [] child in
                  Alcotest.(check bool)
                    "fork child closes only its inherited lock handle" true
                    (status = Unix.WEXITED 0);
                  expect_lock_busy (S.try_lock directory name C.Exclusive);
                  expect_cleanup_complete "parent shared release"
                    (S.release_lock parent_shared);
                  let parent_exclusive =
                    acquire_lock (S.try_lock directory name C.Exclusive)
                  in
                  run_lock_child root C.Shared "busy";
                  expect_cleanup_complete "parent exclusive release"
                    (S.release_lock parent_exclusive);
                  run_lock_child root C.Exclusive "acquired"
          in
          exercise_contention ())
        ~finally:(fun () ->
          close_quietly S.release_lock parent_shared;
          close_quietly S.close_directory directory))

let test_lock_fault_evidence () =
  with_temp_directory (fun root ->
      let directory = existing_directory root in
      let name value = native_name (value ^ ".lock") in
      let expect_open_failure result =
        match result with
        | Error { C.primary = C.Operation_error error; suppressed = [] } ->
            Alcotest.(check bool)
              "open fault remains the acquisition primary" true
              (error.operation = C.Try_lock
              && error.native_domain = C.Contract
              && error.native_code = "injected-lock-open-failure")
        | Error failure ->
            Alcotest.failf "unexpected open failure shape: %s"
              (C.issue_error failure.primary).native_code
        | Ok _ -> Alcotest.fail "injected lock open unexpectedly succeeded"
      in
      let release_problems = function
        | C.Cleanup_failed { primary; suppressed } -> primary :: suppressed
        | C.Cleanup_complete | C.Cleanup_local_only ->
            Alcotest.fail "injected lock release unexpectedly succeeded"
      in
      Fun.protect
        (fun () ->
          let open_name = name "open-fault" in
          inject_native_internal_fault lock_open_fault_site true false;
          expect_open_failure (S.try_lock directory open_name C.Exclusive);
          Alcotest.(check bool)
            "pre-open fault commits no lock file" false
            (Sys.file_exists (Filename.concat root "open-fault.lock"));

          inject_native_internal_fault lock_acquire_fault_site true true;
          expect_action_then_cleanup ~label:"lock action plus close"
            ~action_operation:C.Try_lock ~cleanup_operation:C.Release_lock
            (S.try_lock directory (name "acquire-fault") C.Exclusive);

          let contended_name = name "busy-close-fault" in
          let held =
            acquire_lock (S.try_lock directory contended_name C.Exclusive)
          in
          inject_native_internal_fault lock_acquire_fault_site false true;
          expect_cleanup_primary ~label:"busy lock plus internal close"
            ~cleanup_operation:C.Release_lock
            (S.try_lock directory contended_name C.Exclusive);
          expect_cleanup_complete "contended lock release" (S.release_lock held);

          let unlock_fault =
            acquire_lock
              (S.try_lock directory (name "unlock-fault") C.Exclusive)
          in
          inject_native_internal_fault lock_release_fault_site true false;
          (match release_problems (S.release_lock unlock_fault) with
          | [ problem ] ->
              Alcotest.(check bool)
                "close proves release after unlock error" true
                (problem.error.native_code = "injected-lock-unlock-failure"
                && problem.local_handle_state = C.Closed
                && problem.namespace_released)
          | _ -> Alcotest.fail "unlock failure cleanup order was not exact");

          let close_fault =
            acquire_lock (S.try_lock directory (name "close-fault") C.Exclusive)
          in
          inject_native_internal_fault lock_release_fault_site false true;
          (match release_problems (S.release_lock close_fault) with
          | [ problem ] ->
              Alcotest.(check bool)
                "unlock evidence survives close failure" true
                (problem.error.native_code = "injected-lock-close-failure"
                && problem.namespace_released
                && problem.local_handle_state
                   = if Sys.win32 then C.Still_open else C.Invalidated_unknown)
          | _ -> Alcotest.fail "close failure cleanup order was not exact");
          if Sys.win32 then
            expect_cleanup_complete "retryable close-only release"
              (S.release_lock close_fault);

          let double_fault =
            acquire_lock
              (S.try_lock directory (name "double-fault") C.Exclusive)
          in
          inject_native_internal_fault lock_release_fault_site true true;
          (match release_problems (S.release_lock double_fault) with
          | [ unlock_problem; close_problem ] ->
              let expected_state =
                if Sys.win32 then C.Still_open else C.Invalidated_unknown
              in
              Alcotest.(check bool)
                "unlock then close errors remain ordered" true
                (unlock_problem.error.native_code
                 = "injected-lock-unlock-failure"
                && close_problem.error.native_code
                   = "injected-lock-close-failure"
                && unlock_problem.local_handle_state = expected_state
                && close_problem.local_handle_state = expected_state
                && (not unlock_problem.namespace_released)
                && not close_problem.namespace_released)
          | _ -> Alcotest.fail "double release failure evidence was flattened");
          if Sys.win32 then
            expect_cleanup_complete "retryable unlock and close release"
              (S.release_lock double_fault))
        ~finally:(fun () ->
          reset_native_internal_fault ();
          close_quietly S.close_directory directory))

let run_main () =
  match Sys.getenv_opt "OCAML_MUTANTS_DIRCAP_PUBLISH_CHILD" with
  | Some root ->
      let source =
        Option.get (Sys.getenv_opt "OCAML_MUTANTS_DIRCAP_PUBLISH_SOURCE")
      in
      let target =
        Option.get (Sys.getenv_opt "OCAML_MUTANTS_DIRCAP_PUBLISH_TARGET")
      in
      let ready =
        Option.get (Sys.getenv_opt "OCAML_MUTANTS_DIRCAP_PUBLISH_READY")
      in
      let release =
        Option.get (Sys.getenv_opt "OCAML_MUTANTS_DIRCAP_PUBLISH_RELEASE")
      in
      exit (publication_child_exit_code root source target ready release)
  | None -> (
      match Sys.getenv_opt "OCAML_MUTANTS_DIRCAP_LOCK_CHILD" with
      | Some root ->
          let mode =
            match Sys.getenv_opt "OCAML_MUTANTS_DIRCAP_LOCK_MODE" with
            | Some "shared" -> C.Shared
            | Some "exclusive" -> C.Exclusive
            | _ -> exit 2
          in
          let expected =
            match Sys.getenv_opt "OCAML_MUTANTS_DIRCAP_LOCK_EXPECT" with
            | Some expected -> expected
            | None -> exit 2
          in
          exit (lock_child_exit_code root mode expected)
      | None ->
          Alcotest.run "dir-cap-system-contract"
            [
              ( "native",
                [
                  Alcotest.test_case "native names and captured reads" `Quick
                    test_native_names_and_captured_reads;
                  Alcotest.test_case "reparse rejection" `Quick
                    test_no_follow_reparse;
                  Alcotest.test_case "materialization progress" `Quick
                    test_materialization_progress;
                  Alcotest.test_case "materialization missing sibling proof"
                    `Quick test_materialization_missing_sibling_proof;
                  Alcotest.test_case "materialization rebinding and collision"
                    `Quick test_materialization_rebinding_and_collision;
                  Alcotest.test_case "materialization fault evidence" `Quick
                    test_materialization_fault_evidence;
                  Alcotest.test_case
                    "owner-private directory capability creation" `Quick
                    test_owner_private_directory_creation_is_capability_relative;
                  Alcotest.test_case "directory creation fault evidence" `Quick
                    test_owner_private_directory_creation_fault_evidence;
                  Alcotest.test_case "owner-private file capability creation"
                    `Quick
                    test_owner_private_file_creation_is_capability_relative;
                  Alcotest.test_case "file creation fault evidence" `Quick
                    test_owner_private_file_creation_fault_evidence;
                  Alcotest.test_case "captured handle deletion evidence" `Quick
                    test_captured_handle_deletion_and_fault_evidence;
                  Alcotest.test_case "identity relationship" `Quick
                    test_identity_relationship;
                  Alcotest.test_case "invalid names and private file policy"
                    `Quick test_invalid_names_and_unsupported_mutation;
                  Alcotest.test_case "fork-local owner boundary" `Quick
                    test_fork_owner;
                  Alcotest.test_case "independent directory duplicate" `Quick
                    test_duplicate_directory_is_independent;
                  Alcotest.test_case "lossless internal close evidence" `Quick
                    test_internal_close_evidence_is_lossless;
                  Alcotest.test_case "enumeration resource evidence" `Quick
                    test_enumeration_resource_evidence;
                  Alcotest.test_case "atomic no-replace and fault evidence"
                    `Quick test_atomic_no_replace_publication_and_fault_evidence;
                  Alcotest.test_case "atomic replace and fault evidence" `Quick
                    test_atomic_replace_publication_and_fault_evidence;
                  Alcotest.test_case "cross-process atomic no-replace" `Quick
                    test_cross_process_atomic_no_replace_conflict;
                  Alcotest.test_case "root-relative lock modes and identity"
                    `Quick test_root_relative_lock_modes_and_identity;
                  Alcotest.test_case "lock fork ownership and contention" `Quick
                    test_lock_fork_ownership_and_cross_process_contention;
                  Alcotest.test_case "lock fault evidence" `Quick
                    test_lock_fault_evidence;
                ] );
            ])

let () =
  match Sys.getenv_opt "OCAML_MUTANTS_DIRCAP_CLOSED_STDIN_LEAF" with
  | Some _ -> exit (closed_stdin_leaf_exit_code ())
  | None -> (
      match Sys.getenv_opt "OCAML_MUTANTS_DIRCAP_CLOSED_STDIN_PROBE" with
      | Some root -> exit (closed_stdin_probe_exit_code root)
      | None -> run_main ())
