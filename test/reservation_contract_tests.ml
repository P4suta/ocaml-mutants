module Core = Ocaml_mutants_core
module Engine = Ocaml_mutants_engine

external inject_native_internal_fault : string -> bool -> bool -> unit
  = "ocaml_mutants_dircap_test_inject"

let native_lock_acquire_fault_site = "lock-acquire"
let create_directory_after_commit_fault_site = "create-directory-after-commit"
let create_file_after_commit_fault_site = "create-file-after-commit"
let delete_before_commit_fault_site = "delete-before-commit"
let reset_native_internal_fault () = inject_native_internal_fault "" false false

let get_ok = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%a" Engine.Error.pp error

let command =
  match Core.Nonempty_argv.of_list [ "dune"; "runtest" ] with
  | Ok command -> command
  | Error message -> failwith message

let report id : Engine.Run_store.run =
  {
    metadata =
      {
        id;
        started_at = "2026-01-01T00:00:00Z";
        finished_at = "2026-01-01T00:00:01Z";
        workspace_digest = String.make 64 'a';
        toolchain = "test-toolchain";
        profile = Core.Operator.Profile.Balanced;
        selection = "reservation-contract";
        test_command = command;
        baseline_duration = None;
        baseline_stages = [];
        timeout = None;
        cache_mode = "off";
        cache_key = "unavailable";
      };
    status = Engine.Run_store.Completed;
    results = [];
    completeness = Engine.Run_store.Complete;
    expectations = [];
    skipped = [];
    warnings = [];
  }

let create_directory path = Unix.mkdir path 0o700

let create_directories path =
  match Engine.Util.mkdir_p path with
  | Ok () -> ()
  | Error message -> Alcotest.failf "cannot create %s: %s" path message

let with_layout action =
  let parent = Filename.temp_file "ocaml-mutants-reservation-" ".tmp" in
  Sys.remove parent;
  create_directory parent;
  (* Resolved: the OS temp prefix may itself be a symlink (macOS /var, /tmp),
     which the capability walk refuses to follow. *)
  let parent = Unix.realpath parent in
  Fun.protect
    ~finally:(fun () -> ignore (Engine.Util.remove_tree parent))
    (fun () ->
      let cache = Filename.concat parent "cache" in
      let workspace = Filename.concat parent "workspace" in
      create_directory workspace;
      let store =
        get_ok (Engine.Run_store.create ~workspace ~directory:cache ())
      in
      action ~parent ~cache ~workspace ~store)

let with_fault_layout ?next_reservation_sequence fail action =
  let parent = Filename.temp_file "ocaml-mutants-publication-" ".tmp" in
  Sys.remove parent;
  create_directory parent;
  let parent = Unix.realpath parent in
  Fun.protect
    ~finally:(fun () -> ignore (Engine.Util.remove_tree parent))
    (fun () ->
      let cache = Filename.concat parent "cache" in
      let workspace = Filename.concat parent "workspace" in
      create_directory workspace;
      let store =
        get_ok
          (Engine.Run_store.For_testing.create ~workspace ~directory:cache
             ?next_reservation_sequence ~fail ())
      in
      action ~parent ~cache ~workspace ~store)

let raw_remove_noerr path = ignore (Engine.Util.remove_tree path)

let protect_reservation store reservation action =
  Fun.protect
    ~finally:(fun () ->
      ignore (Engine.Run_store.abandon_reservation store reservation))
    action

let monotonic_sequence initial =
  let next = ref initial in
  fun () ->
    let current = !next in
    next := Int64.succ current;
    Ok current

let reservation_markers cache =
  Engine.Util.files_recursive cache
  |> List.filter (fun path -> Filename.check_suffix path ".reserved")
  |> List.sort String.compare

let check_marker_count cache expected =
  Alcotest.(check int)
    "active reservation markers" expected
    (List.length (reservation_markers cache))

let marker_path cache =
  match reservation_markers cache with
  | [ marker ] -> Filename.concat cache marker
  | markers ->
      Alcotest.failf "expected one marker, found %d" (List.length markers)

let report_paths marker =
  let stem =
    String.sub marker 0 (String.length marker - String.length ".reserved")
  in
  (stem ^ ".json", stem ^ ".json.pending")

let rebase_beneath ~from ~onto path =
  let root_length = String.length from in
  if
    String.length path <= root_length
    || (not (String.equal from (String.sub path 0 root_length)))
    ||
    let separator = path.[root_length] in
    separator <> '/' && separator <> '\\'
  then Alcotest.failf "%s is not beneath %s" path from;
  Filename.concat onto
    (String.sub path (root_length + 1) (String.length path - root_length - 1))

let read_file_or_fail path =
  match Engine.Util.read_file path with
  | Ok contents -> contents
  | Error message -> Alcotest.failf "cannot read %s: %s" path message

let rec native_error_codes error =
  Option.to_list (List.assoc_opt "native_code" (Engine.Error.context error))
  @ List.concat_map native_error_codes (Engine.Error.suppressed error)

let check_error_cause expected = function
  | Ok _ -> Alcotest.failf "expected %s error" expected
  | Error error ->
      Alcotest.(check string)
        "error cause" expected
        (Engine.Error.cause_name (Engine.Error.cause error))

let reserve store =
  get_ok (Engine.Run_store.reserve store ~started_at:"20260101T000000Z")

let allocator_started_at = "20260101T000000Z"

let allocator_id sequence =
  Printf.sprintf "%s-p%d-s%Ld" allocator_started_at (Unix.getpid ()) sequence

let normalize_scope path =
  let normalized = Core.Mutant.normalize_path path in
  if Sys.win32 then String.lowercase_ascii normalized else normalized

let finalize_clean staged =
  let finalization = get_ok (Engine.Run_store.finalize_run staged) in
  (match finalization.cleanup_errors with
  | [] -> ()
  | errors ->
      Alcotest.failf "unexpected finalization errors: %a" Engine.Error.pp
        (List.hd errors));
  finalization

let publish_clean store reservation run =
  let staged = get_ok (Engine.Run_store.stage_run store reservation) in
  let finalization = finalize_clean staged in
  get_ok (Engine.Run_store.publish_run finalization.publication run)

let test_structural_id_and_exact_marker () =
  with_fault_layout ~next_reservation_sequence:(monotonic_sequence 0L)
    (fun _ -> None)
    (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let reservation =
        get_ok (Engine.Run_store.reserve store ~started_at:allocator_started_at)
      in
      Fun.protect
        ~finally:(fun () ->
          ignore (Engine.Run_store.abandon_reservation store reservation))
        (fun () ->
          let id = Engine.Run_store.reservation_id reservation in
          let run_id = Core.Run_id.to_string id in
          Alcotest.(check string)
            "readable structural ID" (allocator_id 0L) run_id;
          let marker = marker_path cache in
          let scope = Filename.dirname (Filename.dirname marker) in
          let normalized_scope = normalize_scope scope in
          let owner_pid = Unix.getpid () in
          let proof =
            String.concat "\000"
              [
                "ocaml-mutants-reservation-owner-v2";
                normalized_scope;
                run_id;
                string_of_int owner_pid;
              ]
            |> Engine.Util.sha256
          in
          let expected =
            Printf.sprintf
              "owner=ocaml-mutants\n\
               schema=2\n\
               scope=%s\n\
               run_id=%s\n\
               owner_pid=%d\n\
               proof=%s\n"
              (Engine.Util.sha256 normalized_scope)
              run_id owner_pid proof
          in
          match Engine.Util.read_file marker with
          | Ok contents ->
              Alcotest.(check string)
                "marker is exact ownership evidence" expected contents
          | Error _ when Sys.win32 ->
              Alcotest.(check bool)
                "live marker handle excludes independent ambient capture" true
                (Result.is_error (Engine.Util.read_file marker))
          | Error message -> Alcotest.fail message))

let test_collisions_continue_until_first_free_name () =
  let forced_collision_count = 64 in
  let sequence_calls = ref 0 in
  let sequence = monotonic_sequence 0L in
  let next_reservation_sequence () =
    incr sequence_calls;
    sequence ()
  in
  with_fault_layout ~next_reservation_sequence
    (fun _ -> None)
    (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let primer =
        get_ok (Engine.Run_store.reserve store ~started_at:allocator_started_at)
      in
      let runs = Filename.dirname (marker_path cache) in
      get_ok (Engine.Run_store.abandon_reservation store primer);
      let collisions =
        List.init forced_collision_count (fun index ->
            let sequence = Int64.of_int (index + 1) in
            let path =
              Filename.concat runs (allocator_id sequence ^ ".reserved")
            in
            (match Engine.Util.write_file path "pre-existing\n" with
            | Ok () -> ()
            | Error message -> Alcotest.fail message);
            path)
      in
      Fun.protect
        ~finally:(fun () ->
          List.iter
            (fun path -> if Sys.file_exists path then Sys.remove path)
            collisions)
        (fun () ->
          let reservation =
            get_ok
              (Engine.Run_store.reserve store ~started_at:allocator_started_at)
          in
          Fun.protect
            ~finally:(fun () ->
              ignore (Engine.Run_store.abandon_reservation store reservation))
            (fun () ->
              Alcotest.(check string)
                "first free structural name wins"
                (allocator_id (Int64.of_int (forced_collision_count + 1)))
                (Core.Run_id.to_string
                   (Engine.Run_store.reservation_id reservation));
              Alcotest.(check int)
                "one sequence value per exclusive-create attempt"
                (forced_collision_count + 2)
                !sequence_calls)))

let test_concurrent_reservations_are_unique () =
  let concurrent_reservation_count = 8 in
  with_layout (fun ~parent:_ ~cache:_ ~workspace:_ ~store ->
      let start = Atomic.make false in
      let domains =
        List.init concurrent_reservation_count (fun _ ->
            Domain.spawn (fun () ->
                while not (Atomic.get start) do
                  Domain.cpu_relax ()
                done;
                Engine.Run_store.reserve store ~started_at:allocator_started_at))
      in
      Atomic.set start true;
      let outcomes = List.map Domain.join domains in
      let successful =
        List.filter_map
          (function Ok reservation -> Some reservation | Error _ -> None)
          outcomes
      in
      Fun.protect
        ~finally:(fun () ->
          List.iter
            (fun reservation ->
              ignore (Engine.Run_store.abandon_reservation store reservation))
            successful)
        (fun () ->
          let reservations = List.map get_ok outcomes in
          let ids =
            List.map
              (fun reservation ->
                Engine.Run_store.reservation_id reservation
                |> Core.Run_id.to_string)
              reservations
          in
          Alcotest.(check int)
            "every concurrent reservation has one ID"
            concurrent_reservation_count
            (List.sort_uniq String.compare ids |> List.length);
          let expected_prefix =
            Printf.sprintf "%s-p%d-s" allocator_started_at (Unix.getpid ())
          in
          Alcotest.(check bool)
            "every ID carries the allocating process" true
            (List.for_all
               (Engine.Util.string_starts_with ~prefix:expected_prefix)
               ids)))

let test_sequence_exhaustion_fails_closed () =
  with_fault_layout
    ~next_reservation_sequence:(fun () ->
      Error "run reservation sequence is exhausted")
    (fun _ -> None)
    (fun ~parent:_ ~cache ~workspace:_ ~store ->
      check_error_cause "invariant-violation"
        (Engine.Run_store.reserve store ~started_at:allocator_started_at);
      check_marker_count cache 0;
      ignore (get_ok (Engine.Run_store.gc store ~older_than_days:0)))

let test_save_consumes_exact_marker () =
  with_layout (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let reservation = reserve store in
      let id = Engine.Run_store.reservation_id reservation in
      check_marker_count cache 1;
      let staged = get_ok (Engine.Run_store.stage_run store reservation) in
      let finalization = finalize_clean staged in
      check_marker_count cache 0;
      let published =
        get_ok
          (Engine.Run_store.publish_run finalization.publication (report id))
      in
      Alcotest.(check string)
        "published path is authoritative"
        (Core.Run_id.to_string id ^ ".json")
        (Filename.basename published.path);
      let loaded = get_ok (Engine.Run_store.load_run store "latest") in
      Alcotest.(check string)
        "latest report ID" (Core.Run_id.to_string id)
        (Core.Run_id.to_string loaded.metadata.id);
      check_error_cause "invariant-violation"
        (Engine.Run_store.publish_run finalization.publication (report id));
      get_ok (Engine.Run_store.abandon_reservation store reservation))

let test_publication_root_swap_contract () =
  let publish_boundary = ref (fun () -> ()) in
  let attempted = ref false in
  let swapped = ref false in
  let rename_error = ref None in
  with_fault_layout
    (fun point ->
      match point with
      | Engine.Run_store.Pending_report_write when not !attempted ->
          attempted := true;
          !publish_boundary ();
          None
      | _ -> None)
    (fun ~parent ~cache ~workspace:_ ~store ->
      let reservation = reserve store in
      protect_reservation store reservation (fun () ->
          let id = Engine.Run_store.reservation_id reservation in
          let final_path, pending_path = report_paths (marker_path cache) in
          let scope = Filename.dirname (Filename.dirname final_path) in
          let latest_path = Filename.concat scope "latest" in
          let moved_cache = Filename.concat parent "captured-cache" in
          (publish_boundary :=
             fun () ->
               try
                 Sys.rename cache moved_cache;
                 swapped := true;
                 create_directories (Filename.dirname pending_path)
               with Sys_error message when Sys.win32 ->
                 rename_error := Some message);
          let staged = get_ok (Engine.Run_store.stage_run store reservation) in
          let finalization = finalize_clean staged in
          let expected = report id in
          let published =
            get_ok
              (Engine.Run_store.publish_run finalization.publication expected)
          in
          Alcotest.(check bool)
            "root rename was attempted at publish boundary" true !attempted;
          Alcotest.(check int)
            "captured-root publication has no advisory" 0
            (List.length published.advisories);
          if Sys.win32 then (
            Alcotest.(check bool)
              "live lock pins the Windows root namespace" false !swapped;
            Alcotest.(check bool)
              "Windows root rename returned an OS error" true
              (Option.is_some !rename_error);
            Alcotest.(check bool)
              "pending was atomically consumed" false
              (Sys.file_exists pending_path);
            Alcotest.(check string)
              "pinned root contains the authoritative report"
              (Engine.Run_store.run_to_string expected)
              (read_file_or_fail final_path);
            Alcotest.(check string)
              "pinned root contains the latest index" (Core.Run_id.to_string id)
              (read_file_or_fail latest_path))
          else (
            Alcotest.(check bool)
              "POSIX root was swapped at publish boundary" true !swapped;
            List.iter
              (fun (label, path) ->
                Alcotest.(check bool) label false (Sys.file_exists path))
              [
                ("replacement root has no pending report", pending_path);
                ("replacement root has no final report", final_path);
                ("replacement root has no latest index", latest_path);
              ];
            let moved_pending =
              rebase_beneath ~from:cache ~onto:moved_cache pending_path
            in
            let moved_final =
              rebase_beneath ~from:cache ~onto:moved_cache final_path
            in
            let moved_latest =
              rebase_beneath ~from:cache ~onto:moved_cache latest_path
            in
            Alcotest.(check bool)
              "captured root pending was atomically consumed" false
              (Sys.file_exists moved_pending);
            Alcotest.(check string)
              "captured root contains the authoritative report"
              (Engine.Run_store.run_to_string expected)
              (read_file_or_fail moved_final);
            Alcotest.(check string)
              "captured root contains the latest index"
              (Core.Run_id.to_string id)
              (read_file_or_fail moved_latest))))

let test_join_root_identity_contract () =
  with_layout (fun ~parent ~cache ~workspace ~store:first_store ->
      let first = reserve first_store in
      let moved_cache = Filename.concat parent "leased-cache" in
      let swapped = ref false in
      Fun.protect
        ~finally:(fun () ->
          if !swapped then (
            ignore (Engine.Util.remove_tree cache);
            Sys.rename moved_cache cache);
          ignore (Engine.Run_store.abandon_reservation first_store first))
        (fun () ->
          let rename_result =
            try
              Sys.rename cache moved_cache;
              swapped := true;
              Ok ()
            with Sys_error message -> Error message
          in
          if Sys.win32 then (
            (match rename_result with
            | Error _ -> ()
            | Ok () ->
                Alcotest.fail
                  "the live Windows root was replaceable despite its lock");
            let same_root_store =
              get_ok (Engine.Run_store.create ~workspace ~directory:cache ())
            in
            let same_root = reserve same_root_store in
            check_marker_count cache 2;
            get_ok
              (Engine.Run_store.abandon_reservation same_root_store same_root);
            check_marker_count cache 1)
          else (
            (match rename_result with
            | Ok () -> ()
            | Error message ->
                Alcotest.failf "POSIX root rename was refused: %s" message);
            create_directory cache;
            (* The identity mismatch against the live root lease fails closed at
               the earliest boundary that observes it: store creation when the
               replacement root is adopted, otherwise reservation. *)
            (match Engine.Run_store.create ~workspace ~directory:cache () with
            | Error error ->
                Alcotest.(check string)
                  "fresh join fails on live root identity mismatch"
                  "workspace-violation"
                  (Engine.Error.cause_name (Engine.Error.cause error))
            | Ok replacement_store -> (
                match
                  Engine.Run_store.reserve replacement_store
                    ~started_at:allocator_started_at
                with
                | Ok replacement ->
                    ignore
                      (Engine.Run_store.abandon_reservation replacement_store
                         replacement);
                    Alcotest.fail
                      "a valid marker on a different root identity was accepted"
                | Error error ->
                    Alcotest.(check string)
                      "fresh join fails on live root identity mismatch"
                      "workspace-violation"
                      (Engine.Error.cause_name (Engine.Error.cause error))));
            check_marker_count cache 0)))

let test_native_acquisition_cleanup_retry_is_consumed () =
  with_layout (fun ~parent ~cache ~workspace:_ ~store ->
      Fun.protect ~finally:reset_native_internal_fault (fun () ->
          inject_native_internal_fault native_lock_acquire_fault_site true true;
          let failure =
            match
              Engine.Run_store.reserve store ~started_at:allocator_started_at
            with
            | Error error -> error
            | Ok reservation ->
                ignore (Engine.Run_store.abandon_reservation store reservation);
                Alcotest.fail
                  "injected native lock acquisition unexpectedly succeeded"
          in
          Alcotest.(check (list string))
            "action stays primary before internal cleanup failure"
            [
              "injected-lock-acquire-failure"; "injected-internal-close-failure";
            ]
            (native_error_codes failure);
          reset_native_internal_fault ();
          let moved_cache = Filename.concat parent "retry-closed-cache" in
          (try
             Sys.rename cache moved_cache;
             Sys.rename moved_cache cache
           with Sys_error message ->
             Alcotest.failf
               "retry left the native no-delete-share handle live: %s" message);
          let reservation = reserve store in
          get_ok (Engine.Run_store.abandon_reservation store reservation)))

let test_bootstrap_postcommit_failure_recovers_only_empty_root () =
  let parent = Filename.temp_file "ocaml-mutants-bootstrap-fault-" ".tmp" in
  Sys.remove parent;
  create_directory parent;
  let parent = Unix.realpath parent in
  Fun.protect
    ~finally:(fun () ->
      reset_native_internal_fault ();
      ignore (Engine.Util.remove_tree parent))
    (fun () ->
      let cache = Filename.concat parent "cache" in
      let workspace = Filename.concat parent "workspace" in
      create_directory workspace;
      inject_native_internal_fault create_directory_after_commit_fault_site true
        false;
      (match Engine.Run_store.create ~workspace ~directory:cache () with
      | Ok _ -> Alcotest.fail "post-commit root creation fault was hidden"
      | Error error ->
          Alcotest.(check bool)
            "post-commit root evidence is lossless" true
            (List.mem "injected-create-after-commit" (native_error_codes error)));
      Alcotest.(check bool)
        "post-commit root residual exists" true (Sys.is_directory cache);
      reset_native_internal_fault ();
      let store =
        get_ok (Engine.Run_store.create ~workspace ~directory:cache ())
      in
      Alcotest.(check string)
        "empty-root recovery installs exact ownership marker"
        "owner=ocaml-mutants\nschema=2\n"
        (read_file_or_fail (Filename.concat cache ".ocaml-mutants-cache-v2"));
      let reservation = reserve store in
      get_ok (Engine.Run_store.abandon_reservation store reservation))

let test_reservation_marker_postcommit_failure_is_audit_only () =
  with_layout (fun ~parent:_ ~cache ~workspace:_ ~store ->
      Fun.protect ~finally:reset_native_internal_fault (fun () ->
          inject_native_internal_fault create_file_after_commit_fault_site true
            false;
          (match
             Engine.Run_store.reserve store ~started_at:allocator_started_at
           with
          | Ok reservation ->
              ignore (Engine.Run_store.abandon_reservation store reservation);
              Alcotest.fail "post-commit marker creation fault was hidden"
          | Error error ->
              Alcotest.(check bool)
                "post-commit marker evidence is lossless" true
                (List.mem "injected-create-file-after-commit"
                   (native_error_codes error)));
          let audit_only = reservation_markers cache in
          Alcotest.(check int)
            "uncaptured marker residual remains audit-only" 1
            (List.length audit_only);
          reset_native_internal_fault ();
          let reservation = reserve store in
          Alcotest.(check int)
            "allocator skips the audit-only collision" 2
            (List.length (reservation_markers cache));
          get_ok (Engine.Run_store.abandon_reservation store reservation);
          Alcotest.(check (list string))
            "exact abandon preserves unrelated audit residual" audit_only
            (reservation_markers cache)))

let test_exact_marker_delete_retry_precedes_lease_release () =
  with_layout (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let reservation = reserve store in
      let marker = marker_path cache in
      Fun.protect ~finally:reset_native_internal_fault (fun () ->
          inject_native_internal_fault delete_before_commit_fault_site true
            false;
          (match Engine.Run_store.abandon_reservation store reservation with
          | Ok () -> Alcotest.fail "pre-commit marker delete fault was hidden"
          | Error error ->
              Alcotest.(check bool)
                "first exact delete failure remains primary" true
                (List.mem "injected-delete-before-commit"
                   (native_error_codes error)));
          Alcotest.(check bool)
            "lease teardown retries and deletes the same captured marker" false
            (Sys.file_exists marker);
          reset_native_internal_fault ();
          let next = reserve store in
          get_ok (Engine.Run_store.abandon_reservation store next)))

let test_unique_reservations_and_report_id_match () =
  with_layout (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let first = reserve store in
      let second = reserve store in
      let first_id = Engine.Run_store.reservation_id first in
      let second_id = Engine.Run_store.reservation_id second in
      Alcotest.(check bool)
        "reservations are unique" true
        (Core.Run_id.compare first_id second_id <> 0);
      check_marker_count cache 2;
      let staged = get_ok (Engine.Run_store.stage_run store first) in
      let finalization = finalize_clean staged in
      check_error_cause "invariant-violation"
        (Engine.Run_store.publish_run finalization.publication
           (report second_id));
      check_marker_count cache 1;
      get_ok (Engine.Run_store.abandon_reservation store first);
      get_ok (Engine.Run_store.abandon_reservation store second);
      check_marker_count cache 0)

let test_store_namespace_owns_reservation () =
  with_layout (fun ~parent ~cache ~workspace:_ ~store:first_store ->
      let second_workspace = Filename.concat parent "second-workspace" in
      create_directory second_workspace;
      let second_store =
        get_ok
          (Engine.Run_store.create ~workspace:second_workspace ~directory:cache
             ())
      in
      let reservation = reserve first_store in
      let second_reservation = reserve second_store in
      check_marker_count cache 2;
      check_error_cause "invariant-violation"
        (Engine.Run_store.stage_run second_store reservation);
      check_error_cause "invariant-violation"
        (Engine.Run_store.abandon_reservation second_store reservation);
      check_marker_count cache 2;
      get_ok (Engine.Run_store.abandon_reservation first_store reservation);
      get_ok
        (Engine.Run_store.abandon_reservation second_store second_reservation);
      check_marker_count cache 0)

let test_stage_is_io_free_and_one_shot () =
  with_layout (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let reservation = reserve store in
      let marker = marker_path cache in
      Fun.protect
        ~finally:(fun () ->
          ignore (Engine.Run_store.abandon_reservation store reservation);
          raw_remove_noerr marker)
        (fun () ->
          (* Windows delete/write sharing excludes the ambient writer; POSIX has
             no mandatory exclusion, so the in-place rewrite succeeds and the
             capability below still finalizes through the pinned inode. *)
          (match Engine.Util.write_file marker "foreign-owner\n" with
          | Error _ when Sys.win32 -> ()
          | Error message ->
              Alcotest.failf "live marker replacement failed unexpectedly: %s"
                message
          | Ok () when Sys.win32 ->
              Alcotest.fail
                "live reservation marker did not exclude an ambient writer"
          | Ok () -> ());
          let staged = get_ok (Engine.Run_store.stage_run store reservation) in
          check_error_cause "invariant-violation"
            (Engine.Run_store.stage_run store reservation);
          let finalization = finalize_clean staged in
          check_error_cause "invariant-violation"
            (Engine.Run_store.finalize_run staged);
          check_marker_count cache 0;
          ignore
            (get_ok
               (Engine.Run_store.publish_run finalization.publication
                  (report (Engine.Run_store.reservation_id reservation))))))

let test_stage_defers_publication_conflicts () =
  List.iter
    (fun (conflict, timing) ->
      with_layout (fun ~parent:_ ~cache ~workspace:_ ~store ->
          let reservation = reserve store in
          let id = Engine.Run_store.reservation_id reservation in
          let final_path, pending_path = report_paths (marker_path cache) in
          let conflict_path =
            match conflict with
            | `Pending -> pending_path
            | `Final -> final_path
          in
          let foreign = "foreign-publication\n" in
          let create_conflict () =
            match Engine.Util.write_file conflict_path foreign with
            | Ok () -> ()
            | Error message -> Alcotest.fail message
          in
          if timing = `Before_stage then create_conflict ();
          let staged = get_ok (Engine.Run_store.stage_run store reservation) in
          if timing = `After_stage then create_conflict ();
          let finalization = finalize_clean staged in
          check_error_cause "io-failure"
            (Engine.Run_store.publish_run finalization.publication (report id));
          Alcotest.(check string)
            "publish never replaces a conflicting artifact" foreign
            (match Engine.Util.read_file conflict_path with
            | Ok contents -> contents
            | Error message -> Alcotest.fail message)))
    [
      (`Pending, `Before_stage);
      (`Final, `Before_stage);
      (`Pending, `After_stage);
      (`Final, `After_stage);
    ]

let test_abandon_consumes_marker () =
  with_layout (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let reservation = reserve store in
      check_marker_count cache 1;
      get_ok (Engine.Run_store.abandon_reservation store reservation);
      check_marker_count cache 0;
      get_ok (Engine.Run_store.abandon_reservation store reservation);
      check_error_cause "invariant-violation"
        (Engine.Run_store.stage_run store reservation))

let fault_name = function
  | Engine.Run_store.Pending_report_write -> "pending-report-write"
  | Engine.Run_store.Report_publish -> "report-publish"
  | Engine.Run_store.Latest_index_update -> "latest-index-update"
  | Engine.Run_store.Reservation_marker_remove -> "reservation-marker-remove"
  | Engine.Run_store.Lease_unlock -> "lease-unlock"
  | Engine.Run_store.Lease_close -> "lease-close"
  | Engine.Run_store.Lease_root_close -> "lease-root-close"
  | Engine.Run_store.Publication_lease_unlock -> "publication-lease-unlock"
  | Engine.Run_store.Publication_lease_close -> "publication-lease-close"
  | Engine.Run_store.Publication_lease_root_close ->
      "publication-lease-root-close"

let injected_fault failures events point =
  events := !events @ [ point ];
  if List.mem point failures then Some ("injected " ^ fault_name point)
  else None

let run_for_resolution id resolution =
  let status =
    match Engine.Application.report_status resolution with
    | Engine.Application.Report_completed -> Engine.Run_store.Completed
    | Engine.Application.Report_interrupted -> Engine.Run_store.Interrupted
    | Engine.Application.Report_failed error -> Engine.Run_store.Failed error
  in
  { (report id) with status }

let check_published_round_trip expected actual =
  Alcotest.(check string)
    "published report is lossless"
    (Engine.Run_store.run_to_string expected)
    (Engine.Run_store.run_to_string actual)

let test_cleanup_faults_publish_failed_report () =
  let selected = Engine.Run_store.Reservation_marker_remove in
  let events = ref [] in
  with_fault_layout (injected_fault [ selected ] events)
    (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let reservation = reserve store in
      protect_reservation store reservation (fun () ->
          let id = Engine.Run_store.reservation_id reservation in
          let marker = marker_path cache in
          let staged = get_ok (Engine.Run_store.stage_run store reservation) in
          let finalization = get_ok (Engine.Run_store.finalize_run staged) in
          Alcotest.(check int)
            (fault_name selected ^ " cleanup error count")
            1
            (List.length finalization.cleanup_errors);
          let resolution =
            Engine.Application.resolve
              (Engine.Application.Completed Engine.Application.All_detected)
              ~cleanup_errors:finalization.cleanup_errors
          in
          let final_report = run_for_resolution id resolution in
          let published =
            get_ok
              (Engine.Run_store.publish_run finalization.publication
                 final_report)
          in
          Alcotest.(check int)
            "cleanup fault has no latest advisory" 0
            (List.length published.advisories);
          let loaded =
            get_ok (Engine.Run_store.load_run store (Core.Run_id.to_string id))
          in
          check_published_round_trip final_report loaded;
          (match loaded.status with
          | Engine.Run_store.Failed _ -> ()
          | Engine.Run_store.Completed | Engine.Run_store.Interrupted ->
              Alcotest.failf "%s cleanup was published as non-failure"
                (fault_name selected));
          (match Engine.Application.result resolution with
          | Error _ -> ()
          | Ok code ->
              Alcotest.failf "%s cleanup returned exit %d" (fault_name selected)
                code);
          if selected = Engine.Run_store.Reservation_marker_remove then
            Alcotest.(check bool)
              "fault is injected only after exact marker deletion" false
              (Sys.file_exists marker)
          else assert false))

let test_abandon_lease_cleanup_faults_are_lossless () =
  List.iter
    (fun selected ->
      let events = ref [] in
      with_fault_layout (injected_fault [ selected ] events)
        (fun ~parent:_ ~cache:_ ~workspace:_ ~store ->
          let reservation = reserve store in
          (match Engine.Run_store.abandon_reservation store reservation with
          | Ok () ->
              Alcotest.failf "%s abandon unexpectedly succeeded"
                (fault_name selected)
          | Error error ->
              Alcotest.(check string)
                (fault_name selected ^ " phase")
                "cleanup"
                (Engine.Error.phase_name (Engine.Error.phase error)));
          Alcotest.(check (list string))
            "abandon attempts marker, lock release, and root close in order"
            (List.map fault_name
               [
                 Engine.Run_store.Reservation_marker_remove;
                 Engine.Run_store.Lease_unlock;
                 Engine.Run_store.Lease_close;
                 Engine.Run_store.Lease_root_close;
               ])
            (List.map fault_name !events)))
    [
      Engine.Run_store.Lease_unlock;
      Engine.Run_store.Lease_close;
      Engine.Run_store.Lease_root_close;
    ]

let test_multiple_cleanup_fault_order_is_lossless () =
  let failures =
    [
      Engine.Run_store.Reservation_marker_remove;
      Engine.Run_store.Lease_unlock;
      Engine.Run_store.Lease_close;
      Engine.Run_store.Lease_root_close;
    ]
  in
  let events = ref [] in
  with_fault_layout (injected_fault failures events)
    (fun ~parent:_ ~cache:_ ~workspace:_ ~store ->
      let reservation = reserve store in
      let result = Engine.Run_store.abandon_reservation store reservation in
      Alcotest.(check (list string))
        "marker, unlock, and close are all attempted in order"
        (List.map fault_name failures)
        (List.map fault_name !events);
      match result with
      | Error primary ->
          Alcotest.(check int)
            "lock and root cleanup remain suppressed in order" 3
            (List.length (Engine.Error.suppressed primary))
      | Ok () -> Alcotest.fail "multiple cleanup failures were discarded")

let test_pending_write_failure_is_nonauthoritative () =
  let events = ref [] in
  with_fault_layout
    (injected_fault [ Engine.Run_store.Pending_report_write ] events)
    (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let reservation = reserve store in
      let id = Engine.Run_store.reservation_id reservation in
      let marker = marker_path cache in
      let final_path, pending_path = report_paths marker in
      let staged = get_ok (Engine.Run_store.stage_run store reservation) in
      let finalization = finalize_clean staged in
      check_error_cause "io-failure"
        (Engine.Run_store.publish_run finalization.publication (report id));
      Alcotest.(check bool) "no final report" false (Sys.file_exists final_path);
      Alcotest.(check bool)
        "no pending report after refused write" false
        (Sys.file_exists pending_path);
      check_error_cause "io-failure"
        (Engine.Run_store.load_run store (Core.Run_id.to_string id));
      check_error_cause "invariant-violation"
        (Engine.Run_store.publish_run finalization.publication (report id)))

let test_publish_failure_leaves_only_collectable_pending () =
  let events = ref [] in
  with_fault_layout (injected_fault [ Engine.Run_store.Report_publish ] events)
    (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let reservation = reserve store in
      let id = Engine.Run_store.reservation_id reservation in
      let marker = marker_path cache in
      let final_path, pending_path = report_paths marker in
      let staged = get_ok (Engine.Run_store.stage_run store reservation) in
      let finalization = finalize_clean staged in
      check_error_cause "io-failure"
        (Engine.Run_store.publish_run finalization.publication (report id));
      Alcotest.(check bool)
        "atomic final was not published" false
        (Sys.file_exists final_path);
      Alcotest.(check bool)
        "pending artifact retained for recovery" true
        (Sys.file_exists pending_path);
      check_error_cause "io-failure"
        (Engine.Run_store.load_run store (Core.Run_id.to_string id));
      check_error_cause "invalid-input"
        (Engine.Run_store.load_run store "latest");
      Unix.utimes pending_path 1. 1.;
      ignore (get_ok (Engine.Run_store.gc store ~older_than_days:1));
      Alcotest.(check bool)
        "gc collects stale pending artifact" false
        (Sys.file_exists pending_path))

let test_latest_failure_is_nonfatal_advisory () =
  let events = ref [] in
  with_fault_layout
    (injected_fault [ Engine.Run_store.Latest_index_update ] events)
    (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let reservation = reserve store in
      let id = Engine.Run_store.reservation_id reservation in
      let marker = marker_path cache in
      let final_path, pending_path = report_paths marker in
      let staged = get_ok (Engine.Run_store.stage_run store reservation) in
      let finalization = finalize_clean staged in
      let resolution =
        Engine.Application.resolve
          (Engine.Application.Completed Engine.Application.All_detected)
          ~cleanup_errors:[]
      in
      let final_report = run_for_resolution id resolution in
      let published =
        get_ok
          (Engine.Run_store.publish_run finalization.publication final_report)
      in
      Alcotest.(check int)
        "latest failure becomes one advisory" 1
        (List.length published.advisories);
      Alcotest.(check bool)
        "authoritative final exists" true
        (Sys.file_exists final_path);
      Alcotest.(check bool)
        "pending is consumed by publish" false
        (Sys.file_exists pending_path);
      let loaded =
        get_ok (Engine.Run_store.load_run store (Core.Run_id.to_string id))
      in
      check_published_round_trip final_report loaded;
      check_error_cause "invalid-input"
        (Engine.Run_store.load_run store "latest");
      match Engine.Application.result resolution with
      | Ok 0 -> ()
      | Ok code -> Alcotest.failf "latest advisory changed exit to %d" code
      | Error error ->
          Alcotest.failf "latest advisory became fatal: %a" Engine.Error.pp
            error)

let test_publication_lease_teardown_is_advisory () =
  List.iter
    (fun selected ->
      let events = ref [] in
      with_fault_layout (injected_fault [ selected ] events)
        (fun ~parent:_ ~cache:_ ~workspace:_ ~store ->
          let reservation = reserve store in
          let id = Engine.Run_store.reservation_id reservation in
          let staged = get_ok (Engine.Run_store.stage_run store reservation) in
          let finalization = finalize_clean staged in
          let resolution =
            Engine.Application.resolve
              (Engine.Application.Completed Engine.Application.All_detected)
              ~cleanup_errors:[]
          in
          let final_report = run_for_resolution id resolution in
          let published =
            get_ok
              (Engine.Run_store.publish_run finalization.publication
                 final_report)
          in
          Alcotest.(check int)
            (fault_name selected ^ " advisory count")
            1
            (List.length published.advisories);
          let loaded =
            get_ok (Engine.Run_store.load_run store (Core.Run_id.to_string id))
          in
          check_published_round_trip final_report loaded;
          match Engine.Application.result resolution with
          | Ok 0 -> ()
          | Ok code ->
              Alcotest.failf "%s changed exit to %d" (fault_name selected) code
          | Error error ->
              Alcotest.failf "%s became fatal: %a" (fault_name selected)
                Engine.Error.pp error))
    [
      Engine.Run_store.Publication_lease_unlock;
      Engine.Run_store.Publication_lease_close;
      Engine.Run_store.Publication_lease_root_close;
    ]

let test_post_publish_output_failures_are_advisory () =
  let attempts = ref [] in
  let advisories =
    Engine.Runner.For_testing.emit_after_publish
      ~write:(fun _ ->
        attempts := !attempts @ [ "write" ];
        failwith "injected output write failure")
      ~flush:(fun () ->
        attempts := !attempts @ [ "flush" ];
        failwith "injected output flush failure")
      "published output"
  in
  Alcotest.(check (list string))
    "write and flush are both attempted" [ "write"; "flush" ] !attempts;
  Alcotest.(check int)
    "both output failures become advisories" 2 (List.length advisories);
  Alcotest.(check (list string))
    "output advisory order" [ "write"; "flush" ]
    (List.map
       (fun advisory ->
         Option.value ~default:"missing"
           (List.assoc_opt "operation" (Engine.Error.context advisory)))
       advisories)

let () =
  Alcotest.run "Run reservation contract"
    [
      ( "reservation",
        [
          Alcotest.test_case "structural ID and exact marker" `Quick
            test_structural_id_and_exact_marker;
          Alcotest.test_case "exclusive-create collision chain" `Quick
            test_collisions_continue_until_first_free_name;
          Alcotest.test_case "concurrent uniqueness" `Quick
            test_concurrent_reservations_are_unique;
          Alcotest.test_case "sequence exhaustion fails closed" `Quick
            test_sequence_exhaustion_fails_closed;
          Alcotest.test_case "publish consumes exact marker" `Quick
            test_save_consumes_exact_marker;
          Alcotest.test_case "publication root swap contract" `Quick
            test_publication_root_swap_contract;
          Alcotest.test_case "join root identity contract" `Quick
            test_join_root_identity_contract;
          Alcotest.test_case "native acquisition cleanup retry" `Quick
            test_native_acquisition_cleanup_retry_is_consumed;
          Alcotest.test_case "bootstrap postcommit empty-root recovery" `Quick
            test_bootstrap_postcommit_failure_recovers_only_empty_root;
          Alcotest.test_case "marker postcommit residual is audit-only" `Quick
            test_reservation_marker_postcommit_failure_is_audit_only;
          Alcotest.test_case "exact marker delete retries before lease release"
            `Quick test_exact_marker_delete_retry_precedes_lease_release;
          Alcotest.test_case "unique IDs and exact report ID" `Quick
            test_unique_reservations_and_report_id_match;
          Alcotest.test_case "store namespace ownership" `Quick
            test_store_namespace_owns_reservation;
          Alcotest.test_case "stage is I/O-free and one-shot" `Quick
            test_stage_is_io_free_and_one_shot;
          Alcotest.test_case "publish owns conflict detection" `Quick
            test_stage_defers_publication_conflicts;
          Alcotest.test_case "explicit abandon" `Quick
            test_abandon_consumes_marker;
          Alcotest.test_case "cleanup faults publish failed report" `Quick
            test_cleanup_faults_publish_failed_report;
          Alcotest.test_case "abandon lease cleanup faults" `Quick
            test_abandon_lease_cleanup_faults_are_lossless;
          Alcotest.test_case "multiple cleanup fault order" `Quick
            test_multiple_cleanup_fault_order_is_lossless;
          Alcotest.test_case "pending write is nonauthoritative" `Quick
            test_pending_write_failure_is_nonauthoritative;
          Alcotest.test_case "publish failure leaves pending" `Quick
            test_publish_failure_leaves_only_collectable_pending;
          Alcotest.test_case "latest failure is advisory" `Quick
            test_latest_failure_is_nonfatal_advisory;
          Alcotest.test_case "publication lease teardown is advisory" `Quick
            test_publication_lease_teardown_is_advisory;
          Alcotest.test_case "post-publish output is advisory" `Quick
            test_post_publish_output_failures_are_advisory;
        ] );
    ]
