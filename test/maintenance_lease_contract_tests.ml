module Core = Ocaml_mutants_core
module Engine = Ocaml_mutants_engine

let child_mode = "OCAML_MUTANTS_MAINTENANCE_CHILD"
let child_workspace = "OCAML_MUTANTS_MAINTENANCE_WORKSPACE"
let child_cache = "OCAML_MUTANTS_MAINTENANCE_CACHE"
let child_ready = "OCAML_MUTANTS_MAINTENANCE_READY"
let child_release = "OCAML_MUTANTS_MAINTENANCE_RELEASE"

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
        toolchain = "maintenance-contract";
        profile = Core.Operator.Profile.Balanced;
        selection = "all";
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

let with_layout action =
  let parent = Filename.temp_file "ocaml-mutants-maintenance-" ".tmp" in
  Sys.remove parent;
  create_directory parent;
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

let raw_remove_noerr path = ignore (Engine.Util.remove_tree path)

let protect_reservation store reservation action =
  Fun.protect
    ~finally:(fun () ->
      ignore (Engine.Run_store.abandon_reservation store reservation))
    action

let reserve store =
  get_ok (Engine.Run_store.reserve store ~started_at:"20260101T000000Z")

let publish_clean store reservation run =
  let staged = get_ok (Engine.Run_store.stage_run store reservation) in
  let finalization = get_ok (Engine.Run_store.finalize_run staged) in
  (match finalization.cleanup_errors with
  | [] -> ()
  | error :: _ ->
      Alcotest.failf "unexpected finalization error: %a" Engine.Error.pp error);
  get_ok (Engine.Run_store.publish_run finalization.publication run)

let reservation_markers cache =
  Engine.Util.files_recursive cache
  |> List.filter (fun path -> Filename.check_suffix path ".reserved")
  |> List.sort String.compare

let expect_maintenance_blocked label = function
  | Ok _ -> Alcotest.failf "%s unexpectedly acquired maintenance lease" label
  | Error error ->
      Alcotest.(check string)
        (label ^ " phase") "cache"
        (Engine.Error.phase_name (Engine.Error.phase error));
      Alcotest.(check string)
        (label ^ " cause") "resource-busy"
        (Engine.Error.cause_name (Engine.Error.cause error))

let test_resource_busy_report_codec_round_trip () =
  let id =
    match
      Core.Run_id.create ~started_at:"20260101T000000Z"
        ~nonce:"resource-busy-codec"
    with
    | Ok id -> id
    | Error message -> Alcotest.fail message
  in
  let primary =
    Engine.Error.create ~phase:Engine.Error.Cache
      ~cause:Engine.Error.Resource_busy
      ~context:[ ("cache_root", "contract-cache") ]
      "maintenance lease is held"
  in
  let cleanup =
    Engine.Error.create ~phase:Engine.Error.Cleanup
      ~cause:Engine.Error.Io_failure "cleanup failed"
  in
  let original : Engine.Run_store.run =
    {
      (report id) with
      status = Engine.Run_store.Failed (Engine.Error.suppress primary cleanup);
    }
  in
  match
    Engine.Run_store.run_of_json (Engine.Run_store.run_to_yojson original)
  with
  | Error message -> Alcotest.fail message
  | Ok { status = Engine.Run_store.Failed decoded; _ } -> (
      Alcotest.(check string)
        "dedicated cause" "resource-busy"
        (Engine.Error.cause_name (Engine.Error.cause decoded));
      Alcotest.(check (list (pair string string)))
        "context survives"
        [ ("cache_root", "contract-cache") ]
        (Engine.Error.context decoded);
      match Engine.Error.suppressed decoded with
      | [ suppressed ] ->
          Alcotest.(check string)
            "suppressed cause survives" "io-failure"
            (Engine.Error.cause_name (Engine.Error.cause suppressed))
      | suppressed ->
          Alcotest.failf "expected one suppressed error, found %d"
            (List.length suppressed))
  | Ok _ ->
      Alcotest.fail "resource-busy failure decoded as a non-failure status"

let test_active_reservation_blocks_maintenance_then_saves () =
  with_layout (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let reservation = reserve store in
      protect_reservation store reservation (fun () ->
          let id = Engine.Run_store.reservation_id reservation in
          expect_maintenance_blocked "gc"
            (Engine.Run_store.gc store ~older_than_days:0);
          expect_maintenance_blocked "clean" (Engine.Run_store.clean store);
          Alcotest.(check int)
            "maintenance preserves live marker" 1
            (List.length (reservation_markers cache));
          let staged = get_ok (Engine.Run_store.stage_run store reservation) in
          let finalization = get_ok (Engine.Run_store.finalize_run staged) in
          (match finalization.cleanup_errors with
          | [] -> ()
          | error :: _ ->
              Alcotest.failf "unexpected finalization error: %a" Engine.Error.pp
                error);
          expect_maintenance_blocked "gc after finalize"
            (Engine.Run_store.gc store ~older_than_days:0);
          expect_maintenance_blocked "clean after finalize"
            (Engine.Run_store.clean store);
          ignore
            (get_ok
               (Engine.Run_store.publish_run finalization.publication
                  (report id)));
          Alcotest.(check int)
            "save consumes live marker" 0
            (List.length (reservation_markers cache));
          ignore (get_ok (Engine.Run_store.gc store ~older_than_days:0));
          get_ok (Engine.Run_store.clean store)))

let test_staged_conflict_keeps_lease_until_abandon () =
  with_layout (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let reservation = reserve store in
      let foreign_path = ref None in
      Fun.protect
        ~finally:(fun () -> Option.iter raw_remove_noerr !foreign_path)
        (fun () ->
          protect_reservation store reservation (fun () ->
              let marker =
                match reservation_markers cache with
                | [ marker ] -> Filename.concat cache marker
                | markers ->
                    Alcotest.failf "expected one marker, found %d"
                      (List.length markers)
              in
              let final_path =
                String.sub marker 0
                  (String.length marker - String.length ".reserved")
                ^ ".json"
              in
              foreign_path := Some final_path;
              (match Engine.Util.write_file final_path "foreign-report\n" with
              | Ok () -> ()
              | Error message -> Alcotest.fail message);
              ignore (get_ok (Engine.Run_store.stage_run store reservation));
              Alcotest.(check int)
                "I/O-free stage preserves marker" 1
                (List.length (reservation_markers cache));
              expect_maintenance_blocked "gc after staged conflict"
                (Engine.Run_store.gc store ~older_than_days:0);
              expect_maintenance_blocked "clean after staged conflict"
                (Engine.Run_store.clean store);
              get_ok (Engine.Run_store.abandon_reservation store reservation);
              ignore (get_ok (Engine.Run_store.gc store ~older_than_days:0));
              get_ok (Engine.Run_store.clean store))))

let test_multiple_reservations_hold_one_root_lease () =
  with_layout (fun ~parent ~cache ~workspace:_ ~store:first_store ->
      let second_workspace = Filename.concat parent "second-workspace" in
      create_directory second_workspace;
      let second_store =
        get_ok
          (Engine.Run_store.create ~workspace:second_workspace ~directory:cache
             ())
      in
      let first = reserve first_store in
      protect_reservation first_store first (fun () ->
          let second = reserve second_store in
          protect_reservation second_store second (fun () ->
              get_ok (Engine.Run_store.abandon_reservation first_store first);
              (match Engine.Run_store.gc first_store ~older_than_days:0 with
              | Error error ->
                  Alcotest.(check (option string))
                    "one shared capability remains" (Some "1")
                    (List.assoc_opt "active_reservations"
                       (Engine.Error.context error))
              | Ok _ -> Alcotest.fail "gc ignored the second reservation");
              expect_maintenance_blocked "clean with second reservation"
                (Engine.Run_store.clean first_store);
              get_ok (Engine.Run_store.abandon_reservation second_store second);
              ignore
                (get_ok (Engine.Run_store.gc first_store ~older_than_days:0));
              get_ok (Engine.Run_store.clean second_store))))

let test_live_marker_excludes_ambient_replacement () =
  with_layout (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let reservation = reserve store in
      let marker =
        match reservation_markers cache with
        | [ marker ] -> Filename.concat cache marker
        | markers ->
            Alcotest.failf "expected one marker, found %d" (List.length markers)
      in
      Fun.protect
        ~finally:(fun () ->
          ignore (Engine.Run_store.abandon_reservation store reservation);
          raw_remove_noerr marker)
        (fun () ->
          (match Engine.Util.write_file marker "foreign-owner\n" with
          | Error _ when Sys.win32 -> ()
          | Error message -> Alcotest.fail message
          | Ok () when Sys.win32 ->
              Alcotest.fail
                "live reservation marker allowed an independent ambient writer"
          | Ok () ->
              (* POSIX cannot deny write sharing; same-effective-user
                 interference is detected at the deletion boundary rather than
                 prevented while the marker capability is live. *)
              ());
          get_ok (Engine.Run_store.abandon_reservation store reservation);
          Alcotest.(check bool)
            "exact abandon deletes the captured marker" false
            (Sys.file_exists marker);
          ignore (get_ok (Engine.Run_store.gc store ~older_than_days:0));
          get_ok (Engine.Run_store.clean store)))

let test_negative_gc_age_rejected () =
  with_layout (fun ~parent:_ ~cache:_ ~workspace:_ ~store ->
      match Engine.Run_store.gc store ~older_than_days:(-1) with
      | Ok _ -> Alcotest.fail "negative gc age was accepted"
      | Error error ->
          Alcotest.(check string)
            "negative age cause" "invalid-input"
            (Engine.Error.cause_name (Engine.Error.cause error)))

module Noop_signals = struct
  type token = unit

  let install _ _ = ()
  let restore () = ()
end

module Raising_services = struct
  module Signals = Noop_signals

  module Workspace = struct
    type snapshot = Snapshot

    type 'a bracket_outcome =
      | Acquisition_failed of Engine.Error.t
      | Action_returned of 'a * (unit, Engine.Error.t) result
      | Action_raised of
          exn * Printexc.raw_backtrace * (unit, Engine.Error.t) result

    let bracket _root action =
      try Action_returned (action Snapshot, Ok ())
      with exception_ ->
        Action_raised (exception_, Printexc.get_raw_backtrace (), Ok ())
  end

  module Clock = struct
    let now () = "20260101T000000Z"
  end

  module Store = Engine.Run_store

  type draft = unit

  let prepare_in_snapshot ~cancel:_ ~store:_ ~reservation:_ ~started_at:_
      ~root:_ ~config:_ ~fresh:_ ~selection:_ ~output:_
      ~snapshot:Workspace.Snapshot =
    raise Exit

  let prepare_failure ~cancel:_ ~store:_ ~reservation:_ ~started_at:_ ~root:_
      ~config:_ ~fresh:_ ~selection:_ ~output:_ error =
    {
      Engine.Application.draft = ();
      verdict = Engine.Application.Failed error;
      cleanup_errors = [];
    }

  let commit_reserved ~store:_ ~reservation:_ ~finished_at:_ ~resolution:_ () =
    failwith "unreachable commit after raising preparation"

  let list_mutants ~cancel:_ ~root:_ ~config:_ ~selection:_ ~output:_ = Ok 0
end

module Raising_application = Engine.Application.Make (Raising_services)

let test_application_exception_releases_reservation () =
  with_layout (fun ~parent:_ ~cache ~workspace ~store ->
      let defaults = Engine.Config.defaults in
      let config : Engine.Config.t =
        { defaults with cache = { defaults.cache with directory = Some cache } }
      in
      (match
         Raising_application.run ~root:workspace ~config ~fresh:true
           ~selection:Engine.Application_request.All
           ~output:
             (Engine.Application_request.Terminal
                { quiet = true; color = false })
       with
      | Ok code ->
          Alcotest.failf "structured preparation exception returned %d" code
      | Error error ->
          Alcotest.(check string)
            "preparation exception remains primary" (Printexc.to_string Exit)
            (List.assoc "exception" (Engine.Error.context error)));
      Alcotest.(check int)
        "exception cleanup consumes marker" 0
        (List.length (reservation_markers cache));
      ignore (get_ok (Engine.Run_store.gc store ~older_than_days:0));
      get_ok (Engine.Run_store.clean store))

let wait_for_file path =
  let deadline = Unix.gettimeofday () +. 15. in
  let rec wait () =
    if Sys.file_exists path then ()
    else if Unix.gettimeofday () >= deadline then
      failwith ("timed out waiting for " ^ path)
    else (
      Unix.sleepf 0.01;
      wait ())
  in
  wait ()

let signal_parent message =
  try
    output_string stdout (message ^ "\n");
    flush stdout;
    true
  with Sys_error _ -> false

let child_error _ready error =
  ignore (signal_parent ("error:" ^ Format.asprintf "%a" Engine.Error.pp error));
  exit 2

let maintenance_child () =
  let workspace = Sys.getenv child_workspace in
  let cache = Sys.getenv child_cache in
  let ready = Sys.getenv child_ready in
  let release = Sys.getenv child_release in
  let store =
    match Engine.Run_store.create ~workspace ~directory:cache () with
    | Ok store -> store
    | Error error -> child_error ready error
  in
  let reservation =
    match Engine.Run_store.reserve store ~started_at:"20260101T000000Z" with
    | Ok reservation -> reservation
    | Error error -> child_error ready error
  in
  if not (signal_parent "ready") then exit 3;
  (try wait_for_file release
   with Failure _ ->
     ignore (Engine.Run_store.abandon_reservation store reservation);
     exit 4);
  match Engine.Run_store.abandon_reservation store reservation with
  | Ok () -> exit 0
  | Error _ -> exit 5

let maintenance_probe_child () =
  let workspace = Sys.getenv child_workspace in
  let cache = Sys.getenv child_cache in
  let is_busy = function
    | Error error when Engine.Error.cause error = Engine.Error.Resource_busy ->
        true
    | Error _ | Ok _ -> false
  in
  let result =
    match Engine.Run_store.create ~workspace ~directory:cache () with
    | Error error -> "error:" ^ Format.asprintf "%a" Engine.Error.pp error
    | Ok store -> (
        let gc = Engine.Run_store.gc store ~older_than_days:0 in
        let clean = Engine.Run_store.clean store in
        if is_busy gc && is_busy clean then "blocked"
        else
          "error:gc="
          ^ (match gc with
            | Ok _ -> "acquired"
            | Error error -> Format.asprintf "%a" Engine.Error.pp error)
          ^ ";clean="
          ^
          match clean with
          | Ok () -> "acquired"
          | Error error -> Format.asprintf "%a" Engine.Error.pp error)
  in
  if signal_parent result then exit 0 else exit 6

let stdio_probe_child () = if signal_parent "ready" then exit 0 else exit 7

let child_environment ~mode ~workspace ~cache ~ready ~release =
  let overrides =
    [
      (child_mode, mode);
      (child_workspace, workspace);
      (child_cache, cache);
      (child_ready, ready);
      (child_release, release);
    ]
  in
  let overridden name =
    List.exists (fun (candidate, _) -> String.equal name candidate) overrides
  in
  let inherited =
    Unix.environment () |> Array.to_list
    |> List.filter (fun binding ->
        match String.index_opt binding '=' with
        | None -> true
        | Some index -> not (overridden (String.sub binding 0 index)))
  in
  Array.of_list
    (inherited @ List.map (fun (name, value) -> name ^ "=" ^ value) overrides)

let close_descriptor_noerr descriptor =
  try Unix.close descriptor with Unix.Unix_error _ -> ()

let create_child_process ~diagnostics executable environment =
  let null_device = if Sys.win32 then "NUL" else "/dev/null" in
  let child_input = Unix.openfile null_device [ Unix.O_RDONLY ] 0 in
  Fun.protect
    ~finally:(fun () -> close_descriptor_noerr child_input)
    (fun () ->
      let child_error_output =
        Unix.openfile diagnostics
          [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ]
          0o600
      in
      Fun.protect
        ~finally:(fun () -> close_descriptor_noerr child_error_output)
        (fun () ->
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

let child_diagnostics path =
  match Engine.Util.read_file path with
  | Ok "" -> ""
  | Ok contents -> "\nchild diagnostics:\n" ^ contents
  | Error message -> "\nchild diagnostics unavailable: " ^ message

let spawn_reservation_child ~parent ~workspace ~cache suffix =
  let ready = Filename.concat parent (suffix ^ "-ready") in
  let release = Filename.concat parent (suffix ^ "-release") in
  let diagnostics = Filename.concat parent (suffix ^ "-diagnostics.log") in
  let executable = Unix.realpath Sys.executable_name in
  let environment =
    child_environment ~mode:"reservation" ~workspace ~cache ~ready ~release
  in
  let process, ready_input =
    create_child_process ~diagnostics executable environment
  in
  (process, ready_input, release, diagnostics)

let spawn_maintenance_probe ~parent ~workspace ~cache suffix =
  let ready = Filename.concat parent (suffix ^ "-ready") in
  let release = Filename.concat parent (suffix ^ "-unused") in
  let diagnostics = Filename.concat parent (suffix ^ "-diagnostics.log") in
  let executable = Unix.realpath Sys.executable_name in
  let environment =
    child_environment ~mode:"maintenance-probe" ~workspace ~cache ~ready
      ~release
  in
  let process, ready_input =
    create_child_process ~diagnostics executable environment
  in
  (process, ready_input, diagnostics)

let spawn_stdio_probe ~parent ~workspace ~cache suffix =
  let ready = Filename.concat parent (suffix ^ "-ready") in
  let release = Filename.concat parent (suffix ^ "-unused") in
  let diagnostics = Filename.concat parent (suffix ^ "-diagnostics.log") in
  let executable = Unix.realpath Sys.executable_name in
  let environment =
    child_environment ~mode:"stdio-probe" ~workspace ~cache ~ready ~release
  in
  let process, ready_input =
    create_child_process ~diagnostics executable environment
  in
  (process, ready_input, diagnostics)

let child_signal ready_input =
  let channel = Unix.in_channel_of_descr ready_input in
  try
    let message = input_line channel in
    let message =
      let length = String.length message in
      if length > 0 && message.[length - 1] = '\r' then
        String.sub message 0 (length - 1)
      else message
    in
    close_in_noerr channel;
    Ok message
  with
  | End_of_file ->
      close_in_noerr channel;
      Error "child closed its readiness pipe without a signal"
  | Sys_error message ->
      close_in_noerr channel;
      Error message

let await_child_ready ready_input =
  match child_signal ready_input with
  | Ok "ready" -> ()
  | Ok message -> Alcotest.failf "reservation child: %s" message
  | Error message -> Alcotest.fail message

let test_cross_process_reservation_blocks_maintenance () =
  with_layout (fun ~parent ~cache ~workspace ~store ->
      let process, ready, release, diagnostics =
        spawn_reservation_child ~parent ~workspace ~cache "live-child"
      in
      let reaped = ref None in
      let release_and_wait () =
        match !reaped with
        | Some status -> status
        | None ->
            ignore (Engine.Util.atomic_write release "release");
            let status = snd (Unix.waitpid [] process) in
            reaped := Some status;
            status
      in
      let status =
        try
          await_child_ready ready;
          expect_maintenance_blocked "cross-process gc"
            (Engine.Run_store.gc store ~older_than_days:0);
          expect_maintenance_blocked "cross-process clean"
            (Engine.Run_store.clean store);
          release_and_wait ()
        with exception_ ->
          ignore (release_and_wait ());
          raise exception_
      in
      (match status with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED code ->
          Alcotest.failf "reservation child exited %d%s" code
            (child_diagnostics diagnostics)
      | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
          Alcotest.failf "reservation child stopped by signal %d%s" signal
            (child_diagnostics diagnostics));
      ignore (get_ok (Engine.Run_store.gc store ~older_than_days:0));
      get_ok (Engine.Run_store.clean store))

let test_publication_lease_blocks_cross_process_maintenance () =
  with_layout (fun ~parent ~cache ~workspace ~store ->
      let reservation = reserve store in
      protect_reservation store reservation (fun () ->
          let id = Engine.Run_store.reservation_id reservation in
          let staged = get_ok (Engine.Run_store.stage_run store reservation) in
          let finalization = get_ok (Engine.Run_store.finalize_run staged) in
          Alcotest.(check int)
            "reservation cleanup completed before publication" 0
            (List.length finalization.cleanup_errors);
          let process, ready, diagnostics =
            spawn_maintenance_probe ~parent ~workspace ~cache
              "finalized-publication-probe"
          in
          let reaped = ref false in
          let probe, probe_status =
            Fun.protect
              ~finally:(fun () ->
                if not !reaped then (
                  (try Unix.kill process Sys.sigkill
                   with Unix.Unix_error _ -> ());
                  (try ignore (Unix.waitpid [] process)
                   with Unix.Unix_error _ -> ());
                  reaped := true))
              (fun () ->
                let probe =
                  match child_signal ready with
                  | Ok result -> result
                  | Error message -> "error:" ^ message
                in
                let status = snd (Unix.waitpid [] process) in
                reaped := true;
                (probe, status))
          in
          (match (probe, probe_status) with
          | "blocked", Unix.WEXITED 0 -> ()
          | message, Unix.WEXITED code ->
              Alcotest.failf "maintenance probe %S exited %d%s" message code
                (child_diagnostics diagnostics)
          | message, Unix.WSIGNALED signal | message, Unix.WSTOPPED signal ->
              Alcotest.failf "maintenance probe %S stopped by signal %d%s"
                message signal
                (child_diagnostics diagnostics));
          let published =
            get_ok
              (Engine.Run_store.publish_run finalization.publication (report id))
          in
          let loaded =
            get_ok (Engine.Run_store.load_run store (Core.Run_id.to_string id))
          in
          Alcotest.(check string)
            "published report survived maintenance race"
            (Engine.Run_store.run_to_string published.run)
            (Engine.Run_store.run_to_string loaded);
          ignore (get_ok (Engine.Run_store.gc store ~older_than_days:1));
          get_ok (Engine.Run_store.clean store)))

let test_process_crash_releases_lease_and_gc_collects_marker () =
  with_layout (fun ~parent ~cache ~workspace ~store ->
      let process, ready, release, _diagnostics =
        spawn_reservation_child ~parent ~workspace ~cache "crash-child"
      in
      try
        await_child_ready ready;
        let marker =
          match reservation_markers cache with
          | [ marker ] -> Filename.concat cache marker
          | markers ->
              Alcotest.failf "expected one child marker, found %d"
                (List.length markers)
        in
        Unix.kill process Sys.sigkill;
        ignore (Unix.waitpid [] process);
        Unix.utimes marker 1. 1.;
        let removed = get_ok (Engine.Run_store.gc store ~older_than_days:0) in
        Alcotest.(check bool) "orphan marker collected" true (removed >= 1);
        Alcotest.(check bool)
          "orphan marker absent" false (Sys.file_exists marker)
      with exception_ ->
        ignore (Engine.Util.atomic_write release "release");
        (try Unix.kill process Sys.sigkill with Unix.Unix_error _ -> ());
        (try ignore (Unix.waitpid [] process) with Unix.Unix_error _ -> ());
        raise exception_)

let with_closed_standard_input action =
  match Unix.dup Unix.stdin with
  | exception Unix.Unix_error (Unix.EBADF, _, _) -> action ()
  | saved_input ->
      Unix.set_close_on_exec saved_input;
      Fun.protect
        ~finally:(fun () ->
          Unix.dup2 saved_input Unix.stdin;
          close_descriptor_noerr saved_input)
        (fun () ->
          Unix.close Unix.stdin;
          action ())

let standard_input_is_closed () =
  match Unix.fstat Unix.stdin with
  | exception Unix.Unix_error (Unix.EBADF, _, _) -> true
  | _ -> false

let test_child_spawn_owns_standard_descriptors () =
  with_layout (fun ~parent ~cache ~workspace ~store:_ ->
      with_closed_standard_input (fun () ->
          Alcotest.(check bool)
            "closed inherited stdin precondition" true
            (standard_input_is_closed ());
          let process, ready, diagnostics =
            spawn_stdio_probe ~parent ~workspace ~cache "closed-stdin-probe"
          in
          let handshake = child_signal ready in
          let status = snd (Unix.waitpid [] process) in
          match (handshake, status) with
          | Ok "ready", Unix.WEXITED 0 -> ()
          | Ok message, Unix.WEXITED code ->
              Alcotest.failf "stdio probe %S exited %d%s" message code
                (child_diagnostics diagnostics)
          | Error message, Unix.WEXITED code ->
              Alcotest.failf "stdio probe handshake failed (%s), exit %d%s"
                message code
                (child_diagnostics diagnostics)
          | Ok message, (Unix.WSIGNALED signal | Unix.WSTOPPED signal) ->
              Alcotest.failf "stdio probe %S stopped by signal %d%s" message
                signal
                (child_diagnostics diagnostics)
          | Error message, (Unix.WSIGNALED signal | Unix.WSTOPPED signal) ->
              Alcotest.failf
                "stdio probe handshake failed (%s), stopped by signal %d%s"
                message signal
                (child_diagnostics diagnostics)))

let test_non_regular_lock_leaf_fails_through_native_contract () =
  with_layout (fun ~parent:_ ~cache ~workspace:_ ~store ->
      let lock_path =
        Filename.concat cache ".ocaml-mutants-maintenance-v1.lock"
      in
      Fun.protect
        ~finally:(fun () -> raw_remove_noerr lock_path)
        (fun () ->
          create_directory lock_path;
          match
            Engine.Run_store.reserve store ~started_at:"20260101T000000Z"
          with
          | Ok reservation ->
              ignore (Engine.Run_store.abandon_reservation store reservation);
              Alcotest.fail "native root lease accepted a directory lock leaf"
          | Error error ->
              Alcotest.(check string)
                "native lock failure cause" "io-failure"
                (Engine.Error.cause_name (Engine.Error.cause error));
              Alcotest.(check (option string))
                "Cache_fs operation is retained" (Some "try-lock")
                (List.assoc_opt "operation" (Engine.Error.context error));
              Alcotest.(check (option string))
                "Dir_cap primitive is retained" (Some "try-lock")
                (List.assoc_opt "primitive_operation"
                   (Engine.Error.context error));
              Alcotest.(check int)
                "failed native acquisition creates no reservation marker" 0
                (List.length (reservation_markers cache))))

let test_workspace_less_maintenance_uses_cwd_boundary () =
  let parent = Filename.temp_file "ocaml-mutants-maintenance-global-" ".tmp" in
  Sys.remove parent;
  create_directory parent;
  Fun.protect
    ~finally:(fun () -> ignore (Engine.Util.remove_tree parent))
    (fun () ->
      let cache = Filename.concat parent "cache" in
      let store = get_ok (Engine.Run_store.create ~directory:cache ()) in
      ignore (get_ok (Engine.Run_store.gc store ~older_than_days:0));
      get_ok (Engine.Run_store.clean store))

let run_tests () =
  Alcotest.run "Run_store maintenance lease contract"
    [
      ( "maintenance",
        [
          Alcotest.test_case "active reservation then save" `Quick
            test_active_reservation_blocks_maintenance_then_saves;
          Alcotest.test_case "staged conflict keeps lease" `Quick
            test_staged_conflict_keeps_lease_until_abandon;
          Alcotest.test_case "multiple reservations" `Quick
            test_multiple_reservations_hold_one_root_lease;
          Alcotest.test_case "live marker excludes ambient replacement" `Quick
            test_live_marker_excludes_ambient_replacement;
          Alcotest.test_case "negative gc age" `Quick
            test_negative_gc_age_rejected;
          Alcotest.test_case "resource busy report codec" `Quick
            test_resource_busy_report_codec_round_trip;
          Alcotest.test_case "application exception cleanup" `Quick
            test_application_exception_releases_reservation;
          Alcotest.test_case "cross-process reservation" `Quick
            test_cross_process_reservation_blocks_maintenance;
          Alcotest.test_case "cross-process publication lease" `Quick
            test_publication_lease_blocks_cross_process_maintenance;
          Alcotest.test_case "process crash releases lease" `Quick
            test_process_crash_releases_lease_and_gc_collects_marker;
          Alcotest.test_case "non-regular lock leaf is native failure" `Quick
            test_non_regular_lock_leaf_fails_through_native_contract;
          Alcotest.test_case "workspace-less maintenance boundary" `Quick
            test_workspace_less_maintenance_uses_cwd_boundary;
          Alcotest.test_case "child spawn owns standard descriptors" `Quick
            test_child_spawn_owns_standard_descriptors;
        ] );
    ]

let () =
  match Sys.getenv_opt child_mode with
  | Some "reservation" -> maintenance_child ()
  | Some "maintenance-probe" -> maintenance_probe_child ()
  | Some "stdio-probe" -> stdio_probe_child ()
  | _ -> run_tests ()
