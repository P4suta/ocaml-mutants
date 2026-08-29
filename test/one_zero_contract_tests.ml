module Core = Ocaml_mutants_core
module Engine = Ocaml_mutants_engine

let get_ok = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let get_ok_engine = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%a" Engine.Error.pp error

let check_error label = function
  | Error _ -> ()
  | Ok _ -> Alcotest.failf "%s: expected an error" label

let contains ~needle value =
  let rec search offset =
    offset + String.length needle <= String.length value
    && (String.sub value offset (String.length needle) = needle
       || search (offset + 1))
  in
  needle = "" || search 0

let mutant_at offset =
  let source = "true true true true" in
  let range =
    get_ok
      (Core.Source_range.make ~start_byte:offset ~end_byte:(offset + 4)
         ~start_line:1 ~start_column:offset ~end_line:1 ~end_column:(offset + 4))
  in
  let rule = get_ok (Core.Operator.Rule.of_stable_name "true-to-false@1") in
  let unchecked =
    match
      Core.Mutant.unchecked ~path:"lib/subject.ml" ~range ~rule
        ~replacement:"false"
    with
    | Ok value -> value
    | Error error -> Alcotest.failf "%a" Core.Mutant.pp_validation_error error
  in
  match
    Core.Mutant.validate ~source:(Core.Source.of_string source) unchecked
  with
  | Ok value -> value
  | Error error -> Alcotest.failf "%a" Core.Mutant.pp_validation_error error

let duration = get_ok (Core.Duration.of_seconds 0.1)
let command = get_ok (Core.Nonempty_argv.of_list [ "dune"; "runtest" ])

let result ?(origin = Engine.Run_store.Execution) ?expected_reason mutant
    outcome : Engine.Run_store.mutant_result =
  let cached =
    match origin with
    | Engine.Run_store.Execution | Engine.Run_store.Fast_estimated -> false
    | Engine.Run_store.Checkpoint_resume | Engine.Run_store.Checkpoint_estimated
    | Engine.Run_store.Historical_exact | Engine.Run_store.Historical_estimated
      ->
        true
  in
  {
    mutant;
    outcome;
    duration;
    cached;
    evidence_origin = origin;
    stages = [];
    timeout_confirmed = false;
    timeout_retry = None;
    expected_reason;
    stdout = Engine.Run_store.captured "";
    stderr = Engine.Run_store.captured "";
  }

let metadata ?(selection = "all") ?(config = Engine.Config.defaults) nonce :
    Engine.Run_store.metadata =
  let resolved_config, config_digest = Fixtures.config_evidence config in
  {
    id = get_ok (Core.Run_id.create ~started_at:"20260101T000000Z" ~nonce);
    started_at = "2026-01-01T00:00:00Z";
    finished_at = "2026-01-01T00:00:01Z";
    workspace_digest = String.make 64 'a';
    toolchain = "contract";
    profile = Core.Operator.Profile.Balanced;
    selection;
    test_command = command;
    baseline_duration = None;
    baseline_stages = [];
    hit_map = [];
    timeout = None;
    cache_mode = "off";
    execution_mode = "strict";
    historical_reuse = "off";
    cache_key = String.make 64 'b';
    resolved_config;
    input_fingerprint = String.make 64 'b';
    config_digest;
  }

let run ?(status = Engine.Run_store.Completed)
    ?(completeness = Engine.Run_store.Complete) ?(expectations = []) ?config
    nonce results : Engine.Run_store.run =
  {
    metadata = metadata ?config nonce;
    status;
    results;
    checkpointed =
      List.fold_left
        (fun count result ->
          if Engine.Run_store.checkpointable_result result then count + 1
          else count)
        0 results;
    completeness;
    expectations;
    skipped = [];
    warnings = [];
  }

let policy ?(require_complete = true) ?(max_unexpected_survivors = 0)
    ?minimum_score ?maximum_score_drop ?(allow_estimated = false) () =
  {
    Engine.Config.require_complete;
    max_unexpected_survivors;
    minimum_score;
    maximum_score_drop;
    allow_estimated;
  }

let check_verdict label expected_verdict expected_exit evaluation =
  Alcotest.(check string)
    (label ^ " verdict") expected_verdict
    (Engine.Policy.verdict_name evaluation.Engine.Policy.verdict);
  Alcotest.(check int) (label ^ " exit") expected_exit evaluation.exit_code

let test_policy_truth_table () =
  let killed = result (mutant_at 0) Core.Outcome.Killed in
  let survivor = result (mutant_at 5) Core.Outcome.Survived in
  Engine.Policy.evaluate ~policy:(policy ()) (run "pass" [ killed ])
  |> check_verdict "executed complete" "passed" 0;
  Engine.Policy.evaluate ~policy:(policy ()) (run "survivor" [ survivor ])
  |> check_verdict "survivor" "policy-violation" 1;
  let partial =
    run
      ~completeness:(Engine.Run_store.Partial [ mutant_at 5 ])
      "partial" [ killed ]
  in
  Engine.Policy.evaluate ~policy:(policy ()) partial
  |> check_verdict "partial" "refused" 2;
  Engine.Policy.evaluate ~policy:(policy ~require_complete:false ()) partial
  |> check_verdict "partial allowed" "passed" 0;
  let estimated =
    run "estimated"
      [
        result ~origin:Engine.Run_store.Historical_estimated (mutant_at 0)
          Core.Outcome.Killed;
      ]
  in
  Engine.Policy.evaluate ~policy:(policy ()) estimated
  |> check_verdict "estimated" "refused" 2;
  Engine.Policy.evaluate ~policy:(policy ~allow_estimated:true ()) estimated
  |> check_verdict "estimated allowed" "passed" 0;
  Engine.Policy.evaluate
    ~policy:(policy ~minimum_score:101. ())
    (run "score" [ killed ])
  |> check_verdict "minimum score" "policy-violation" 1;
  Engine.Policy.evaluate
    ~policy:(policy ~maximum_score_drop:0. ())
    (run "missing-reference" [ killed ])
  |> check_verdict "missing reference" "refused" 2

let test_expectation_policy () =
  let mutant = mutant_at 0 in
  let mutant_id = Core.Mutant.Id.full (Core.Mutant.id mutant) in
  let unfulfilled =
    {
      Engine.Run_store.mutant_id;
      reason = "equivalent";
      status = Engine.Run_store.Expectation_unfulfilled_killed;
    }
  in
  let evidence =
    run ~expectations:[ unfulfilled ] "unfulfilled"
      [ result ~expected_reason:"equivalent" mutant Core.Outcome.Killed ]
  in
  Engine.Policy.evaluate ~policy:(policy ()) evidence
  |> check_verdict "unfulfilled expectation" "policy-violation" 1;
  let stale =
    { unfulfilled with status = Engine.Run_store.Expectation_stale }
  in
  Engine.Policy.evaluate ~policy:(policy ())
    (run ~expectations:[ stale ] "stale" [ result mutant Core.Outcome.Killed ])
  |> check_verdict "stale expectation" "refused" 2

let test_event_contract () =
  let mutant = mutant_at 0 in
  let environment =
    Engine.Runner.For_testing.mutant_environment ~root:"workspace" mutant
  in
  Alcotest.(check (option (option string)))
    "mutant attempt clears inherited hit file" (Some None)
    (List.assoc_opt "OCAML_MUTANTS_HIT_FILE" environment);
  Alcotest.(check (option (option string)))
    "mutant attempt clears inherited hit owner" (Some None)
    (List.assoc_opt Core.Instrumentation.hit_owner_environment environment);
  Alcotest.(check (option (option string)))
    "mutant attempt activates the full ID"
    (Some (Some (Core.Mutant.Id.full (Core.Mutant.id mutant))))
    (List.assoc_opt "OCAML_MUTANTS_ACTIVE" environment);
  let pending_timeout = result mutant Core.Outcome.Timeout in
  Alcotest.(check bool)
    "initial timeout is not settled" false
    (Engine.Runner.For_testing.settlement_ready pending_timeout);
  Alcotest.(check bool)
    "ordinary result settles immediately" true
    (Engine.Runner.For_testing.settlement_ready
       (result (mutant_at 0) Core.Outcome.Survived));
  let event =
    Engine.Event_bus.Progress
      {
        phase = "mutation";
        completed = 3;
        total = 10;
        workers = 2;
        cache_hits = 1;
        resume_hits = 1;
        elapsed_seconds = 2.5;
        eta_seconds = Some 4.0;
      }
  in
  let json =
    Engine.Event_bus.to_yojson ~sequence:7 ~timestamp:"20260101T000000Z" event
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "document type" "ocaml-mutants.event-v1"
    (json |> member "document_type" |> to_string);
  Alcotest.(check int) "sequence" 7 (json |> member "sequence" |> to_int);
  Alcotest.(check string)
    "event type" "progress"
    (json |> member "type" |> to_string);
  Alcotest.(check int)
    "completed" 3
    (json |> member "payload" |> member "completed" |> to_int);
  let observed = ref [] in
  Engine.Event_bus.with_sink
    (Engine.Event_bus.callback (fun event -> observed := event :: !observed))
    (fun () ->
      Engine.Event_bus.emit (Engine.Event_bus.Run_started { run_id = "run" });
      Engine.Event_bus.emit (Engine.Event_bus.Run_finished { exit_code = 0 }));
  match List.rev !observed with
  | [ Engine.Event_bus.Run_started _; Engine.Event_bus.Run_finished _ ] -> ()
  | events -> Alcotest.failf "callback observed %d events" (List.length events)

let test_event_callback_can_emit_reentrantly () =
  let observed = ref [] in
  Engine.Event_bus.with_sink
    (Engine.Event_bus.callback (fun event ->
         observed := event :: !observed;
         match event with
         | Engine.Event_bus.Run_started _ ->
             Engine.Event_bus.emit
               (Engine.Event_bus.Warning
                  { code = "nested"; message = "reentrant" })
         | _ -> ()))
    (fun () ->
      Engine.Event_bus.emit (Engine.Event_bus.Run_started { run_id = "run" }));
  Alcotest.(check int)
    "reentrant callback receives both events" 2 (List.length !observed)

let catalog_and_mutants () =
  let mutants = List.map mutant_at [ 0; 5; 10; 15 ] in
  let catalog =
    match Core.Catalog.of_list mutants with
    | Ok value -> value
    | Error error -> Alcotest.failf "%a" Core.Catalog.pp_error error
  in
  (catalog, mutants)

let test_shard_plan_and_merge () =
  let catalog, mutants = catalog_and_mutants () in
  let durations =
    List.mapi
      (fun index mutant ->
        ( Core.Mutant.Id.full (Core.Mutant.id mutant),
          float_of_int (List.length mutants - index) ))
      mutants
  in
  let create () =
    get_ok
      (Engine.Shard_plan.create ~workspace_digest:(String.make 64 'a')
         ~toolchain:"contract" ~config:Engine.Config.defaults ~catalog
         ~shard_count:2 ~durations)
  in
  let first = create () and second = create () in
  Alcotest.(check string)
    "deterministic plan"
    (Engine.Shard_plan.to_string first)
    (Engine.Shard_plan.to_string second);
  let decoded =
    get_ok (Engine.Shard_plan.of_string (Engine.Shard_plan.to_string first))
  in
  Alcotest.(check string)
    "round trip"
    (Engine.Shard_plan.to_string first)
    (Engine.Shard_plan.to_string decoded);
  let by_id = Hashtbl.create 4 in
  List.iter
    (fun mutant ->
      Hashtbl.add by_id (Core.Mutant.Id.full (Core.Mutant.id mutant)) mutant)
    mutants;
  let shard_run index =
    let assignment = get_ok (Engine.Shard_plan.assignment first index) in
    let results =
      List.map
        (fun id -> result (Hashtbl.find by_id id) Core.Outcome.Killed)
        assignment.mutant_ids
    in
    {
      (run ("shard" ^ string_of_int index) results) with
      metadata =
        metadata
          ~selection:(Engine.Shard_plan.selection_tag first ~index)
          ("shard" ^ string_of_int index);
    }
  in
  let shard0 = shard_run 0 and shard1 = shard_run 1 in
  check_error "missing shard"
    (Engine.Shard_plan.merge ~plan:first
       ~id:
         (get_ok
            (Core.Run_id.create ~started_at:"20260101T000000Z"
               ~nonce:"merged-missing"))
       ~finished_at:"2026-01-01T00:00:02Z" [ shard0 ]);
  check_error "duplicate shard"
    (Engine.Shard_plan.merge ~plan:first
       ~id:
         (get_ok
            (Core.Run_id.create ~started_at:"20260101T000000Z"
               ~nonce:"merged-duplicate"))
       ~finished_at:"2026-01-01T00:00:02Z" [ shard0; shard0 ]);
  let merged =
    get_ok
      (Engine.Shard_plan.merge ~plan:first
         ~id:
           (get_ok
              (Core.Run_id.create ~started_at:"20260101T000000Z" ~nonce:"merged"))
         ~finished_at:"2026-01-01T00:00:02Z" [ shard1; shard0 ])
  in
  Alcotest.(check int) "merged result count" 4 (List.length merged.results);
  Alcotest.(check bool)
    "merged complete" true
    (merged.completeness = Engine.Run_store.Complete)

let with_owned_temp prefix action =
  let root = Filename.temp_dir ~perms:0o700 prefix ".tmp" |> Unix.realpath in
  Fun.protect
    (fun () -> action root)
    ~finally:(fun () ->
      match Engine.Util.remove_tree root with
      | Ok () -> ()
      | Error message -> Alcotest.failf "cleanup %s: %s" root message)

let test_plain_event_rate_limit () =
  with_owned_temp "ocaml-mutants-events-" (fun root ->
      let path = Filename.concat root "events.log" in
      let channel = open_out_bin path in
      Fun.protect
        (fun () ->
          Engine.Event_bus.with_sink (Engine.Event_bus.plain channel) (fun () ->
              let emit completed total =
                Engine.Event_bus.emit
                  (Engine.Event_bus.Progress
                     {
                       phase = "mutation";
                       completed;
                       total;
                       workers = 2;
                       cache_hits = 0;
                       resume_hits = 0;
                       elapsed_seconds = 0.1;
                       eta_seconds = Some 0.1;
                     })
              in
              emit 1 3;
              emit 2 3;
              emit 3 3))
        ~finally:(fun () -> close_out_noerr channel);
      let lines =
        get_ok (Engine.Util.read_file path) |> Engine.Util.split_lines
      in
      let progress_lines =
        List.filter (fun line -> contains ~needle:"] mutation" line) lines
      in
      Alcotest.(check int)
        "plain progress emits initial and final lines" 2
        (List.length progress_lines))

let test_journal_resume () =
  with_owned_temp "ocaml-mutants-journal-contract-" (fun parent ->
      let workspace = Filename.concat parent "workspace" in
      Unix.mkdir workspace 0o700;
      let cache = Filename.concat parent "cache" in
      let store =
        get_ok_engine (Engine.Run_store.create ~workspace ~directory:cache ())
      in
      let key = String.make 64 'c' in
      let mutant = mutant_at 0 in
      let source = Core.Source.of_string "true true true true" in
      let first =
        get_ok_engine
          (Engine.Run_store.reserve store ~started_at:"20260101T000000Z")
      in
      let journal =
        get_ok_engine
          (Engine.Run_store.open_journal store first ~key ~fresh:false)
      in
      Alcotest.(check bool)
        "new journal" false
        (Engine.Run_store.journal_resumed journal);
      let settled = result mutant Core.Outcome.Killed in
      get_ok_engine (Engine.Run_store.checkpoint_mutant journal settled);
      let checkpoint_name =
        Core.Mutant.Id.full (Core.Mutant.id mutant) ^ ".json"
      in
      let checkpoint_path =
        Engine.Util.files_recursive cache
        |> List.filter (fun relative ->
            String.equal (Filename.basename relative) checkpoint_name)
        |> function
        | [ relative ] -> Filename.concat cache relative
        | paths ->
            Alcotest.failf "expected one checkpoint path, got %d"
              (List.length paths)
      in
      get_ok (Engine.Util.write_file checkpoint_path "{corrupt");
      get_ok_engine (Engine.Run_store.checkpoint_mutant journal settled);
      check_error "valid immutable checkpoint conflicts"
        (Engine.Run_store.checkpoint_mutant journal
           { settled with outcome = Core.Outcome.Survived });
      let estimated_mutant = mutant_at 5 in
      let estimated =
        result ~origin:Engine.Run_store.Fast_estimated estimated_mutant
          Core.Outcome.Survived
      in
      Alcotest.(check bool)
        "fast estimate is not historically cacheable" false
        (Engine.Run_store.cacheable_result estimated);
      get_ok_engine (Engine.Run_store.checkpoint_mutant journal estimated);
      get_ok_engine (Engine.Run_store.abandon_reservation store first);
      let second =
        get_ok_engine
          (Engine.Run_store.reserve store ~started_at:"20260101T000001Z")
      in
      let resumed =
        get_ok_engine
          (Engine.Run_store.open_journal store second ~key ~fresh:false)
      in
      Alcotest.(check bool)
        "resumed journal" true
        (Engine.Run_store.journal_resumed resumed);
      let restored =
        get_ok_engine
          (Engine.Run_store.load_checkpoint resumed ~source ~expected:mutant)
      in
      (match restored with
      | Some restored ->
          Alcotest.(check bool) "restored is cached" true restored.cached;
          Alcotest.(check bool)
            "restored origin" true
            (restored.evidence_origin = Engine.Run_store.Checkpoint_resume)
      | None -> Alcotest.fail "checkpoint was not restored");
      let restored_estimate =
        get_ok_engine
          (Engine.Run_store.load_checkpoint resumed ~source
             ~expected:estimated_mutant)
      in
      (match restored_estimate with
      | Some restored ->
          Alcotest.(check bool)
            "estimated checkpoint is cached" true restored.cached;
          Alcotest.(check bool)
            "estimated checkpoint does not become exact" true
            (restored.evidence_origin = Engine.Run_store.Checkpoint_estimated)
      | None -> Alcotest.fail "estimated checkpoint was not restored");
      get_ok_engine (Engine.Run_store.complete_journal resumed);
      get_ok_engine (Engine.Run_store.abandon_reservation store second))

let test_fast_stage_selection () =
  let mutant = mutant_at 0 in
  let id = Core.Mutant.Id.full (Core.Mutant.id mutant) in
  let stages =
    [
      { Engine.Config.name = "covering"; command };
      { Engine.Config.name = "remaining"; command };
    ]
  in
  let hit_map =
    [
      { Engine.Run_store.test = "covering"; mutant_ids = [ id ] };
      { Engine.Run_store.test = "remaining"; mutant_ids = [] };
    ]
  in
  let config mode =
    {
      Engine.Config.defaults with
      test = { Engine.Config.defaults.test with stages };
      execution = { Engine.Config.defaults.execution with mode };
    }
  in
  let strict_stages, strict_omitted =
    Engine.Runner.For_testing.selected_stages
      (config Engine.Config.Strict)
      hit_map mutant
  in
  Alcotest.(check (list string))
    "strict keeps covering stages first and remains exhaustive"
    [ "covering"; "remaining" ]
    strict_stages;
  Alcotest.(check bool) "strict omits no stage" false strict_omitted;
  let fast_stages, fast_omitted =
    Engine.Runner.For_testing.selected_stages
      (config Engine.Config.Fast)
      hit_map mutant
  in
  Alcotest.(check (list string))
    "fast runs only covering stages" [ "covering" ] fast_stages;
  Alcotest.(check bool) "fast records its omission" true fast_omitted;
  let fallback_stages, fallback_omitted =
    Engine.Runner.For_testing.selected_stages
      (config Engine.Config.Fast)
      [] mutant
  in
  Alcotest.(check (list string))
    "fast without an inventory fails closed to all stages"
    [ "covering"; "remaining" ]
    fallback_stages;
  Alcotest.(check bool)
    "fast fallback has no inferred omission" false fallback_omitted

let test_dune_driver_plan_and_private_cache () =
  let described : Engine.Dune_adapter.described_test =
    {
      name = "subject";
      source_dir = "test";
      target = "_build/default/test/subject.exe";
    }
  in
  let effective, baseline =
    get_ok_engine
      (Engine.Runner.For_testing.resolved_test_plan Engine.Config.defaults
         [ described ])
  in
  Alcotest.(check bool)
    "auto resolves to dune" true
    (effective.test.driver = Engine.Config.Dune_driver);
  let stage_names (config : Engine.Config.t) =
    List.map
      (fun (stage : Engine.Config.stage) -> stage.name)
      config.test.stages
  in
  Alcotest.(check (list string))
    "individual alias followed by exhaustive fallback"
    [ "dune:@test/runtest-subject"; "dune:@runtest" ]
    (stage_names effective);
  Alcotest.(check (list string))
    "baseline executes the exhaustive alias once" [ "dune:@runtest" ]
    (stage_names baseline);
  let individual = List.hd effective.test.stages in
  Alcotest.(check (list string))
    "custom Dune action remains behind its alias"
    [ "dune"; "build"; "--force"; "@test/runtest-subject" ]
    (Core.Nonempty_argv.to_list individual.command);
  let mutant = mutant_at 0 in
  let id = Core.Mutant.Id.full (Core.Mutant.id mutant) in
  let other = Core.Mutant.Id.full (Core.Mutant.id (mutant_at 5)) in
  let classified =
    Engine.Runner.For_testing.classify_exhaustive_hits effective
      [
        { Engine.Run_store.test = individual.name; mutant_ids = [ id ] };
        { Engine.Run_store.test = "dune:@runtest"; mutant_ids = [ id; other ] };
      ]
  in
  let fallback_hits =
    List.find
      (fun (entry : Engine.Run_store.hit_map_entry) ->
        String.equal entry.test "dune:@runtest")
      classified
  in
  Alcotest.(check (list string))
    "global hits retain only unclassified rules" [ other ]
    fallback_hits.mutant_ids;
  let strict_stages, strict_omitted =
    Engine.Runner.For_testing.selected_stages effective classified mutant
  in
  Alcotest.(check (list string))
    "strict prioritizes covering test then proves exhaustive remainder"
    [ individual.name; "dune:@runtest" ]
    strict_stages;
  Alcotest.(check bool) "strict evidence is complete" false strict_omitted;
  let fast =
    {
      effective with
      execution = { effective.execution with mode = Engine.Config.Fast };
    }
  in
  let fast_stages, fast_omitted =
    Engine.Runner.For_testing.selected_stages fast classified mutant
  in
  Alcotest.(check (list string))
    "fast executes only the known covering alias" [ individual.name ]
    fast_stages;
  Alcotest.(check bool) "fast marks omitted fallback" true fast_omitted;
  let shell = get_ok (Core.Nonempty_argv.of_list [ "custom-test" ]) in
  let custom =
    {
      Engine.Config.defaults with
      test =
        {
          Engine.Config.defaults.test with
          driver = Engine.Config.Auto_driver;
          command = shell;
          stages = [ { Engine.Config.name = "custom"; command = shell } ];
        };
    }
  in
  let custom, custom_baseline =
    get_ok_engine
      (Engine.Runner.For_testing.resolved_test_plan custom [ described ])
  in
  Alcotest.(check bool)
    "auto preserves a custom command driver" true
    (custom.test.driver = Engine.Config.Command_driver);
  Alcotest.(check (list string))
    "custom ordered stages remain verbatim" [ "custom" ] (stage_names custom);
  Alcotest.(check (list string))
    "custom baseline uses the same stages" [ "custom" ]
    (stage_names custom_baseline);
  Alcotest.(check (list (pair string (option string))))
    "private Dune cache excludes user rules"
    [
      ("DUNE_CACHE", Some "enabled-except-user-rules");
      ( "DUNE_CACHE_ROOT",
        Some (Filename.concat "snapshot" ".ocaml-mutants-compiler-cache") );
    ]
    (Engine.Test_command.dune_cache_environment ~root:"snapshot")

let test_tui_model_and_html_safety () =
  Alcotest.(check string)
    "NO_COLOR strips SGR but preserves terminal protocols"
    "ared\027[>4;1m\027[?25l\027[0 qz"
    (Engine.Tui.For_testing.monochrome
       [ "a\027[38;2;"; "255;0;0mred\027[>4"; ";1m\027[?25l\027[0 q\027[mz" ]);
  Alcotest.(check string)
    "literal redaction masks every match" "*****=* *****=*"
    (Engine.Runner.For_testing.redact [ "token"; "a" ] "token=a token=a");
  let killed = result (mutant_at 0) Core.Outcome.Killed in
  let survived = result (mutant_at 5) Core.Outcome.Survived in
  let errored =
    {
      (result (mutant_at 10) (Core.Outcome.Error "</script>\001")) with
      stderr =
        Engine.Run_store.captured "</script><script>alert(1)</script>\001";
    }
  in
  let evidence = run "tui" [ killed; survived; errored ] in
  let older = run "older" [ survived ] in
  let evidence_id = Core.Run_id.to_string evidence.metadata.id in
  let history = Engine.Tui.init_history [ evidence; older ] in
  let older_model = Engine.Tui.update_model Engine.Tui.Previous_run history in
  Alcotest.(check int) "history moves to older run" 1 older_model.history_index;
  Alcotest.(check string)
    "history selects older run"
    (Core.Run_id.to_string older.metadata.id)
    (match older_model.run with
    | Some run -> Core.Run_id.to_string run.metadata.id
    | None -> "missing");
  let newest = Engine.Tui.update_model Engine.Tui.Next_run older_model in
  Alcotest.(check int) "history moves to newer run" 0 newest.history_index;
  let model = Engine.Tui.init ~width:40 ~height:10 (Some evidence) in
  Alcotest.(check int)
    "actionable filter" 2
    (List.length (Engine.Tui.visible_results model));
  let moved = Engine.Tui.update_model (Engine.Tui.Move 99) model in
  Alcotest.(check int) "selection clamps" 1 moved.selected;
  let all = Engine.Tui.update_model Engine.Tui.Cycle_filter model in
  Alcotest.(check int)
    "all filter" 3
    (List.length (Engine.Tui.visible_results all));
  let resized = Engine.Tui.update_model (Engine.Tui.Resize (0, 0)) model in
  Alcotest.(check int) "minimum width" 1 resized.width;
  Alcotest.(check int) "minimum height" 1 resized.height;
  let live_model =
    {
      model with
      live_status = Engine.Tui.Running (Engine.Cancel.create ());
      run = None;
    }
  in
  let live =
    Engine.Tui.update_model
      (Engine.Tui.Live_event
         (Engine.Event_bus.Run_started { run_id = evidence_id }))
      live_model
    |> Engine.Tui.update_model
         (Engine.Tui.Live_event
            (Engine.Event_bus.Phase_started
               { phase = "mutation"; total = Some 3 }))
    |> Engine.Tui.update_model
         (Engine.Tui.Live_event
            (Engine.Event_bus.Progress
               {
                 phase = "mutation";
                 completed = 1;
                 total = 3;
                 workers = 2;
                 cache_hits = 0;
                 resume_hits = 1;
                 elapsed_seconds = 1.5;
                 eta_seconds = Some 3.;
               }))
    |> Engine.Tui.update_model
         (Engine.Tui.Live_event
            (Engine.Event_bus.Mutant_settled
               { result = survived; coverage = "covered" }))
    |> Engine.Tui.update_model
         (Engine.Tui.Live_event
            (Engine.Event_bus.Warning { code = "proof"; message = "warning" }))
  in
  Alcotest.(check (option string))
    "live run id" (Some evidence_id) live.live_run_id;
  Alcotest.(check (option string))
    "live phase" (Some "mutation") live.live_phase;
  Alcotest.(check int) "settled count" 1 live.live_settled;
  Alcotest.(check int)
    "settled detail is immediately actionable" 1
    (List.length (Engine.Tui.visible_results live));
  Alcotest.(check string)
    "live detail retains the full mutant ID"
    (Core.Mutant.Id.full (Core.Mutant.id survived.mutant))
    (match Engine.Tui.visible_results live with
    | result :: _ -> Core.Mutant.Id.full (Core.Mutant.id result.mutant)
    | [] -> "missing");
  Alcotest.(check int)
    "progress completed" 1
    (Option.fold ~none:(-1)
       ~some:(fun (progress : Engine.Event_bus.progress) -> progress.completed)
       live.live_progress);
  Alcotest.(check int) "warning retained" 1 (List.length live.live_warnings);
  let refreshed =
    Engine.Tui.update_model
      (Engine.Tui.Live_finished
         { exit_code = 0; runs = Some [ evidence; older ]; error = None })
      live
  in
  Alcotest.(check int) "finished run reselected by id" 0 refreshed.history_index;
  Alcotest.(check string)
    "finished run is authoritative report" evidence_id
    (match refreshed.run with
    | Some run -> Core.Run_id.to_string run.metadata.id
    | None -> "missing");
  ignore (Engine.Tui.view resized);
  let html =
    get_ok_engine
      (Engine.Artifact_report.render ~root:"." ~color:false
         Engine.Artifact_report.Html evidence)
  in
  Alcotest.(check bool)
    "CSP present" true
    (contains ~needle:"Content-Security-Policy" html
    && String.starts_with ~prefix:"<!doctype html>" html);
  Alcotest.(check bool)
    "raw closing script escaped" false
    (contains ~needle:"</script><script>alert(1)" html);
  let no_source_config =
    {
      Engine.Config.defaults with
      privacy =
        {
          Engine.Config.defaults.privacy with
          source_embedding = Engine.Config.No_source;
        };
    }
  in
  let no_source = run ~config:no_source_config "no-source" [ survived ] in
  let no_source_html =
    get_ok_engine
      (Engine.Artifact_report.render ~root:"." ~color:false
         Engine.Artifact_report.Html no_source)
  in
  Alcotest.(check bool)
    "none embedding omits mutation text" true
    (contains ~needle:"\"original\":\"\"" no_source_html
    && contains ~needle:"\"replacement\":\"\"" no_source_html);
  with_owned_temp "ocaml-mutants-html-source-" (fun root ->
      let library = Filename.concat root "lib" in
      Unix.mkdir library 0o700;
      get_ok
        (Engine.Util.write_file
           (Filename.concat library "subject.ml")
           "true true true true");
      let all_source_config =
        {
          Engine.Config.defaults with
          privacy =
            {
              Engine.Config.defaults.privacy with
              source_embedding = Engine.Config.All_source;
            };
        }
      in
      let all_source =
        run ~config:all_source_config "all-source" [ survived ]
      in
      let all_source_html =
        get_ok_engine
          (Engine.Artifact_report.render ~root ~color:false
             Engine.Artifact_report.Html all_source)
      in
      Alcotest.(check bool)
        "all embedding includes verified source" true
        (contains ~needle:"true true true true" all_source_html);
      get_ok
        (Engine.Util.write_file (Filename.concat library "subject.ml") "false");
      check_error "changed source embedding"
        (Engine.Artifact_report.render ~root ~color:false
           Engine.Artifact_report.Html all_source))

let test_tui_history_isolates_corrupt_reports () =
  with_owned_temp "ocaml-mutants-tui-history-" (fun directory ->
      let store =
        get_ok_engine (Engine.Run_store.create ~workspace:"." ~directory ())
      in
      let reservation =
        get_ok_engine
          (Engine.Run_store.reserve store ~started_at:"20260101T000000Z")
      in
      let id = Engine.Run_store.reservation_id reservation in
      let valid =
        let value = run "stored" [ result (mutant_at 0) Core.Outcome.Killed ] in
        { value with metadata = { value.metadata with id } }
      in
      let staged =
        get_ok_engine (Engine.Run_store.stage_run store reservation)
      in
      let finalization = get_ok_engine (Engine.Run_store.finalize_run staged) in
      Alcotest.(check int)
        "publication cleanup" 0
        (List.length finalization.cleanup_errors);
      let published =
        get_ok_engine
          (Engine.Run_store.publish_run finalization.publication valid)
      in
      let corrupt_id =
        get_ok (Core.Run_id.create ~started_at:"20990101T000000Z" ~nonce:"bad")
        |> Core.Run_id.to_string
      in
      let corrupt_path =
        Filename.concat (Filename.dirname published.path) (corrupt_id ^ ".json")
      in
      get_ok (Engine.Util.atomic_write corrupt_path "{not-json");
      check_error "strict history rejects corrupt report"
        (Engine.Run_store.list_runs store);
      let loaded, rejected = Engine.Run_store.list_runs_best_effort store in
      Alcotest.(check int) "valid history retained" 1 (List.length loaded);
      Alcotest.(check string)
        "valid run identity" (Core.Run_id.to_string id)
        (match loaded with
        | [ run ] -> Core.Run_id.to_string run.metadata.id
        | _ -> "missing");
      Alcotest.(check (list string))
        "corrupt run isolated by ID" [ corrupt_id ] (List.map fst rejected))

let test_long_path_io () =
  with_owned_temp "ocaml-mutants-long-path-" (fun root ->
      let rec deepen directory depth =
        let file = Filename.concat directory "proof.json" in
        if String.length file > 260 then (directory, file)
        else if depth = 8 then
          Alcotest.fail "could not construct a bounded path beyond MAX_PATH"
        else
          deepen
            (Filename.concat directory
               (Printf.sprintf "%02d-%s" depth (String.make 60 'a')))
            (depth + 1)
      in
      let directory, file = deepen root 0 in
      get_ok (Engine.Util.mkdir_p directory);
      get_ok (Engine.Util.write_file file "proof");
      Alcotest.(check bool)
        "path exceeds MAX_PATH" true
        (String.length file > 260);
      Alcotest.(check (result string string))
        "extended read" (Ok "proof")
        (Engine.Util.read_file file);
      Alcotest.(check int)
        "recursive enumeration" 1
        (List.length (Engine.Util.files_recursive root));
      if Sys.win32 then
        let dotted =
          Filename.concat root
            (Filename.concat "discarded" (Filename.concat ".." "normalized"))
        in
        let extended = Engine.Util.windows_extended_path dotted in
        Alcotest.(check bool)
          "extended paths contain no parent traversal" false
          (contains ~needle:"\\..\\" extended))

let test_revert_rejects_nonregular_source () =
  with_owned_temp "ocaml-mutants-revert-guard-" (fun root ->
      let library = Filename.concat root "lib" in
      Unix.mkdir library 0o700;
      let target = Filename.concat library "subject.ml" in
      get_ok (Engine.Util.write_file target "true true true true");
      let mutant = mutant_at 0 in
      get_ok_engine (Engine.Mutant_workflow.apply ~root mutant);
      Sys.remove target;
      Unix.mkdir target 0o700;
      check_error "revert rejects a non-regular source"
        (Engine.Mutant_workflow.revert ~root
           ~id:(Core.Mutant.Id.full (Core.Mutant.id mutant))))

let () =
  Alcotest.run "ocaml-mutants 1.0 contracts"
    [
      ( "policy",
        [
          Alcotest.test_case "truth table" `Quick test_policy_truth_table;
          Alcotest.test_case "expectations" `Quick test_expectation_policy;
        ] );
      ( "events",
        [
          Alcotest.test_case "versioned event and sink" `Quick
            test_event_contract;
          Alcotest.test_case "callback emission is reentrant" `Quick
            test_event_callback_can_emit_reentrantly;
          Alcotest.test_case "plain progress is rate limited" `Quick
            test_plain_event_rate_limit;
        ] );
      ( "shards",
        [
          Alcotest.test_case "determinism and merge" `Quick
            test_shard_plan_and_merge;
        ] );
      ( "journal",
        [
          Alcotest.test_case "checkpoint resume" `Quick test_journal_resume;
          Alcotest.test_case "fast stage selection" `Quick
            test_fast_stage_selection;
          Alcotest.test_case "Dune driver and private artifact cache" `Quick
            test_dune_driver_plan_and_private_cache;
        ] );
      ( "ui-artifact",
        [
          Alcotest.test_case "model and XSS" `Quick
            test_tui_model_and_html_safety;
          Alcotest.test_case "corrupt history isolation" `Quick
            test_tui_history_isolates_corrupt_reports;
        ] );
      ( "windows-path",
        [ Alcotest.test_case "extended I/O" `Quick test_long_path_io ] );
      ( "mutant-workflow",
        [
          Alcotest.test_case "revert rejects non-regular source" `Quick
            test_revert_rejects_nonregular_source;
        ] );
    ]
