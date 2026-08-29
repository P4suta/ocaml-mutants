module Core = Ocaml_mutants_core
module Engine = Ocaml_mutants_engine

let get_ok = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let duration seconds = get_ok (Core.Duration.of_seconds seconds)

let mutant ~path ~source ~rule_name ~replacement =
  let range =
    get_ok
      (Core.Source_range.make ~start_byte:0 ~end_byte:(String.length source)
         ~start_line:1 ~start_column:0 ~end_line:1
         ~end_column:(String.length source))
  in
  let rule = get_ok (Core.Operator.Rule.of_stable_name rule_name) in
  let unchecked =
    match Core.Mutant.unchecked ~path ~range ~rule ~replacement with
    | Ok value -> value
    | Error error -> Alcotest.failf "%a" Core.Mutant.pp_validation_error error
  in
  match
    Core.Mutant.validate ~source:(Core.Source.of_string source) unchecked
  with
  | Ok value -> value
  | Error error -> Alcotest.failf "%a" Core.Mutant.pp_validation_error error

let expected_mutant =
  mutant ~path:"lib/expected.ml" ~source:"true" ~rule_name:"true-to-false@1"
    ~replacement:"false"

let killed_mutant =
  mutant ~path:"lib/killed.ml" ~source:"false" ~rule_name:"false-to-true@1"
    ~replacement:"true"

let pending_mutant =
  mutant ~path:"lib/pending.ml" ~source:"true" ~rule_name:"true-to-false@1"
    ~replacement:"false"

let command = get_ok (Core.Nonempty_argv.of_list [ "dune"; "runtest" ])

let result ?expected_reason mutant outcome : Engine.Run_store.mutant_result =
  {
    mutant;
    outcome;
    duration = duration 0.1;
    cached = false;
    evidence_origin = Engine.Run_store.Execution;
    stages = [];
    timeout_confirmed = false;
    timeout_retry = None;
    expected_reason;
    stdout = Engine.Run_store.captured "stdout";
    stderr = Engine.Run_store.captured "stderr";
  }

let expected_reason = "equivalent by construction"

let metadata id : Engine.Run_store.metadata =
  {
    id;
    started_at = "2026-01-01T00:00:00Z";
    finished_at = "2026-01-01T00:00:01Z";
    workspace_digest = String.make 64 'a';
    toolchain = "ocaml=5.5;dune=3.21";
    profile = Core.Operator.Profile.Balanced;
    selection = "all";
    test_command = command;
    baseline_duration = Some (duration 0.3);
    baseline_stages =
      [
        {
          Engine.Run_store.name = "full";
          command;
          runs = [ duration 0.1; duration 0.3 ];
          slowest = duration 0.3;
        };
      ];
    hit_map =
      [
        {
          Engine.Run_store.test = "full";
          mutant_ids = [ Core.Mutant.Id.full (Core.Mutant.id expected_mutant) ];
        };
      ];
    timeout = Some (duration 10.);
    cache_mode = "off";
    execution_mode = "strict";
    historical_reuse = "off";
    cache_key = String.make 64 'b';
    resolved_config = Engine.Config.to_yojson Engine.Config.defaults;
    input_fingerprint = String.make 64 'b';
    config_digest =
      Engine.Util.sha256
        (Yojson.Safe.to_string ~std:true
           (Engine.Config.to_yojson Engine.Config.defaults));
  }

let run_id =
  get_ok
    (Core.Run_id.create ~started_at:"20260101T000000Z"
       ~nonce:"report-consistency")

let expected_id = Core.Mutant.Id.full (Core.Mutant.id expected_mutant)

let base_run () : Engine.Run_store.run =
  {
    metadata = metadata run_id;
    status = Engine.Run_store.Interrupted;
    results =
      [
        result ~expected_reason expected_mutant Core.Outcome.Survived;
        result killed_mutant Core.Outcome.Killed;
      ];
    completeness = Engine.Run_store.Partial [ pending_mutant ];
    expectations =
      [
        {
          Engine.Run_store.mutant_id = expected_id;
          reason = expected_reason;
          status = Engine.Run_store.Expectation_fulfilled;
        };
      ];
    skipped = [];
    warnings = [];
  }

let replace_member name replacement = function
  | `Assoc fields ->
      if List.mem_assoc name fields then
        `Assoc
          (List.map
             (fun (key, value) ->
               if String.equal key name then (key, replacement) else (key, value))
             fields)
      else Alcotest.failf "JSON object has no %S member" name
  | _ -> Alcotest.fail "expected a JSON object"

let remove_member name = function
  | `Assoc fields ->
      if List.mem_assoc name fields then
        `Assoc
          (List.filter (fun (key, _) -> not (String.equal key name)) fields)
      else Alcotest.failf "JSON object has no %S member" name
  | _ -> Alcotest.fail "expected a JSON object"

let map_member name transform json =
  let open Yojson.Safe.Util in
  replace_member name (transform (member name json)) json

let map_first list_name transform json =
  map_member list_name
    (function
      | `List (first :: rest) -> `List (transform first :: rest)
      | _ -> Alcotest.failf "expected a non-empty %S array" list_name)
    json

let append_first list_name json =
  map_member list_name
    (function
      | `List (first :: rest) -> `List (first :: first :: rest)
      | _ -> Alcotest.failf "expected a non-empty %S array" list_name)
    json

let decode label json =
  match Engine.Run_store.run_of_json json with
  | Ok run -> run
  | Error message -> Alcotest.failf "%s: %s" label message

let reject label json =
  match Engine.Run_store.run_of_json json with
  | Error _ -> ()
  | Ok _ -> Alcotest.failf "%s was accepted" label

let check_round_trip label run =
  let encoded = Engine.Run_store.run_to_yojson run in
  let decoded = decode label encoded in
  Alcotest.(check string)
    label
    (Yojson.Safe.to_string encoded)
    (Yojson.Safe.to_string (Engine.Run_store.run_to_yojson decoded))

let test_completeness_round_trip () =
  let partial = base_run () in
  check_round_trip "partial report" partial;
  let failure =
    Engine.Error.create ~phase:Engine.Error.Analysis
      ~cause:Engine.Error.Decode_failure "catalog unavailable"
  in
  let unknown_catalog =
    {
      partial with
      metadata =
        {
          partial.metadata with
          baseline_duration = None;
          baseline_stages = [];
          hit_map = [];
          timeout = None;
        };
      status = Engine.Run_store.Failed failure;
      results = [];
      completeness = Engine.Run_store.Partial [];
      expectations = [];
    }
  in
  check_round_trip "partial report without a catalog" unknown_catalog;
  let complete =
    {
      partial with
      status = Engine.Run_store.Completed;
      completeness = Engine.Run_store.Complete;
    }
  in
  check_round_trip "complete report" complete;
  let encoded = Engine.Run_store.run_to_yojson partial in
  reject "complete summary with an unexecuted mutant"
    (map_member "summary" (replace_member "kind" (`String "complete")) encoded)

let test_summary_is_derived () =
  let encoded = Engine.Run_store.run_to_yojson (base_run ()) in
  let open Yojson.Safe.Util in
  let corrupt field =
    let summary = encoded |> member "summary" in
    let value = summary |> member field |> to_int in
    map_member "summary" (replace_member field (`Int (value + 1))) encoded
  in
  List.iter
    (fun field -> reject ("contradictory summary." ^ field) (corrupt field))
    [
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
    ];
  (* 250 is outside the reachable [0, 100] range, so it contradicts every
     genuine score including the null one. *)
  reject "contradictory summary.score"
    (map_member "summary" (replace_member "score" (`Float 250.0)) encoded);
  reject "unknown summary kind"
    (map_member "summary" (replace_member "kind" (`String "unknown")) encoded)

let test_status_and_failure_are_consistent () =
  let run = base_run () in
  let encoded = Engine.Run_store.run_to_yojson run in
  let failure =
    Engine.Error.create ~phase:Engine.Error.Reporting
      ~cause:Engine.Error.Io_failure "write failed"
  in
  let failed_json =
    Engine.Run_store.run_to_yojson
      { run with status = Engine.Run_store.Failed failure }
  in
  let open Yojson.Safe.Util in
  let failure_json = member "failure" failed_json in
  reject "interrupted report with failure object"
    (replace_member "failure" failure_json encoded);
  reject "completed report with failure object"
    (encoded
    |> replace_member "status" (`String "completed")
    |> replace_member "failure" failure_json);
  reject "failed report without failure object"
    (encoded
    |> replace_member "status" (`String "failed")
    |> replace_member "failure" `Null);
  reject "completed report without a failure member"
    (encoded
    |> replace_member "status" (`String "completed")
    |> remove_member "failure");
  check_round_trip "failed status and failure object"
    { run with status = Engine.Run_store.Failed failure }

let test_mutant_sets_are_unique_and_disjoint () =
  let encoded = Engine.Run_store.run_to_yojson (base_run ()) in
  reject "duplicate result mutant" (append_first "mutants" encoded);
  reject "duplicate not-run mutant" (append_first "not_run" encoded);
  let open Yojson.Safe.Util in
  let executed = encoded |> member "mutants" |> index 0 |> member "mutant" in
  reject "executed and not-run overlap"
    (replace_member "not_run" (`List [ executed ]) encoded);
  reject "duplicate expectation mutant ID" (append_first "expectations" encoded)

let test_expectation_ledger_matches_results () =
  let encoded = Engine.Run_store.run_to_yojson (base_run ()) in
  let open Yojson.Safe.Util in
  reject "expected result omitted from ledger"
    (replace_member "expectations" (`List []) encoded);
  reject "ledger reason differs from result reason"
    (map_first "expectations"
       (replace_member "reason" (`String "different reason"))
       encoded);
  reject "ledger status differs from result outcome"
    (map_first "expectations"
       (fun expectation ->
         expectation
         |> replace_member "status" (`String "unfulfilled-killed")
         |> replace_member "detail" `Null)
       encoded);
  reject "ledger expectation has no result expectation"
    (map_first "mutants" (replace_member "expectation" `Null) encoded);
  let unexecuted_evaluated =
    encoded |> member "expectations" |> index 0
    |> replace_member "mutant_id" (`String (String.make 64 'f'))
  in
  reject "evaluated ledger entry has no executed result"
    (replace_member "expectations" (`List [ unexecuted_evaluated ]) encoded);
  let pending = encoded |> member "not_run" |> index 0 in
  let pending_id = pending |> member "full_id" |> to_string in
  let pending_evaluated =
    encoded |> member "expectations" |> index 0
    |> replace_member "mutant_id" (`String pending_id)
  in
  reject "not-run expectation is reported as evaluated"
    (replace_member "expectations" (`List [ pending_evaluated ]) encoded)

let test_baseline_evidence_is_self_consistent () =
  let encoded = Engine.Run_store.run_to_yojson (base_run ()) in
  let map_first_stage transform =
    map_member "test" (map_first "stages" transform) encoded
  in
  reject "stage slowest differs from its runs"
    (map_first_stage (replace_member "slowest_baseline_seconds" (`Float 0.2)));
  reject "baseline stage has no successful run"
    (map_first_stage (replace_member "baseline_runs_seconds" (`List [])));
  reject "overall baseline differs from recorded runs"
    (map_member "test"
       (replace_member "baseline_duration_seconds" (`Float 0.2))
       encoded);
  reject "overall baseline is absent despite recorded runs"
    (map_member "test"
       (replace_member "baseline_duration_seconds" `Null)
       encoded);
  reject "overall baseline exists without recorded runs"
    (map_member "test"
       (fun test ->
         test
         |> replace_member "stages" (`List [])
         |> replace_member "baseline_duration_seconds" (`Float 0.3))
       encoded);
  reject "hit-map names an unrecorded stage"
    (map_member "test"
       (map_member "inventory"
          (map_first "hit_map" (replace_member "test" (`String "unknown"))))
       encoded);
  reject "coverage contradicts hit-map evidence"
    (map_first "mutants"
       (replace_member "coverage" (`String "not-covered"))
       encoded)

let test_derived_mutant_fields_are_validated () =
  let encoded = Engine.Run_store.run_to_yojson (base_run ()) in
  let change_mutant field value result =
    let open Yojson.Safe.Util in
    let mutant = result |> member "mutant" |> replace_member field value in
    replace_member "mutant" mutant result
  in
  reject "short mutant ID contradicts full ID"
    (map_first "mutants"
       (change_mutant "id" (`String (String.make 20 '0')))
       encoded);
  reject "operator family contradicts rule"
    (map_first "mutants"
       (change_mutant "family" (`String "comparison"))
       encoded)

let test_report_redactions_are_opaque () =
  let config =
    {
      Engine.Config.defaults with
      privacy =
        { Engine.Config.defaults.privacy with redactions = [ "private-token" ] };
    }
  in
  let resolved_config = Engine.Config.to_report_yojson config in
  let config_digest =
    Engine.Util.sha256 (Yojson.Safe.to_string ~std:true resolved_config)
  in
  let base = base_run () in
  let run =
    {
      base with
      metadata = { base.metadata with resolved_config; config_digest };
    }
  in
  check_round_trip "opaque report redactions" run;
  let encoded = Engine.Run_store.run_to_yojson run in
  let exposed =
    map_member "resolved_config"
      (map_member "privacy"
         (replace_member "redactions" (`List [ `String "private-token" ])))
      encoded
  in
  let exposed_config = Yojson.Safe.Util.member "resolved_config" exposed in
  let exposed_digest =
    Engine.Util.sha256 (Yojson.Safe.to_string ~std:true exposed_config)
  in
  reject "report exposes a privacy literal"
    (map_member "input_fingerprint"
       (replace_member "config_digest" (`String exposed_digest))
       exposed)

let test_execution_evidence_is_self_consistent () =
  let run = base_run () in
  let attempt : Engine.Run_store.retry_attempt =
    {
      outcome = Core.Outcome.Timeout;
      duration = duration 0.1;
      stages = [];
      stdout = Engine.Run_store.captured "";
      stderr = Engine.Run_store.captured "";
    }
  in
  let retried : Engine.Run_store.mutant_result =
    {
      mutant = expected_mutant;
      outcome = Core.Outcome.Timeout;
      duration = duration 0.2;
      cached = false;
      evidence_origin = Engine.Run_store.Execution;
      stages = [];
      timeout_confirmed = true;
      timeout_retry =
        Some
          { Engine.Run_store.initial_timeout = attempt; serial_retry = attempt };
      expected_reason = Some expected_reason;
      stdout = Engine.Run_store.captured "timeout";
      stderr = Engine.Run_store.captured "";
    }
  in
  let retried_run =
    {
      run with
      results = [ retried; result killed_mutant Core.Outcome.Killed ];
      expectations =
        [
          {
            Engine.Run_store.mutant_id = expected_id;
            reason = expected_reason;
            status = Engine.Run_store.Expectation_unfulfilled_confirmed_timeout;
          };
        ];
    }
  in
  check_round_trip "confirmed timeout evidence" retried_run;
  let encoded = Engine.Run_store.run_to_yojson retried_run in
  reject "retry durations do not sum to the result duration"
    (map_first "mutants"
       (replace_member "duration_seconds" (`Float 0.3))
       encoded);
  reject "serial retry outcome contradicts the final outcome"
    (map_first "mutants"
       (map_member "timeout_retry"
          (map_member "serial_retry" (fun retry ->
               retry
               |> replace_member "outcome" (`String "killed")
               |> replace_member "error" `Null)))
       encoded);
  reject "untruncated output byte count contradicts retained output"
    (map_first "mutants"
       (map_member "stdout" (replace_member "total_bytes" (`Int 99)))
       encoded);
  reject "evidence level contradicts origin"
    (map_first "mutants"
       (map_member "evidence" (replace_member "level" (`String "estimated")))
       encoded);
  reject "estimated marker contradicts origin"
    (map_first "mutants"
       (map_member "evidence" (replace_member "estimated" (`Bool true)))
       encoded);
  reject "checkpoint marker contradicts origin"
    (map_first "mutants"
       (map_member "checkpoint" (replace_member "resumed" (`Bool true)))
       encoded)

let timed_out_mutant =
  mutant ~path:"lib/timed_out.ml" ~source:"true" ~rule_name:"true-to-false@1"
    ~replacement:"false"

(* An unconfirmed timeout exists only in an interrupted run, where cancellation
   skipped the serial confirmation retry. It must round-trip and must never
   count as detected. *)
let test_unconfirmed_timeout_is_not_detected () =
  let base = base_run () in
  let run =
    {
      base with
      results = base.results @ [ result timed_out_mutant Core.Outcome.Timeout ];
    }
  in
  let summary =
    Yojson.Safe.Util.member "summary" (Engine.Run_store.run_to_yojson run)
  in
  let field name = Yojson.Safe.Util.(summary |> member name |> to_int) in
  Alcotest.(check int) "timeout counts the outcome" 1 (field "timeout");
  Alcotest.(check int)
    "the unconfirmed timeout is reported" 1
    (field "unconfirmed_timeouts");
  Alcotest.(check int)
    "detected excludes the unconfirmed timeout" 1 (field "detected");
  check_round_trip "interrupted run with an unconfirmed timeout" run

let () =
  Alcotest.run "Native run report consistency"
    [
      ( "codec",
        [
          Alcotest.test_case "complete and partial witnesses round trip" `Quick
            test_completeness_round_trip;
          Alcotest.test_case "summary is derived" `Quick test_summary_is_derived;
          Alcotest.test_case "unconfirmed timeout is not detected" `Quick
            test_unconfirmed_timeout_is_not_detected;
          Alcotest.test_case "status matches failure" `Quick
            test_status_and_failure_are_consistent;
          Alcotest.test_case "mutant sets are unique and disjoint" `Quick
            test_mutant_sets_are_unique_and_disjoint;
          Alcotest.test_case "expectation ledger matches results" `Quick
            test_expectation_ledger_matches_results;
          Alcotest.test_case "baseline evidence is self-consistent" `Quick
            test_baseline_evidence_is_self_consistent;
          Alcotest.test_case "derived mutant fields are validated" `Quick
            test_derived_mutant_fields_are_validated;
          Alcotest.test_case "report redactions are opaque" `Quick
            test_report_redactions_are_opaque;
          Alcotest.test_case "execution evidence is self-consistent" `Quick
            test_execution_evidence_is_self_consistent;
        ] );
    ]
