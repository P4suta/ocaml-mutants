module Core = Ocaml_mutants_core
module Engine = Ocaml_mutants_engine

let get_ok = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let contains ~needle value =
  let needle_length = String.length needle in
  let value_length = String.length value in
  let rec search offset =
    offset + needle_length <= value_length
    && (String.equal (String.sub value offset needle_length) needle
       || search (offset + 1))
  in
  needle_length = 0 || search 0

let mutant replacement =
  let range =
    get_ok
      (Core.Source_range.make ~start_byte:0 ~end_byte:4 ~start_line:1
         ~start_column:0 ~end_line:1 ~end_column:4)
  in
  let rule = get_ok (Core.Operator.Rule.of_stable_name "true-to-false@1") in
  let unchecked =
    match
      Core.Mutant.unchecked ~path:"lib/example.ml" ~range ~rule ~replacement
    with
    | Ok value -> value
    | Error error -> Alcotest.failf "%a" Core.Mutant.pp_validation_error error
  in
  match
    Core.Mutant.validate ~source:(Core.Source.of_string "true") unchecked
  with
  | Ok value -> value
  | Error error -> Alcotest.failf "%a" Core.Mutant.pp_validation_error error

let result ~expected_reason mutant outcome : Engine.Run_store.mutant_result =
  {
    mutant;
    outcome;
    duration = get_ok (Core.Duration.of_seconds 0.1);
    cached = false;
    evidence_origin = Engine.Run_store.Execution;
    stages = [];
    timeout_confirmed = false;
    timeout_retry = None;
    expected_reason;
    stdout = Engine.Run_store.captured "";
    stderr = Engine.Run_store.captured "";
  }

let check_contains output expected =
  Alcotest.(check bool) expected true (contains ~needle:expected output)

let test_expectation_policy_is_visible () =
  let expected_survivor = mutant "false" in
  let unfulfilled = mutant "(false)" in
  let expected_survivor_id =
    Core.Mutant.Id.full (Core.Mutant.id expected_survivor)
  in
  let unfulfilled_id = Core.Mutant.Id.full (Core.Mutant.id unfulfilled) in
  let run_id =
    get_ok
      (Core.Run_id.create ~started_at:"20260101T000000Z"
         ~nonce:"terminal-policy")
  in
  let command = get_ok (Core.Nonempty_argv.of_list [ "dune"; "runtest" ]) in
  let run : Engine.Run_store.run =
    {
      metadata =
        {
          id = run_id;
          started_at = "2026-01-01T00:00:00Z";
          finished_at = "2026-01-01T00:00:01Z";
          workspace_digest = String.make 64 'a';
          toolchain = "test";
          profile = Core.Operator.Profile.Balanced;
          selection = "all";
          test_command = command;
          baseline_duration = None;
          baseline_stages = [];
          hit_map = [];
          timeout = None;
          cache_mode = "off";
          execution_mode = "strict";
          historical_reuse = "off";
          cache_key = "unavailable";
          resolved_config = Engine.Config.to_yojson Engine.Config.defaults;
          input_fingerprint = "unavailable";
          config_digest =
            Engine.Util.sha256
              (Yojson.Safe.to_string ~std:true
                 (Engine.Config.to_yojson Engine.Config.defaults));
        };
      status = Engine.Run_store.Completed;
      results =
        [
          result ~expected_reason:(Some "equivalent") expected_survivor
            Core.Outcome.Survived;
          result ~expected_reason:(Some "must remain equivalent") unfulfilled
            Core.Outcome.Killed;
        ];
      completeness = Engine.Run_store.Complete;
      expectations =
        [
          {
            Engine.Run_store.mutant_id = expected_survivor_id;
            reason = "equivalent";
            status = Engine.Run_store.Expectation_fulfilled;
          };
          {
            Engine.Run_store.mutant_id = unfulfilled_id;
            reason = "must remain equivalent";
            status = Engine.Run_store.Expectation_unfulfilled_killed;
          };
          {
            Engine.Run_store.mutant_id = String.make 64 'f';
            reason = "removed mutant";
            status = Engine.Run_store.Expectation_stale;
          };
        ];
      skipped = [];
      warnings =
        [ { Engine.Run_store.code = "stale-expectation"; message = "removed" } ];
    }
  in
  let output =
    Format.asprintf "%a" (Engine.Report.print_run ~color:false) run
  in
  check_contains output "EXPECTED SURVIVOR";
  check_contains output "UNFULFILLED EXPECTATION";
  check_contains output "0 unexpected survivor";
  check_contains output "1 expected survivor";
  check_contains output "Expectations:";
  check_contains output "UNFULFILLED-KILLED";
  check_contains output "STALE";
  check_contains output "Warnings:";
  check_contains output "STALE-EXPECTATION"

let () =
  Alcotest.run "Terminal report contract"
    [
      ( "expectations",
        [
          Alcotest.test_case "policy is visible" `Quick
            test_expectation_policy_is_visible;
        ] );
    ]
