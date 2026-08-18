module Engine = Ocaml_mutants_engine
module Core = Ocaml_mutants_core

module Fake_process = struct
  type result = {
    disposition : Engine.Baseline.process_disposition;
    duration_seconds : float;
    stdout : string;
    stderr : string;
    status : string;
  }

  type invocation = {
    cancel : Engine.Cancel.t;
    cwd : string;
    env : (string * string option) list;
    command : string list;
  }

  let responses : result list ref = ref []
  let invocations : invocation list ref = ref []

  let reset values =
    responses := values;
    invocations := []

  let run ~cancel ~cwd ~env command =
    invocations := { cancel; cwd; env; command } :: !invocations;
    match !responses with
    | result :: rest ->
        responses := rest;
        result
    | [] -> Alcotest.fail "baseline executor made an unexpected process call"

  let disposition result = result.disposition
  let duration_seconds result = result.duration_seconds
  let stdout result = result.stdout
  let stderr result = result.stderr
  let status result = result.status

  let succeeded duration_seconds =
    {
      disposition = Engine.Baseline.Succeeded;
      duration_seconds;
      stdout = "";
      stderr = "";
      status = "exited 0";
    }

  let failed =
    {
      disposition = Engine.Baseline.Failed;
      duration_seconds = 0.25;
      stdout = "partial stdout";
      stderr = "failure stderr";
      status = "exited 1";
    }

  let cancelled =
    {
      disposition = Engine.Baseline.Cancelled;
      duration_seconds = 0.25;
      stdout = "";
      stderr = "cancelled";
      status = "cancelled";
    }
end

module Subject = Engine.Baseline.Make (Fake_process)

let get_ok = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let argv values = get_ok (Core.Nonempty_argv.of_list values)
let positive value = get_ok (Core.Positive_int.of_int value)
let stage name command : Engine.Config.stage = { name; command = argv command }

let config ~runs stages =
  let first_stage : Engine.Config.stage = List.hd stages in
  {
    Engine.Config.defaults with
    test =
      {
        Engine.Config.defaults.test with
        command = first_stage.command;
        stages;
        baseline_runs = positive runs;
      };
  }

let run config responses =
  let cancel = Engine.Cancel.create () in
  Fake_process.reset responses;
  (cancel, Subject.run ~cancel ~root:"workspace" ~config ~build_dir:"baseline")

let seconds duration = Core.Duration.to_seconds duration

let check_float label expected actual =
  Alcotest.(check (float 0.000_001)) label expected actual

let check_runs label expected (stage : Engine.Run_store.baseline_stage) =
  Alcotest.(check (list (float 0.000_001)))
    label expected
    (List.map seconds stage.runs)

let stopped = function
  | Engine.Baseline.Completed _ ->
      Alcotest.fail "baseline unexpectedly completed"
  | Engine.Baseline.Incomplete incomplete -> incomplete

let test_later_stage_failure_retains_completed_stages () =
  let config =
    config ~runs:2 [ stage "fast" [ "fast" ]; stage "full" [ "full" ] ]
  in
  let _, outcome =
    run config
      [
        Fake_process.succeeded 0.1;
        Fake_process.succeeded 0.4;
        Fake_process.failed;
      ]
  in
  let stopped = stopped outcome in
  let completed = Engine.Baseline.completed_stages stopped.evidence in
  Alcotest.(check int) "one completed stage" 1 (List.length completed);
  let fast = List.hd completed in
  Alcotest.(check string) "completed stage" "fast" fast.name;
  check_runs "all fast repetitions" [ 0.1; 0.4 ] fast;
  Alcotest.(check bool)
    "no successful repetition in failing stage" true
    (Option.is_none (Engine.Baseline.partial_stage stopped.evidence));
  check_float "slowest retained measurement" 0.4
    (Engine.Baseline.slowest stopped.evidence |> Option.get |> seconds);
  Alcotest.(check bool)
    "ordinary failure classification" false
    (Engine.Baseline.was_interrupted stopped)

let test_later_repetition_failure_retains_partial_stage () =
  let config = config ~runs:3 [ stage "fast" [ "fast" ] ] in
  let _, outcome =
    run config
      [
        Fake_process.succeeded 0.2;
        Fake_process.succeeded 0.6;
        Fake_process.failed;
      ]
  in
  let stopped = stopped outcome in
  Alcotest.(check int)
    "no fully completed stage" 0
    (List.length (Engine.Baseline.completed_stages stopped.evidence));
  let partial =
    match Engine.Baseline.partial_stage stopped.evidence with
    | Some stage -> stage
    | None -> Alcotest.fail "completed repetitions were discarded"
  in
  Alcotest.(check string) "partial stage identity" "fast" partial.name;
  check_runs "successful repetitions" [ 0.2; 0.6 ] partial;
  check_float "partial stage slowest" 0.6 (seconds partial.slowest);
  check_float "overall slowest" 0.6
    (Engine.Baseline.slowest stopped.evidence |> Option.get |> seconds)

let test_failure_is_fail_fast_and_ordered () =
  let config =
    config ~runs:2
      [
        stage "first" [ "first" ];
        stage "second" [ "second" ];
        stage "third" [ "third" ];
      ]
  in
  let _, outcome =
    run config
      [
        Fake_process.succeeded 0.1;
        Fake_process.succeeded 0.2;
        Fake_process.succeeded 0.3;
        Fake_process.failed;
        Fake_process.succeeded 0.4;
      ]
  in
  ignore (stopped outcome);
  let commands =
    List.rev !Fake_process.invocations
    |> List.map (fun invocation -> invocation.Fake_process.command)
  in
  Alcotest.(check (list (list string)))
    "only commands through the failing repetition"
    [ [ "first" ]; [ "first" ]; [ "second" ]; [ "second" ] ]
    commands;
  Alcotest.(check int)
    "later response unused" 1
    (List.length !Fake_process.responses)

let test_cancellation_is_typed_and_retains_evidence () =
  let config =
    config ~runs:3 [ stage "fast" [ "fast" ]; stage "full" [ "full" ] ]
  in
  let cancel, outcome =
    run config [ Fake_process.succeeded 0.3; Fake_process.cancelled ]
  in
  let stopped = stopped outcome in
  Alcotest.(check bool)
    "typed cancellation" true
    (Engine.Baseline.was_interrupted stopped);
  Alcotest.(check bool)
    "shared cancellation token" true
    (List.for_all
       (fun invocation -> invocation.Fake_process.cancel == cancel)
       !Fake_process.invocations);
  Alcotest.(check int)
    "cancel stops remaining repetitions and stages" 2
    (List.length !Fake_process.invocations);
  let partial = Engine.Baseline.partial_stage stopped.evidence |> Option.get in
  check_runs "pre-cancellation evidence" [ 0.3 ] partial;
  Alcotest.(check bool)
    "error cause is interruption" true
    (Engine.Error.cause stopped.error = Engine.Error.Interrupted_by_user)

let () =
  Alcotest.run "baseline evidence"
    [
      ( "contract",
        [
          Alcotest.test_case "later stage failure retains prior stages" `Quick
            test_later_stage_failure_retains_completed_stages;
          Alcotest.test_case "later repetition retains partial stage" `Quick
            test_later_repetition_failure_retains_partial_stage;
          Alcotest.test_case "commands are ordered and fail-fast" `Quick
            test_failure_is_fail_fast_and_ordered;
          Alcotest.test_case "cancellation is typed" `Quick
            test_cancellation_is_typed_and_retains_evidence;
        ] );
    ]
