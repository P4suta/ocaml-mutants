module Engine = Ocaml_mutants_engine

module Fake_process = struct
  type disposition = Succeeded | Failed | Cancelled

  type result = {
    disposition : disposition;
    stdout : string;
    stderr : string;
    status_name : string;
  }

  type invocation = {
    cwd : string;
    env : (string * string option) list;
    argv : string list;
    shared_cancel : bool;
  }

  let script : result list ref = ref []
  let invocations : invocation list ref = ref []
  let expected_cancel : Engine.Cancel.t option ref = ref None

  let success ?(stdout = "") () =
    { disposition = Succeeded; stdout; stderr = ""; status_name = "exited 0" }

  let failed ?(stderr = "command failed") () =
    { disposition = Failed; stdout = ""; stderr; status_name = "exited 7" }

  let cancelled_result =
    {
      disposition = Cancelled;
      stdout = "";
      stderr = "cancelled";
      status_name = "cancelled";
    }

  let reset ~cancel responses =
    script := responses;
    invocations := [];
    expected_cancel := Some cancel

  let run ~cancel ~cwd ~env argv =
    let shared_cancel =
      match !expected_cancel with
      | Some expected -> expected == cancel
      | None -> false
    in
    invocations := { cwd; env; argv; shared_cancel } :: !invocations;
    match !script with
    | [] -> Alcotest.fail "Dune_adapter.Make issued an unexpected command"
    | response :: rest ->
        script := rest;
        response

  let cancelled result = result.disposition = Cancelled
  let succeeded result = result.disposition = Succeeded
  let stdout result = result.stdout
  let stderr result = result.stderr
  let status result = result.status_name

  let finish () =
    Alcotest.(check int)
      "every scripted response consumed" 0 (List.length !script);
    List.rev !invocations
end

module Subject = Engine.Dune_adapter.Make (Fake_process)

let workspace_command =
  [ "dune"; "describe"; "workspace"; "--format"; "csexp"; "--lang"; "0.1" ]

let tests_command = [ "dune"; "describe"; "tests"; "--format"; "csexp" ]

let build_command =
  [
    "dune";
    "build";
    "--build-dir";
    ".analysis";
    "@all";
    "lib/a.cmt";
    "bin/b.cmt";
  ]

let invocation_testable =
  let pp formatter (invocation : Fake_process.invocation) =
    Format.fprintf formatter "cwd=%S env=%d argv=[%s] cancel=%b" invocation.cwd
      (List.length invocation.env)
      (String.concat ";" invocation.argv)
      invocation.shared_cancel
  in
  Alcotest.testable pp ( = )

let invoke response action =
  let cancel = Engine.Cancel.create () in
  Fake_process.reset ~cancel [ response ];
  let result = action cancel in
  let invocations = Fake_process.finish () in
  Alcotest.(check int)
    "exactly one subprocess acquired" 1 (List.length invocations);
  (result, List.hd invocations)

let check_invocation ~argv ~env (invocation : Fake_process.invocation) =
  let expected : Fake_process.invocation =
    { cwd = "workspace"; env; argv; shared_cancel = true }
  in
  Alcotest.check invocation_testable "subprocess request" expected invocation

let expect_error ~phase ~cause ~message ~context = function
  | Ok _ -> Alcotest.fail "expected Dune adapter error"
  | Error error ->
      Alcotest.(check string)
        "error phase" phase
        (Engine.Error.phase_name (Engine.Error.phase error));
      Alcotest.(check string)
        "error cause" cause
        (Engine.Error.cause_name (Engine.Error.cause error));
      Alcotest.(check string)
        "error message" message
        (Engine.Error.message error);
      Alcotest.(check (list (pair string string)))
        "error context" context
        (Engine.Error.context error)

let test_describe_workspace_contract () =
  let success, invocation =
    invoke (Fake_process.success ~stdout:"()" ()) (fun cancel ->
        Subject.describe ~cancel ~root:"workspace")
  in
  check_invocation ~argv:workspace_command ~env:[] invocation;
  (match success with
  | Ok workspace ->
      Alcotest.(check int)
        "empty workspace decoded" 0
        (List.length workspace.source_files)
  | Error error -> Alcotest.failf "unexpected error: %a" Engine.Error.pp error);
  let failure, invocation =
    invoke (Fake_process.failed ~stderr:"describe failed" ()) (fun cancel ->
        Subject.describe ~cancel ~root:"workspace")
  in
  check_invocation ~argv:workspace_command ~env:[] invocation;
  expect_error ~phase:"dune" ~cause:"process-failure"
    ~message:"dune describe workspace failed:\ndescribe failed"
    ~context:[ ("status", "exited 7") ]
    failure;
  let cancellation, invocation =
    invoke Fake_process.cancelled_result (fun cancel ->
        Subject.describe ~cancel ~root:"workspace")
  in
  check_invocation ~argv:workspace_command ~env:[] invocation;
  expect_error ~phase:"dune" ~cause:"interrupted"
    ~message:"dune describe was interrupted" ~context:[] cancellation

let test_describe_tests_contract () =
  let success, invocation =
    invoke (Fake_process.success ~stdout:"()" ()) (fun cancel ->
        Subject.describe_tests ~cancel ~root:"workspace")
  in
  check_invocation ~argv:tests_command ~env:[] invocation;
  (match success with
  | Ok tests ->
      Alcotest.(check int) "empty test list decoded" 0 (List.length tests)
  | Error error -> Alcotest.failf "unexpected error: %a" Engine.Error.pp error);
  let failure, invocation =
    invoke (Fake_process.failed ~stderr:"tests failed" ()) (fun cancel ->
        Subject.describe_tests ~cancel ~root:"workspace")
  in
  check_invocation ~argv:tests_command ~env:[] invocation;
  expect_error ~phase:"dune" ~cause:"process-failure"
    ~message:"dune describe tests failed:\ntests failed"
    ~context:[ ("status", "exited 7") ]
    failure;
  let cancellation, invocation =
    invoke Fake_process.cancelled_result (fun cancel ->
        Subject.describe_tests ~cancel ~root:"workspace")
  in
  check_invocation ~argv:tests_command ~env:[] invocation;
  expect_error ~phase:"dune" ~cause:"interrupted"
    ~message:"dune test description was interrupted" ~context:[] cancellation

let build cancel =
  Subject.build_analysis ~cancel ~root:"workspace" ~build_dir:".analysis"
    ~cmt_targets:[ "lib/a.cmt"; "bin/b.cmt" ]

let test_build_analysis_contract () =
  let success, invocation =
    invoke (Fake_process.success ~stdout:"built" ()) build
  in
  check_invocation ~argv:build_command
    ~env:[ ("DUNE_CACHE", Some "disabled") ]
    invocation;
  (match success with
  | Ok result ->
      Alcotest.(check string) "opaque result retained" "built" result.stdout
  | Error error -> Alcotest.failf "unexpected error: %a" Engine.Error.pp error);
  let failure, invocation =
    invoke (Fake_process.failed ~stderr:"build failed" ()) build
  in
  check_invocation ~argv:build_command
    ~env:[ ("DUNE_CACHE", Some "disabled") ]
    invocation;
  expect_error ~phase:"analysis" ~cause:"process-failure"
    ~message:"analysis build failed:\nbuild failed"
    ~context:[ ("status", "exited 7") ]
    failure;
  let cancellation, invocation = invoke Fake_process.cancelled_result build in
  check_invocation ~argv:build_command
    ~env:[ ("DUNE_CACHE", Some "disabled") ]
    invocation;
  expect_error ~phase:"analysis" ~cause:"interrupted"
    ~message:"analysis build was interrupted" ~context:[] cancellation

let () =
  Alcotest.run "Dune adapter contract"
    [
      ( "process algebra",
        [
          Alcotest.test_case "describe workspace" `Quick
            test_describe_workspace_contract;
          Alcotest.test_case "describe tests" `Quick
            test_describe_tests_contract;
          Alcotest.test_case "build analysis" `Quick
            test_build_analysis_contract;
        ] );
    ]
