module Engine = Ocaml_mutants_engine

module Fake_process = struct
  type disposition = Succeeded | Failed | Cancelled
  type result = { disposition : disposition; stdout : string; stderr : string }

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
    { disposition = Succeeded; stdout; stderr = "" }

  let failed ?(stderr = "command failed") () =
    { disposition = Failed; stdout = ""; stderr }

  let cancelled_result =
    { disposition = Cancelled; stdout = ""; stderr = "cancelled" }

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
    | [] -> Alcotest.fail "Changed.Make issued an unexpected Git command"
    | response :: rest ->
        script := rest;
        response

  let cancelled result = result.disposition = Cancelled
  let succeeded result = result.disposition = Succeeded
  let stdout result = result.stdout
  let stderr result = result.stderr
  let observed () = List.rev !invocations

  let assert_consumed () =
    Alcotest.(check int)
      "every scripted response consumed" 0 (List.length !script)
end

module Subject = Engine.Changed.Make (Fake_process)

let upstream =
  [ "git"; "rev-parse"; "--abbrev-ref"; "--symbolic-full-name"; "@{upstream}" ]

let merge_base upstream_name = [ "git"; "merge-base"; "HEAD"; upstream_name ]

let diff base =
  [ "git"; "diff"; "--name-only"; "--diff-filter=ACMR"; base; "--" ]

let untracked = [ "git"; "ls-files"; "--others"; "--exclude-standard" ]
let argv_testable = Alcotest.list Alcotest.string
let argv_trace_testable = Alcotest.list argv_testable

let invoke ?from responses =
  let cancel = Engine.Cancel.create () in
  Fake_process.reset ~cancel responses;
  let result = Subject.files ~cancel ~root:"workspace" ~from in
  let invocations = Fake_process.observed () in
  Fake_process.assert_consumed ();
  List.iter
    (fun (invocation : Fake_process.invocation) ->
      Alcotest.(check string) "workspace cwd" "workspace" invocation.cwd;
      Alcotest.(check (list (pair string (option string))))
        "Git environment unchanged" [] invocation.env;
      Alcotest.(check bool)
        "one cancellation token propagated" true invocation.shared_cancel)
    invocations;
  (result, List.map (fun invocation -> invocation.Fake_process.argv) invocations)

let expect_files expected = function
  | Ok actual -> Alcotest.(check (list string)) "selected files" expected actual
  | Error error -> Alcotest.failf "unexpected error: %a" Engine.Error.pp error

let expect_error ~phase ~cause ~message = function
  | Ok files ->
      Alcotest.failf "expected an error, got %d selected files"
        (List.length files)
  | Error error ->
      Alcotest.(check string)
        "error phase" phase
        (Engine.Error.phase_name (Engine.Error.phase error));
      Alcotest.(check string)
        "error cause" cause
        (Engine.Error.cause_name (Engine.Error.cause error));
      Alcotest.(check string)
        "error message" message
        (Engine.Error.message error)

let test_auto_base_success () =
  let result, commands =
    invoke
      [
        Fake_process.success ~stdout:"origin/main\n" ();
        Fake_process.success ~stdout:"abc123\n" ();
        Fake_process.success ~stdout:"lib\\b.ml\n lib/a.ml\n\n" ();
        Fake_process.success ~stdout:"new.ml\nlib/a.ml\n" ();
      ]
  in
  expect_files [ "lib/a.ml"; "lib/b.ml"; "new.ml" ] result;
  Alcotest.check argv_trace_testable "Git command order"
    [ upstream; merge_base "origin/main"; diff "abc123"; untracked ]
    commands

let test_explicit_base_skips_discovery () =
  let result, commands =
    invoke ~from:"release-base"
      [
        Fake_process.success ~stdout:"changed.ml\n" (); Fake_process.success ();
      ]
  in
  expect_files [ "changed.ml" ] result;
  Alcotest.check argv_trace_testable "only selection commands run"
    [ diff "release-base"; untracked ]
    commands

let test_failures () =
  let cases =
    [
      ( "upstream",
        [ Fake_process.failed ~stderr:"no upstream" () ],
        [ upstream ],
        "cli",
        "invalid-input",
        "--changed could not find an upstream branch; use --changed-from REV" );
      ( "merge-base",
        [
          Fake_process.success ~stdout:"origin/main\n" ();
          Fake_process.failed ~stderr:"bad merge" ();
        ],
        [ upstream; merge_base "origin/main" ],
        "analysis",
        "io-failure",
        "git merge-base failed: bad merge" );
      ( "diff",
        [
          Fake_process.success ~stdout:"origin/main\n" ();
          Fake_process.success ~stdout:"abc123\n" ();
          Fake_process.failed ~stderr:"bad diff" ();
        ],
        [ upstream; merge_base "origin/main"; diff "abc123" ],
        "analysis",
        "io-failure",
        "git diff failed: bad diff" );
      ( "untracked",
        [
          Fake_process.success ~stdout:"origin/main\n" ();
          Fake_process.success ~stdout:"abc123\n" ();
          Fake_process.success ~stdout:"changed.ml\n" ();
          Fake_process.failed ~stderr:"bad index" ();
        ],
        [ upstream; merge_base "origin/main"; diff "abc123"; untracked ],
        "analysis",
        "io-failure",
        "git ls-files failed: bad index" );
    ]
  in
  List.iter
    (fun (name, responses, expected_commands, phase, cause, message) ->
      let result, commands = invoke responses in
      expect_error ~phase ~cause ~message result;
      Alcotest.check argv_trace_testable
        (name ^ " stops at failure")
        expected_commands commands)
    cases

let test_cancellation_at_every_command () =
  let ok_upstream = Fake_process.success ~stdout:"origin/main\n" () in
  let ok_merge = Fake_process.success ~stdout:"abc123\n" () in
  let ok_diff = Fake_process.success ~stdout:"changed.ml\n" () in
  let cases =
    [
      ("upstream", [ Fake_process.cancelled_result ], [ upstream ]);
      ( "merge-base",
        [ ok_upstream; Fake_process.cancelled_result ],
        [ upstream; merge_base "origin/main" ] );
      ( "diff",
        [ ok_upstream; ok_merge; Fake_process.cancelled_result ],
        [ upstream; merge_base "origin/main"; diff "abc123" ] );
      ( "untracked",
        [ ok_upstream; ok_merge; ok_diff; Fake_process.cancelled_result ],
        [ upstream; merge_base "origin/main"; diff "abc123"; untracked ] );
    ]
  in
  List.iter
    (fun (name, responses, expected_commands) ->
      let result, commands = invoke responses in
      expect_error ~phase:"analysis" ~cause:"interrupted"
        ~message:"Git selection was interrupted" result;
      Alcotest.check argv_trace_testable
        (name ^ " cancellation stops work")
        expected_commands commands)
    cases

let () =
  Alcotest.run "Changed contract"
    [
      ( "git",
        [
          Alcotest.test_case "auto base success" `Quick test_auto_base_success;
          Alcotest.test_case "explicit base" `Quick
            test_explicit_base_skips_discovery;
          Alcotest.test_case "all subprocess failures" `Quick test_failures;
          Alcotest.test_case "cancellation at every command" `Quick
            test_cancellation_at_every_command;
        ] );
    ]
