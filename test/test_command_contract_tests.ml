module Core = Ocaml_mutants_core
module Engine = Ocaml_mutants_engine

let command arguments =
  match Core.Nonempty_argv.of_list arguments with
  | Ok value -> value
  | Error message -> Alcotest.fail message

let check_resolution label ~managed ~input ~build_dir expected =
  let command = command input in
  Alcotest.(check bool)
    (label ^ " management") managed
    (Engine.Test_command.dune_managed command);
  Alcotest.(check (list string))
    (label ^ " argv") expected
    (Engine.Test_command.resolve command build_dir)

let test_dune_stages_receive_private_build_directories () =
  check_resolution "build" ~managed:true
    ~input:[ "dune"; "build"; "--force"; "@@semantic-oracle" ]
    ~build_dir:"worker-3-stage-1"
    [
      "dune";
      "build";
      "--build-dir";
      "worker-3-stage-1";
      "--force";
      "@@semantic-oracle";
    ];
  check_resolution "runtest" ~managed:true
    ~input:[ "dune"; "runtest"; "--force" ] ~build_dir:"baseline-stage-0"
    [
      "dune";
      "runtest";
      "--build-dir";
      "baseline-stage-0";
      "--force";
    ];
  check_resolution "test" ~managed:true
    ~input:[ "dune"; "test"; "pkg" ] ~build_dir:"worker-1-stage-2"
    [ "dune"; "test"; "--build-dir"; "worker-1-stage-2"; "pkg" ]

let test_explicit_or_unmanaged_commands_are_unchanged () =
  check_resolution "explicit separated build dir" ~managed:false
    ~input:[ "dune"; "build"; "--build-dir"; "shared"; "@@oracle" ]
    ~build_dir:"private"
    [ "dune"; "build"; "--build-dir"; "shared"; "@@oracle" ];
  check_resolution "explicit joined build dir" ~managed:false
    ~input:[ "dune"; "runtest"; "--build-dir=shared"; "--force" ]
    ~build_dir:"private"
    [ "dune"; "runtest"; "--build-dir=shared"; "--force" ];
  check_resolution "unmanaged dune command" ~managed:false
    ~input:[ "dune"; "exec"; "tool" ] ~build_dir:"private"
    [ "dune"; "exec"; "tool" ];
  check_resolution "non-dune command" ~managed:false
    ~input:[ "make"; "test" ] ~build_dir:"private" [ "make"; "test" ]

let () =
  Alcotest.run "Test command isolation"
    [
      ( "resolution",
        [
          Alcotest.test_case "managed Dune stages are isolated" `Quick
            test_dune_stages_receive_private_build_directories;
          Alcotest.test_case "explicit and unmanaged commands are preserved"
            `Quick test_explicit_or_unmanaged_commands_are_unchanged;
        ] );
    ]
