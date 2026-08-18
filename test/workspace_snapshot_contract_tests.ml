module Snapshot = Ocaml_mutants_engine.Workspace_snapshot

module Recording_temporary_directory = struct
  type request = {
    temp_dir : string;
    permissions : int;
    prefix : string;
    suffix : string;
  }

  let requests = ref []

  let create_exclusive ~temp_dir ~permissions ~prefix ~suffix =
    requests := { temp_dir; permissions; prefix; suffix } :: !requests;
    Ok (Filename.concat temp_dir (prefix ^ "owned" ^ suffix))
end

let test_snapshot_root_allocation_is_one_exclusive_directory_create () =
  Recording_temporary_directory.requests := [];
  let temp_dir = Filename.concat "contract" "temporary-root" in
  let allocated =
    Snapshot.For_testing.allocate_temporary_root
      (module Recording_temporary_directory)
      ~temp_dir
  in
  Alcotest.(check (result string string))
    "factory result is returned without a placeholder transition"
    (Ok (Filename.concat temp_dir "ocaml-mutants-snapshot-owned"))
    allocated;
  match !Recording_temporary_directory.requests with
  | [ request ] ->
      Alcotest.(check string) "temporary parent" temp_dir request.temp_dir;
      Alcotest.(check int)
        "private directory permissions" 0o700 request.permissions;
      Alcotest.(check string)
        "snapshot prefix" "ocaml-mutants-snapshot-" request.prefix;
      Alcotest.(check string) "no suffix transition" "" request.suffix
  | requests ->
      Alcotest.failf "expected one exclusive directory create, observed %d"
        (List.length requests)

let test_exclusions_are_component_based_and_platform_normalized () =
  let skip = Snapshot.For_testing.default_skip_for_platform in
  Alcotest.(check bool)
    "nested exact component is excluded" true
    (skip ~case_sensitive:true "fixtures/basic/_build/default/generated.ml");
  Alcotest.(check bool)
    "suffix lookalike is retained" false
    (skip ~case_sensitive:true "fixtures/basic/not_build/generated.ml");
  Alcotest.(check bool)
    "POSIX comparison remains case-sensitive" false
    (skip ~case_sensitive:true "fixtures/basic/_BUILD/default/generated.ml");
  Alcotest.(check bool)
    "Windows comparison folds the component" true
    (skip ~case_sensitive:false "fixtures/basic/_BUILD/default/generated.ml")

let () =
  Alcotest.run "workspace snapshot contracts"
    [
      ( "allocation",
        [
          Alcotest.test_case "exclusive directory create only" `Quick
            test_snapshot_root_allocation_is_one_exclusive_directory_create;
        ] );
      ( "exclusions",
        [
          Alcotest.test_case "component and platform semantics" `Quick
            test_exclusions_are_component_based_and_platform_normalized;
        ] );
    ]
