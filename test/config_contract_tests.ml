module Engine = Ocaml_mutants_engine

let contains text fragment =
  let text_length = String.length text in
  let fragment_length = String.length fragment in
  let rec search offset =
    if offset + fragment_length > text_length then false
    else if String.sub text offset fragment_length = fragment then true
    else search (offset + 1)
  in
  fragment_length = 0 || search 0

let expect_diagnostics ~name text fragments =
  match Engine.Config.parse ~file:(name ^ ".toml") text with
  | Ok _ -> Alcotest.failf "%s: invalid configuration was accepted" name
  | Error diagnostics ->
      List.iter
        (fun fragment ->
          Alcotest.(check bool)
            (Printf.sprintf "%s includes %S" name fragment)
            true
            (contains diagnostics fragment))
        fragments

let test_nested_unknown_keys () =
  let id = String.make 64 'a' in
  let text =
    Printf.sprintf
      {|version = 1
[[mutation.expect]]
id = %S
reasno = "misspelled"

[[test.stages]]
name = "fast"
command_typo = ["dune", "build"]
|}
      id
  in
  expect_diagnostics ~name:"nested-keys" text
    [
      "mutation.expect.0.reasno: unknown configuration key";
      "mutation.expect.0.reason: missing required key";
      "test.stages.0.command_typo: unknown configuration key";
      "test.stages.0.command: missing required key";
    ]

let test_duplicate_rows () =
  let id = String.make 64 'b' in
  let text =
    Printf.sprintf
      {|version = 1
[[mutation.expect]]
id = %S
reason = "first proof"

[[mutation.expect]]
id = %S
reason = "conflicting proof"

[[test.stages]]
name = "same"
command = ["dune", "build"]

[[test.stages]]
name = "same"
command = ["dune", "runtest"]
|}
      id id
  in
  expect_diagnostics ~name:"duplicates" text
    [
      "mutation.expect.1.id: duplicate expectation mutant ID";
      "test.stages.1.name: duplicate test stage name";
    ]

let test_cache_mode_override () =
  let text =
    {|version = 1
[cache]
mode = "off"
directory = "shared-proof-cache"
|}
  in
  match Engine.Config.parse ~file:"cache-override.toml" text with
  | Error diagnostics -> Alcotest.fail diagnostics
  | Ok config ->
      let unchanged =
        Engine.Config.apply config Engine.Config.empty_overrides
      in
      Alcotest.(check bool)
        "absent override preserves TOML mode" true
        (unchanged.cache.mode = Engine.Config.Off);
      let overridden =
        Engine.Config.apply config
          {
            Engine.Config.empty_overrides with
            cache_mode = Some Engine.Config.On;
          }
      in
      Alcotest.(check bool)
        "explicit CLI policy overrides TOML mode" true
        (overridden.cache.mode = Engine.Config.On);
      Alcotest.(check (option string))
        "mode override preserves configured directory"
        (Some "shared-proof-cache") overridden.cache.directory

(* The starter file written by `init` must never narrow behaviour away from the
   built-in defaults: a freshly initialized workspace behaves exactly like one
   with no configuration at all. *)
let test_example_matches_defaults () =
  match Engine.Config.parse ~file:"example.toml" Engine.Config.example with
  | Error diagnostics -> Alcotest.fail diagnostics
  | Ok config ->
      Alcotest.(check bool)
        "example enables every operator family" true
        (config.mutation.operators = Ocaml_mutants_core.Operator.Family.all);
      Alcotest.(check bool)
        "example selects the balanced profile" true
        (config.mutation.profile = Ocaml_mutants_core.Operator.Profile.Balanced)

let () =
  Alcotest.run "configuration contracts"
    [
      ( "toml-v1",
        [
          Alcotest.test_case "nested row keys are strict" `Quick
            test_nested_unknown_keys;
          Alcotest.test_case "row identities are unique" `Quick
            test_duplicate_rows;
          Alcotest.test_case "cache mode override is typed" `Quick
            test_cache_mode_override;
          Alcotest.test_case "init starter matches the defaults" `Quick
            test_example_matches_defaults;
        ] );
    ]
