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

let test_v2_canonical_round_trip () =
  let encoded = Engine.Config.to_toml Engine.Config.defaults in
  Alcotest.(check bool)
    "canonical output declares v2" true
    (contains encoded "version = 2");
  match Engine.Config.parse_with_metadata ~file:"canonical-v2.toml" encoded with
  | Error diagnostics -> Alcotest.fail diagnostics
  | Ok loaded ->
      Alcotest.(check bool)
        "origin is v2" true
        (loaded.origin = Engine.Config.Version_2);
      Alcotest.(check (list string))
        "v2 has no migration warning" [] loaded.warnings;
      Alcotest.(check string)
        "canonical round trip" encoded
        (Engine.Config.to_toml loaded.config)

let test_v2_cache_mode_round_trip () =
  let config =
    {
      Engine.Config.defaults with
      cache =
        {
          Engine.Config.defaults.cache with
          mode = Engine.Config.On;
          historical_reuse = Engine.Config.Reuse_off;
        };
    }
  in
  let encoded = Engine.Config.to_toml config in
  match Engine.Config.parse ~file:"cache-mode-v2.toml" encoded with
  | Error diagnostics -> Alcotest.fail diagnostics
  | Ok loaded ->
      Alcotest.(check bool)
        "explicit storage mode is preserved" true
        (loaded.cache.mode = Engine.Config.On);
      Alcotest.(check bool)
        "historical reuse remains independently disabled" true
        (loaded.cache.historical_reuse = Engine.Config.Reuse_off)

let test_v1_normalizes_with_warning () =
  let legacy = {|version = 1
[execution]
jobs = 2

[cache]
mode = "off"
|} in
  match Engine.Config.parse_with_metadata ~file:"legacy-v1.toml" legacy with
  | Error diagnostics -> Alcotest.fail diagnostics
  | Ok loaded ->
      Alcotest.(check bool)
        "origin is v1" true
        (loaded.origin = Engine.Config.Version_1);
      Alcotest.(check bool)
        "migration warning is present" true (loaded.warnings <> []);
      Alcotest.(check bool)
        "normalized model is v2" true
        (loaded.config.version = 2);
      Alcotest.(check bool)
        "migration output declares v2" true
        (contains (Engine.Config.to_toml loaded.config) "version = 2")

let test_privacy_literals_are_nonempty () =
  expect_diagnostics ~name:"empty-redaction"
    {|version = 2
[privacy]
redactions = [""]
|}
    [ "privacy.redactions: array entries must be non-empty strings" ]

let test_report_config_hides_privacy_literals () =
  let config =
    {
      Engine.Config.defaults with
      privacy =
        {
          Engine.Config.defaults.privacy with
          redactions = [ "private-token"; "second-secret" ];
        };
    }
  in
  let encoded = Engine.Config.to_report_yojson config in
  let redactions =
    Yojson.Safe.Util.(
      encoded |> member "privacy" |> member "redactions" |> to_list
      |> List.map to_string)
  in
  Alcotest.(check (list string))
    "report retains only opaque placeholders"
    [
      Engine.Config.report_redaction_placeholder;
      Engine.Config.report_redaction_placeholder;
    ]
    redactions;
  let rendered = Yojson.Safe.to_string encoded in
  Alcotest.(check bool)
    "report omits the first literal" false
    (contains rendered "private-token");
  Alcotest.(check bool)
    "report omits the second literal" false
    (contains rendered "second-secret")

let () =
  Alcotest.run "configuration contracts"
    [
      ( "toml-v1-v2",
        [
          Alcotest.test_case "nested row keys are strict" `Quick
            test_nested_unknown_keys;
          Alcotest.test_case "row identities are unique" `Quick
            test_duplicate_rows;
          Alcotest.test_case "cache mode override is typed" `Quick
            test_cache_mode_override;
          Alcotest.test_case "init starter matches the defaults" `Quick
            test_example_matches_defaults;
          Alcotest.test_case "canonical v2 round trip" `Quick
            test_v2_canonical_round_trip;
          Alcotest.test_case "v2 cache mode round trip" `Quick
            test_v2_cache_mode_round_trip;
          Alcotest.test_case "v1 normalization warning" `Quick
            test_v1_normalizes_with_warning;
          Alcotest.test_case "privacy redactions are nonempty" `Quick
            test_privacy_literals_are_nonempty;
          Alcotest.test_case "report config hides privacy literals" `Quick
            test_report_config_hides_privacy_literals;
        ] );
    ]
