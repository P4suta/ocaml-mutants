open Ocaml_mutants_engine

let fail format =
  Printf.ksprintf
    (fun message ->
      prerr_endline message;
      exit 1)
    format

let contains_substring ~needle value =
  let rec search index =
    if index + String.length needle > String.length value then false
    else if String.sub value index (String.length needle) = needle then true
    else search (index + 1)
  in
  search 0

(** Source-stable behavioral golden for [fixtures/basic]. Unlike the
    self-catalog hash, this changes only when the fixed fixture or mutation
    semantics change. Review every update as an operator-contract change. The
    Balanced profile excludes the if-branch (Strong) and sequence-deletion (All)
    families, so their fixture mutants are absent here by design. *)
let basic_balanced_full_ids =
  [
    "0d4cefec4a095f4c547696732d9bed9e907fef71037ea6f2b607cf129eef90ae";
    "43f725b86a8a602c74f0673d9cd9fee825833d6ef974ec7300e130ffc8e067af";
    "136eb70a98f996965208d22863066cd7ccaaa9bf26f5fe4e7fc625472cf0ea7e";
    "45f9e10d31014864774358fc4c9375ead5799c2f52fa6282808b3830736690c6";
    "bcccc3830e3fe3445a18cb51e4dd895b2cff899ed39ad866eccc0950180f76d4";
    "64ad07323710b330bb715fd81ee44197d62ea2338f3699dd11995c3ff9fbcfac";
    "ef54c23f64ab89e31a40452cb26884990cfabd86b6b5dd29effcc1e6898f6094";
    "ec18dd059b23c0b5a53ad5543377fc89d3436a5c2068ff9ebb5560f45405001d";
    "a7b0c30dd522ff6453ed41bddf6f409046ccd2ecbc574583705b46ec713fb6ed";
    "8560621fa65d04f651f59ae8854c603cbf9c86883ad22cbcbfe0f88449adf813";
    "0ee20e0b8b059edeef393e86a3d571f703c65d9ae092b722413eeccca153fdab";
    "3ff17d4b206419db2b7861fc7d95d8446922575b230a9f5c7468bb3998646e41";
    "53eaa6c638c25a81d30e199df65978e4bf51b52e98f0a41db148cd4b48582855";
    "a2098d99d400f761d93b7fc105921a3207b9659e4558a67ee50c7d6e1c64ef5b";
    "f02c81da9bac4ae6afc9522011aa1d8705146c69e09c7baa00689ec07e622672";
    "da27c21f97b127a82588baa192c48677e4a4b160872a8f7113a18eb7e53e6956";
    "bbad0c6b0a13ffe69ae195ca3aea0c87398d77946ccf00ea02daa7c17adea465";
    "55436579202259115bb654c79879d92487da34d51e42f17e5fce0648fd333e73";
    "debec7e1982f365a7ea4a1365e1fbc59712ffe8099115d46da1845feeb5abfec";
    "d3db5a5d2272b9532a1c277ef85176dbada9efb97a1276052083dbc182a2e553";
    "fcdcfbb550e122075f355e9e1e78b2c3912249163b482f0a94ea01dc3812f621";
    "220a9c65d0b93fb71f5e0298dfd100cb57eb44b3215dcc52e80642375c1eee6f";
    "519ed0b74a17b4bc1aa6c3b86fe58cd5283edfc509cce93545eee0cc0eb05256";
    "895d50be954af7325457074561811d49c8b635159b341dec05238f831e6dab0f";
    "8bb546606ad17a946f5d8a12aca6e7e9e29bda75557eb6c5c1912b0953d71943";
    "d0cdd97734dbac164dc908428c8640aa583b8c1e081862393231c604e6178acb";
    "0a0ffbf506a46574324730f602f2301286516a85c0c69dbdb3af15359124ab9d";
  ]

(** Source-stable behavioral golden for [fixtures/match]: the match-arm and
    constructor-replacement families alongside the families they dedup against.
    Review every update as an operator-contract change. *)
let match_balanced_full_ids =
  [
    "d4770d39233c8e81361db88483f0e4536a82a43562c6ef6194e7065edb995411";
    "018411885ee6268ded1eb6679fd98596a6b9a57e842f054f75e5982db1d4dd38";
    "c47c3f27a837c0a6473820302e8d046cc1517fb476486f206dd85007ee2bc9cf";
    "fa3d50e80926585e62853f50c41cb2bd5cf163266216132b48530e724082e7d7";
    "a1f7a31423295ced4b382a627d0fca57f7b717c253c1ea3db7535ed284e58915";
    "ece8ba42e422ee2aae70df677fb80a9ed30ec5fb3f3b3252dc93b932e9a1f55d";
    "efbe244aa4c2d354863246db7b32e6b11378db6156a9ca5198f66444ac710939";
    "4926593a339de416fd9ce4646b0097fa374c3947d893ed80878f888706119a98";
    "64a02794707673aba42d5c670075b3162bf8eee5fbd81594f765a09a25d96b0d";
    "0ada025f39ffed5c63587d541d590797faa192fbb751bc8c95e0147cc4107a2c";
    "0c0418b19bc301942117877cdd63eb4e59c8d5e8b36a380a47691658ead38755";
    "a69b3ae15ed2faa879d3d529500edd6e847f86c1b18b3c3075f7c75112f73753";
    "6754fd83d16743733a92c433354da0a572bb6c2309f0736330b73b0433f703f6";
    "0ebae2df544724bf1d5ca7e367619f498173a6f3edee29d928b1fca9fa457d9c";
    "3c734eb2cb58df0e238b35766176be86b7e36b55a0080f0a19aee4548ce2f5a2";
    "2626e2dc33538beecd41fc348958ebee8763c3e656045158eda82615bbc9ed54";
    "4eb8901845841e0799ebf7e64ab19c9d26e723791b204c6e3cbec8e827252128";
    "b085853df07f190390a2b0cefdb31f621af2b9d6391d158322dd5fd1dc8ef81c";
    "f08a9bd3d7664328cd690f160c8e64695dfdaeae6c898aa0bcec8ec09be5f3bd";
    "993a44e2c0b48f91b01f805734e6cbd4d859b8220814dcb38ff09a55fbddcee5";
  ]

(* The real CLI resolves its run store under the OS cache root. These E2E runs
   must never write the developer's real store, and the opam-repository test
   sandbox mounts $HOME read-only, so every CLI invocation is redirected into an
   owned temporary cache root. The store materializes at most one new path
   component, so the default root's parent is pre-created. *)
let store_env =
  let root =
    Filename.temp_dir ~perms:0o700 "ocaml-mutants-e2e-store-" ".tmp"
    |> Unix.realpath
  in
  Unix.mkdir (Filename.concat root "ocaml-mutants") 0o700;
  at_exit (fun () -> ignore (Util.remove_tree root));
  [ ((if Sys.win32 then "LOCALAPPDATA" else "XDG_CACHE_HOME"), Some root) ]

(* Dune's POSIX sandbox presents declared dependency files as symlinks into the
   default build context. Resolve the dependency itself before taking its parent
   so the fixture root is the complete build-context tree, rather than a sandbox
   directory whose children legitimately point outside that directory. *)
let fixture_root fixture_project =
  fixture_project |> Unix.realpath |> Filename.dirname

let check_list ~cli fixture_project =
  let fixture = fixture_root fixture_project in
  let before =
    match Util.digest_tree ~skip:Workspace_snapshot.default_skip fixture with
    | Ok value -> value
    | Error message -> fail "cannot digest fixture before run: %s" message
  in
  let process =
    Process_supervisor.run ~timeout:120. ~cwd:fixture ~env:store_env
      [ cli; "list"; fixture; "--json"; "--no-color" ]
  in
  if not (Process_supervisor.succeeded process) then
    fail "list E2E failed (%s):\n%s%s"
      (Process_supervisor.status_string process.status)
      process.stdout process.stderr;
  let json =
    try Yojson.Safe.from_string process.stdout
    with Yojson.Json_error message ->
      fail "invalid list JSON: %s\n%s" message process.stdout
  in
  let mutants = Yojson.Safe.Util.(json |> member "mutants" |> to_list) in
  let skips = Yojson.Safe.Util.(json |> member "skips" |> to_list) in
  List.iter
    (fun skip ->
      let open Yojson.Safe.Util in
      let examples =
        skip |> member "examples" |> to_list |> List.map to_string
      in
      if examples = [] then fail "catalog skip omitted concrete examples";
      if examples <> List.sort_uniq String.compare examples then
        fail "catalog skip examples are not sorted and unique")
    skips;
  let document_type =
    Yojson.Safe.Util.(json |> member "document_type" |> to_string)
  in
  if document_type <> "ocaml-mutants.catalog-v2" then
    fail "unexpected list JSON document type: %s" document_type;
  let fixture_name = Filename.basename fixture in
  if fixture_name = "ppx-imprecise" then (
    if mutants <> [] then
      fail "non-byte-exact preprocessor output produced unproven mutants";
    if skips = [] then
      fail "non-byte-exact preprocessor output was not explained")
  else if fixture_name = "ppx" then (
    if mutants = [] then
      fail "byte-exact preprocessor output was not reverse-mapped")
  else if fixture_name = "match" then (
    let full_ids =
      List.map
        (fun mutant ->
          Yojson.Safe.Util.(mutant |> member "full_id" |> to_string))
        mutants
    in
    if full_ids <> match_balanced_full_ids then
      fail
        "match fixture Balanced catalog changed; review this as an explicit \
         operator-contract change";
    let rules =
      List.map
        (fun mutant -> Yojson.Safe.Util.(mutant |> member "rule" |> to_string))
        mutants
    in
    List.iter
      (fun expected ->
        if not (List.mem expected rules) then
          fail "match fixture omitted rule %s" expected)
      [
        "match-arm-empty-string@1";
        "match-arm-none@1";
        "some-to-none@1";
        "cons-to-nil@1";
      ];
    (* The exact-edit ties must keep their historical winners: the arm [Some
       value] belongs to match-arm-none and the list-typed body to
       return-empty-list, with the losers explained as duplicates. *)
    let duplicate_skip =
      List.find_opt
        (fun skip ->
          let open Yojson.Safe.Util in
          skip |> member "reason" |> to_string
          = "duplicate source transformation")
        skips
    in
    if duplicate_skip = None then
      fail "match fixture dedup losers were not explained in catalog skips")
  else if fixture_name = "root-test" then (
    if mutants = [] then fail "root-test fixture produced no production mutants";
    List.iter
      (fun mutant ->
        let path = Yojson.Safe.Util.(mutant |> member "path" |> to_string) in
        if path <> "subject.ml" then
          fail "root test module leaked into catalog: %s" path)
      mutants)
  else if fixture_name = "basic" then (
    let full_ids =
      List.map
        (fun mutant ->
          Yojson.Safe.Util.(mutant |> member "full_id" |> to_string))
        mutants
    in
    if full_ids <> basic_balanced_full_ids then
      fail
        "basic fixture Balanced catalog changed; review this as an explicit \
         operator-contract change";
    (* The tiers must stay monotonically inclusive and must not regress to
       no-ops: strong adds the fixture's if-branch mutants over balanced. *)
    let profile_ids profile =
      let process =
        Process_supervisor.run ~timeout:120. ~cwd:fixture ~env:store_env
          [ cli; "list"; fixture; "--profile"; profile; "--json"; "--no-color" ]
      in
      if not (Process_supervisor.succeeded process) then
        fail "profile %s list E2E failed (%s):\n%s%s" profile
          (Process_supervisor.status_string process.status)
          process.stdout process.stderr;
      let json =
        try Yojson.Safe.from_string process.stdout
        with Yojson.Json_error message ->
          fail "invalid %s profile list JSON: %s\n%s" profile message
            process.stdout
      in
      Yojson.Safe.Util.(json |> member "mutants" |> to_list)
      |> List.map (fun mutant ->
          Yojson.Safe.Util.(mutant |> member "full_id" |> to_string))
    in
    let strong_ids = profile_ids "strong" in
    let all_ids = profile_ids "all" in
    let subset smaller larger =
      List.for_all (fun id -> List.mem id larger) smaller
    in
    if not (subset full_ids strong_ids) then
      fail "strong profile lost mutants from the balanced catalog";
    if not (subset strong_ids all_ids) then
      fail "all profile lost mutants from the strong catalog";
    if List.length strong_ids <= List.length full_ids then
      fail "strong profile added no mutants over balanced";
    let rules =
      List.map
        (fun mutant -> Yojson.Safe.Util.(mutant |> member "rule" |> to_string))
        mutants
    in
    List.iter
      (fun expected ->
        if not (List.mem expected rules) then
          fail "basic fixture omitted high-value rule %s" expected)
      [ "lt-to-le@1"; "return-none@1"; "return-empty-list@1" ];
    List.iter
      (fun mutant ->
        let open Yojson.Safe.Util in
        if
          mutant |> member "path" |> to_string = "lib/fixture_math.ml"
          && mutant |> member "family" |> to_string = "integer-arithmetic"
          && mutant |> member "range" |> member "start_line" |> to_int >= 17
        then fail "shadowed local operator was mutated")
      mutants;
    let duplicate_rule_pair left right =
      List.mem (left, right)
        [
          ("true-to-false@1", "select-else-branch@1");
          ("false-to-true@1", "select-then-branch@1");
          ("true-to-false@1", "return-false@1");
          ("false-to-true@1", "return-true@1");
        ]
      || List.mem (right, left)
           [
             ("true-to-false@1", "select-else-branch@1");
             ("false-to-true@1", "select-then-branch@1");
             ("true-to-false@1", "return-false@1");
             ("false-to-true@1", "return-true@1");
           ]
    in
    let same_edit_range left right =
      let open Yojson.Safe.Util in
      left |> member "path" = (right |> member "path")
      && left |> member "range" = (right |> member "range")
    in
    List.iter
      (fun left ->
        List.iter
          (fun right ->
            let open Yojson.Safe.Util in
            let left_rule = left |> member "rule" |> to_string in
            let right_rule = right |> member "rule" |> to_string in
            if
              same_edit_range left right
              && duplicate_rule_pair left_rule right_rule
            then
              fail "duplicate semantic mutation at one range: %s and %s"
                left_rule right_rule)
          mutants)
      mutants;
    let duplicate_skip =
      List.find_opt
        (fun skip ->
          let open Yojson.Safe.Util in
          skip |> member "reason" |> to_string
          = "duplicate source transformation")
        skips
    in
    match duplicate_skip with
    | None ->
        fail "semantic duplicate removal was not explained in catalog skips"
    | Some skip ->
        let open Yojson.Safe.Util in
        if skip |> member "count" |> to_int <= 0 then
          fail "semantic duplicate skip has an invalid occurrence count";
        let examples =
          skip |> member "examples" |> to_list |> List.map to_string
        in
        if
          not
            (List.exists
               (fun example ->
                 contains_substring ~needle:"lib/fixture_math.ml:" example)
               examples)
        then fail "semantic duplicate skip has no concrete mutation range")
  else if mutants = [] then
    fail "%s fixture produced no Typedtree mutants" fixture_name;
  let after =
    match Util.digest_tree ~skip:Workspace_snapshot.default_skip fixture with
    | Ok value -> value
    | Error message -> fail "cannot digest fixture after run: %s" message
  in
  if before <> after then fail "ocaml-mutants changed the source fixture"

let check_custom_command ~cli fixture_project =
  let fixture = fixture_root fixture_project in
  let before =
    match Util.digest_tree ~skip:Workspace_snapshot.default_skip fixture with
    | Ok value -> value
    | Error message -> fail "cannot digest custom fixture: %s" message
  in
  let process =
    Process_supervisor.run ~timeout:120. ~cwd:fixture ~env:store_env
      [
        cli;
        "run";
        "--fresh";
        "--jobs";
        "1";
        "--timeout";
        "10";
        "--json";
        "--";
        "dune";
        "exec";
        "./check.exe";
      ]
  in
  if not (Process_supervisor.succeeded process) then
    fail "custom-command E2E failed (%s):\n%s%s"
      (Process_supervisor.status_string process.status)
      process.stdout process.stderr;
  let report =
    try Yojson.Safe.from_string process.stdout
    with Yojson.Json_error message ->
      fail "invalid custom-command report JSON: %s" message
  in
  let hit_map =
    Yojson.Safe.Util.(
      report |> member "test" |> member "inventory" |> member "hit_map"
      |> to_list)
  in
  if hit_map = [] then fail "custom-command report omitted runtime hit evidence";
  let mutants = Yojson.Safe.Util.(report |> member "mutants" |> to_list) in
  let fast_estimates =
    List.filter
      (fun result ->
        Yojson.Safe.Util.(
          result |> member "evidence" |> member "origin" |> to_string)
        = "fast-estimated")
      mutants
  in
  if fast_estimates = [] then
    fail "custom-command fast run omitted no non-covering test evidence";
  List.iter
    (fun result ->
      let open Yojson.Safe.Util in
      if result |> member "coverage" |> to_string <> "not-covered" then
        fail "fast-estimated result was not marked not-covered";
      if result |> member "stages" |> to_list <> [] then
        fail "fast-estimated result retained an executed stage")
    fast_estimates;
  if
    not
      (List.exists
         (fun result ->
           Yojson.Safe.Util.(
             result |> member "evidence" |> member "origin" |> to_string)
           = "execution")
         mutants)
  then fail "custom-command fast run lost all executed evidence";
  if contains_substring ~needle:"privacy-secret" process.stdout then
    fail "custom-command report retained a configured privacy literal";
  if not (contains_substring ~needle:"**************" process.stdout) then
    fail "custom-command report did not retain redaction evidence";
  let dash_output = Filename.concat fixture "-" in
  if Sys.file_exists dash_output then
    fail "custom-command fixture already contains a '-' output path";
  let multiple_stdout =
    Process_supervisor.run ~timeout:30. ~cwd:fixture ~env:store_env
      [
        cli;
        "report";
        ".";
        "--format";
        "json";
        "--format";
        "markdown";
        "--output";
        "-";
      ]
  in
  (match multiple_stdout.status with
  | Process_supervisor.Exited 2 -> ()
  | _ ->
      fail "multiple report formats accepted --output - (%s)"
        (Process_supervisor.status_string multiple_stdout.status));
  if Sys.file_exists dash_output then
    fail "multiple report formats created a '-' directory";
  let after =
    match Util.digest_tree ~skip:Workspace_snapshot.default_skip fixture with
    | Ok value -> value
    | Error message -> fail "cannot digest custom fixture after run: %s" message
  in
  if before <> after then fail "custom run changed the source fixture";
  let without_separator =
    Process_supervisor.run ~timeout:30. ~cwd:fixture ~env:store_env
      [ cli; "run"; "."; "dune"; "runtest" ]
  in
  (match without_separator.status with
  | Process_supervisor.Exited 2 -> ()
  | _ ->
      fail "run without -- accepted surplus argv (%s)"
        (Process_supervisor.status_string without_separator.status));
  let contradictory =
    Process_supervisor.run ~timeout:30. ~cwd:fixture ~env:store_env
      [ cli; "list"; "."; "--json"; "--quiet" ]
  in
  (match contradictory.status with
  | Process_supervisor.Exited 2 -> ()
  | _ ->
      fail "--json/--quiet contradiction was accepted (%s)"
        (Process_supervisor.status_string contradictory.status));
  let cache_help =
    Process_supervisor.run ~timeout:30. ~cwd:fixture ~env:store_env
      [ cli; "cache"; "--help" ]
  in
  if not (Process_supervisor.succeeded cache_help) then
    fail "cache --help failed";
  if
    contains_substring ~needle:"One or more mutants survived"
      (cache_help.stdout ^ cache_help.stderr)
  then fail "cache help reused run-only survivor exit semantics"

let parse_json label process =
  try Yojson.Safe.from_string process.Process_supervisor.stdout
  with Yojson.Json_error message ->
    fail "invalid %s JSON: %s\n%s" label message process.stdout

let check_timeout ~cli fixture_project =
  let fixture = fixture_root fixture_project in
  let catalog =
    Process_supervisor.run ~timeout:120. ~cwd:fixture ~env:store_env
      [ cli; "list"; "."; "--json"; "--no-color" ]
  in
  if not (Process_supervisor.succeeded catalog) then
    fail "timeout fixture catalog failed (%s):\n%s%s"
      (Process_supervisor.status_string catalog.status)
      catalog.stdout catalog.stderr;
  let mutant =
    match
      parse_json "timeout catalog" catalog
      |> Yojson.Safe.Util.member "mutants"
      |> Yojson.Safe.Util.to_list
    with
    | mutant :: _ -> Yojson.Safe.Util.(mutant |> member "full_id" |> to_string)
    | [] -> fail "timeout fixture produced no mutant to execute"
  in
  let process =
    Process_supervisor.run ~timeout:120. ~cwd:fixture ~env:store_env
      [
        cli; "run"; "."; "--json"; "--fresh"; "--jobs"; "1"; "--mutant"; mutant;
      ]
  in
  (match process.status with
  | Process_supervisor.Exited 0 -> ()
  | _ ->
      fail "timeout E2E returned %s:\n%s%s"
        (Process_supervisor.status_string process.status)
        process.stdout process.stderr);
  let json = parse_json "timeout report" process in
  let open Yojson.Safe.Util in
  if json |> member "status" |> to_string <> "completed" then
    fail "timeout run did not complete";
  if json |> member "summary" |> member "timeout" |> to_int = 0 then
    fail "timeout run did not record a timeout outcome"

(* The match fixture has no tests, so every mutant trivially survives: the run
   proves the instrumented tree (match/try arms and constructor swaps included)
   still compiles under the default fatal-warning dev profile, and pins the
   all-survivors score at exactly 0. *)
let check_match_run ~cli fixture_project =
  let fixture = fixture_root fixture_project in
  let process =
    Process_supervisor.run ~timeout:600. ~cwd:fixture ~env:store_env
      [ cli; "run"; "."; "--json"; "--fresh"; "--jobs"; "1" ]
  in
  (match process.status with
  | Process_supervisor.Exited 0 -> ()
  | _ ->
      fail "match run E2E returned %s:\n%s%s"
        (Process_supervisor.status_string process.status)
        process.stdout process.stderr);
  let json = parse_json "match run report" process in
  let open Yojson.Safe.Util in
  if json |> member "status" |> to_string <> "completed" then
    fail "match run did not complete";
  let summary = json |> member "summary" in
  let expected = List.length match_balanced_full_ids in
  if summary |> member "executed" |> to_int <> expected then
    fail "match run did not execute the complete Balanced catalog";
  if summary |> member "unexpected_survivors" |> to_int <> expected then
    fail "match run without tests did not report every mutant as surviving";
  if summary |> member "score" |> to_float <> 0.0 then
    fail "match run without tests did not score exactly zero"

let check_baseline_failure ~cli fixture_project =
  let fixture = fixture_root fixture_project in
  let explicit_timeout_seconds = 17. in
  let process =
    Process_supervisor.run ~timeout:120. ~cwd:fixture ~env:store_env
      [
        cli;
        "run";
        ".";
        "--json";
        "--fresh";
        "--jobs";
        "1";
        "--timeout";
        string_of_float explicit_timeout_seconds;
      ]
  in
  (match process.status with
  | Process_supervisor.Exited 2 -> ()
  | _ ->
      fail "baseline-failure E2E returned %s:\n%s%s"
        (Process_supervisor.status_string process.status)
        process.stdout process.stderr);
  let json = parse_json "baseline failure report" process in
  let open Yojson.Safe.Util in
  if json |> member "status" |> to_string <> "failed" then
    fail "baseline failure report was not failed";
  if json |> member "summary" |> member "kind" |> to_string <> "partial" then
    fail "baseline failure report summary was not partial";
  if json |> member "summary" |> member "not_run" |> to_int = 0 then
    fail "baseline failure report omitted not-run mutants";
  if
    json |> member "test" |> member "timeout_seconds" |> to_float
    <> explicit_timeout_seconds
  then fail "baseline failure report omitted the configured timeout";
  if json |> member "failure" |> member "phase" |> to_string <> "baseline-proof"
  then fail "baseline failure report used the wrong phase"

let check_dirty_git_workspace ~cli fixture_project =
  let fixture = fixture_root fixture_project in
  match
    Workspace_snapshot.with_snapshot fixture (fun snapshot ->
        let root = Workspace_snapshot.root snapshot in
        let git arguments =
          let process =
            Process_supervisor.run ~timeout:30. ~cwd:root ~env:store_env
              ("git" :: arguments)
          in
          if not (Process_supervisor.succeeded process) then
            fail "git %s failed: %s%s"
              (String.concat " " arguments)
              process.stdout process.stderr;
          process.stdout
        in
        ignore (git [ "init"; "--quiet" ]);
        ignore (git [ "config"; "user.email"; "fixture@example.invalid" ]);
        ignore (git [ "config"; "user.name"; "Fixture" ]);
        (match
           Util.write_file (Filename.concat root "renamed.txt") "tracked\n"
         with
        | Ok () -> ()
        | Error message -> fail "cannot create tracked fixture: %s" message);
        ignore (git [ "add"; "." ]);
        ignore (git [ "commit"; "--quiet"; "-m"; "fixture" ]);
        let subject = Filename.concat root "lib/fixture_math.ml" in
        let contents =
          match Util.read_file subject with
          | Ok contents -> contents
          | Error message -> fail "cannot read dirty source: %s" message
        in
        (match Util.write_file subject (contents ^ "\n") with
        | Ok () -> ()
        | Error message -> fail "cannot dirty source: %s" message);
        let project = Filename.concat root "dune-project" in
        let project_contents =
          match Util.read_file project with
          | Ok contents -> contents
          | Error message -> fail "cannot read staged source: %s" message
        in
        (match Util.write_file project (project_contents ^ "\n") with
        | Ok () -> ()
        | Error message -> fail "cannot stage source: %s" message);
        ignore (git [ "add"; "dune-project" ]);
        Sys.rename
          (Filename.concat root "renamed.txt")
          (Filename.concat root "renamed-new.txt");
        (match
           Util.write_file (Filename.concat root "untracked.txt") "untracked\n"
         with
        | Ok () -> ()
        | Error message -> fail "cannot create untracked file: %s" message);
        let before_status = git [ "status"; "--porcelain=v1"; "-z" ] in
        let before_digest =
          match Util.digest_tree ~skip:Workspace_snapshot.default_skip root with
          | Ok digest -> digest
          | Error message -> fail "cannot digest dirty workspace: %s" message
        in
        let process =
          Process_supervisor.run ~timeout:120. ~cwd:root ~env:store_env
            [ cli; "list"; "."; "--json"; "--no-color" ]
        in
        if not (Process_supervisor.succeeded process) then
          fail "dirty Git list failed (%s):\n%s%s"
            (Process_supervisor.status_string process.status)
            process.stdout process.stderr;
        let after_status = git [ "status"; "--porcelain=v1"; "-z" ] in
        let after_digest =
          match Util.digest_tree ~skip:Workspace_snapshot.default_skip root with
          | Ok digest -> digest
          | Error message ->
              fail "cannot digest dirty workspace after run: %s" message
        in
        if before_status <> after_status then
          fail "dirty Git status changed across list";
        if before_digest <> after_digest then
          fail "dirty workspace bytes changed across list";
        Ok ())
  with
  | Ok () -> ()
  | Error error ->
      fail "dirty Git fixture failed: %s" (Format.asprintf "%a" Error.pp error)

let run () =
  if Array.length Sys.argv < 3 then
    fail "usage: e2e_tests CLI FIXTURE_DUNE_PROJECT...";
  let cli = Unix.realpath Sys.argv.(1) in
  let fixtures =
    Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2))
  in
  List.iter (check_list ~cli) fixtures;
  fixtures
  |> List.find_opt (fun project ->
      Filename.basename (fixture_root project) = "custom-command")
  |> Option.iter (check_custom_command ~cli);
  fixtures
  |> List.find_opt (fun project ->
      Filename.basename (fixture_root project) = "timeout")
  |> Option.iter (check_timeout ~cli);
  fixtures
  |> List.find_opt (fun project ->
      Filename.basename (fixture_root project) = "baseline-failure")
  |> Option.iter (check_baseline_failure ~cli);
  fixtures
  |> List.find_opt (fun project ->
      Filename.basename (fixture_root project) = "match")
  |> Option.iter (check_match_run ~cli);
  fixtures
  |> List.find_opt (fun project ->
      Filename.basename (fixture_root project) = "basic")
  |> Option.iter (check_dirty_git_workspace ~cli)

let () =
  if Process_supervisor.helper_requested Sys.argv then
    exit (Process_supervisor.run_helper Sys.argv)
  else (
    Process_supervisor.configure_helper_executable
      (Unix.realpath Sys.executable_name);
    run ())
