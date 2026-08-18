module Engine = Ocaml_mutants_engine

exception Contract_failure of string

let fail format =
  Printf.ksprintf (fun message -> raise (Contract_failure message)) format

let get_ok label = function
  | Ok value -> value
  | Error message -> fail "%s: %s" label message

let write path contents =
  get_ok ("cannot write " ^ path) (Engine.Util.atomic_write path contents)

let normalize path =
  let normalized = Ocaml_mutants_core.Mutant.normalize_path path in
  if Sys.win32 then String.lowercase_ascii normalized else normalized

let within ~parent child =
  let parent = normalize parent in
  let child = normalize child in
  String.equal parent child || String.starts_with ~prefix:(parent ^ "/") child

type layout = {
  parent : string;
  workspace : string;
  cache : string;
  marker : string;
  marker_contents : string;
}

let create_layout () =
  let parent =
    Filename.temp_dir ~perms:0o700 "ocaml-mutants-expectation-" ".tmp"
    |> Unix.realpath
  in
  let workspace = Filename.concat parent "workspace" in
  Unix.mkdir workspace 0o700;
  let cache = Filename.concat parent "cache" in
  let marker = ".expectation-contract-owner" in
  let marker_contents =
    Printf.sprintf "owner=expectation-contract\nnonce=%s\n"
      (Engine.Util.sha256
         (Printf.sprintf "%s\000%d\000%.17g" parent (Unix.getpid ())
            (Unix.gettimeofday ())))
  in
  write (Filename.concat parent marker) marker_contents;
  write (Filename.concat workspace marker) marker_contents;
  { parent; workspace; cache; marker; marker_contents }

let cleanup layout =
  let temp_root = Unix.realpath (Filename.get_temp_dir_name ()) in
  let parent = Unix.realpath layout.parent in
  if String.equal (normalize temp_root) (normalize parent) then
    fail "refusing to remove the temporary root itself";
  if not (within ~parent:temp_root parent) then
    fail "owned directory escaped the temporary root: %s" parent;
  let check_marker path =
    let actual =
      get_ok
        ("cannot read ownership marker " ^ path)
        (Engine.Util.read_file path)
    in
    if not (String.equal actual layout.marker_contents) then
      fail "ownership marker changed: %s" path
  in
  check_marker (Filename.concat parent layout.marker);
  check_marker (Filename.concat layout.workspace layout.marker);
  get_ok "cannot remove owned expectation workspace"
    (Engine.Util.remove_tree parent);
  if Sys.file_exists parent then
    fail "owned temporary workspace was not removed"

let source_digest workspace marker =
  let skip relative =
    Engine.Workspace_snapshot.default_skip relative
    || String.equal relative ".ocaml-mutants.toml"
    || String.equal relative marker
  in
  get_ok "cannot digest source workspace"
    (Engine.Util.digest_tree ~skip workspace)

let cli_timeout_seconds = 180.

let run_cli ~cli ~workspace arguments =
  Engine.Process_supervisor.run ~timeout:cli_timeout_seconds ~cwd:workspace
    ~env:[ ("DUNE_CACHE", Some "disabled") ]
    (cli :: arguments)

let expect_exit label expected process =
  match process.Engine.Process_supervisor.status with
  | Engine.Process_supervisor.Exited actual when actual = expected -> ()
  | status ->
      fail "%s returned %s (expected exit %d):\n%s%s" label
        (Engine.Process_supervisor.status_string status)
        expected process.stdout process.stderr

let parse_json label process =
  try Yojson.Safe.from_string process.Engine.Process_supervisor.stdout
  with Yojson.Json_error message ->
    fail "%s emitted invalid JSON: %s\n%s" label message process.stdout

let discover_mutant ~cli layout =
  let process =
    run_cli ~cli ~workspace:layout.workspace
      [ "list"; layout.workspace; "--json"; "--no-color" ]
  in
  expect_exit "catalog discovery" 0 process;
  let open Yojson.Safe.Util in
  let mutants =
    parse_json "catalog discovery" process |> member "mutants" |> to_list
  in
  match
    List.find_opt
      (fun mutant ->
        String.equal (mutant |> member "path" |> to_string) "subject.ml"
        && String.equal (mutant |> member "rule" |> to_string) "true-to-false@1")
      mutants
  with
  | Some mutant -> mutant |> member "full_id" |> to_string
  | None -> fail "catalog did not contain subject.ml true-to-false@1"

let write_config layout mutant_id stage_command =
  let stage_command =
    stage_command |> List.map (Printf.sprintf "%S") |> String.concat ", "
  in
  let config =
    Printf.sprintf
      {|version = 1

[mutation]
include = ["subject.ml"]
profile = "balanced"

[[mutation.expect]]
id = %S
reason = "Equivalent in the deliberately always-successful build stage."

[test]
baseline_runs = 1
parallel_safe = false

[[test.stages]]
name = "build"
command = [%s]

[execution]
jobs = 1

[cache]
mode = "on"
directory = %S
|}
      mutant_id stage_command layout.cache
  in
  write (Filename.concat layout.workspace ".ocaml-mutants.toml") config

let check_fulfilled_report label mutant_id process =
  expect_exit label 0 process;
  let open Yojson.Safe.Util in
  let report = parse_json label process in
  let summary = report |> member "summary" in
  if summary |> member "expected_survivors" |> to_int <> 1 then
    fail "%s did not count the fulfilled expected survivor" label;
  if summary |> member "unexpected_survivors" |> to_int <> 0 then
    fail "%s counted an unexpected survivor" label;
  if summary |> member "unfulfilled_expectations" |> to_int <> 0 then
    fail "%s counted an unfulfilled expectation" label;
  let result =
    match report |> member "mutants" |> to_list with
    | [ result ] -> result
    | results ->
        fail "%s reported %d mutant results" label (List.length results)
  in
  if
    not
      (String.equal
         (result |> member "mutant" |> member "full_id" |> to_string)
         mutant_id)
  then fail "%s reported a different mutant" label;
  if not (String.equal (result |> member "outcome" |> to_string) "survived")
  then fail "%s expected mutant did not survive" label;
  if not (result |> member "expected_survivor" |> to_bool) then
    fail "%s result was not marked as an expected survivor" label;
  if
    not
      (String.equal
         (result |> member "expectation" |> member "status" |> to_string)
         "fulfilled")
  then fail "%s result expectation was not fulfilled" label;
  if result |> member "cached" |> to_bool then
    fail "%s reused a cached expected-mutant outcome" label;
  match report |> member "expectations" |> to_list with
  | [ expectation ]
    when String.equal (expectation |> member "mutant_id" |> to_string) mutant_id
         && String.equal
              (expectation |> member "status" |> to_string)
              "fulfilled" ->
      ()
  | _ -> fail "%s top-level expectation ledger was not fulfilled" label

let check_unfulfilled_report label mutant_id process =
  expect_exit label 2 process;
  let open Yojson.Safe.Util in
  let report = parse_json label process in
  let summary = report |> member "summary" in
  if summary |> member "expected_survivors" |> to_int <> 0 then
    fail "%s counted a killed mutant as an expected survivor" label;
  if summary |> member "unexpected_survivors" |> to_int <> 0 then
    fail "%s counted an unexpected survivor" label;
  if summary |> member "unfulfilled_expectations" |> to_int <> 1 then
    fail "%s did not count exactly one unfulfilled expectation" label;
  let result =
    match report |> member "mutants" |> to_list with
    | [ result ] -> result
    | results ->
        fail "%s reported %d mutant results" label (List.length results)
  in
  if
    not
      (String.equal
         (result |> member "mutant" |> member "full_id" |> to_string)
         mutant_id)
  then fail "%s reported a different mutant" label;
  if not (String.equal (result |> member "outcome" |> to_string) "killed") then
    fail "%s expected mutant was not killed" label;
  if result |> member "cached" |> to_bool then
    fail "%s reused a cached expected-mutant outcome" label;
  if
    not
      (String.equal
         (result |> member "expectation" |> member "status" |> to_string)
         "unfulfilled-killed")
  then fail "%s result expectation was not classified as unfulfilled" label;
  match report |> member "expectations" |> to_list with
  | [ expectation ]
    when String.equal (expectation |> member "mutant_id" |> to_string) mutant_id
         && String.equal
              (expectation |> member "status" |> to_string)
              "unfulfilled-killed" ->
      ()
  | _ -> fail "%s top-level expectation ledger was not unfulfilled" label

let run_contract cli =
  let layout = create_layout () in
  Fun.protect
    ~finally:(fun () -> cleanup layout)
    (fun () ->
      write
        (Filename.concat layout.workspace "dune-project")
        "(lang dune 3.21)\n(name expectation_contract)\n";
      write
        (Filename.concat layout.workspace "dune")
        {|(library
 (name subject)
 (modules subject))

(test
 (name test_subject)
 (modules test_subject)
 (libraries subject))
|};
      write (Filename.concat layout.workspace "subject.ml") "let value = true\n";
      write
        (Filename.concat layout.workspace "test_subject.ml")
        "let () = if Subject.value then () else failwith \"mutant observed\"\n";
      let before = source_digest layout.workspace layout.marker in
      let mutant_id = discover_mutant ~cli layout in
      write_config layout mutant_id [ "dune"; "build" ];
      let run check label =
        run_cli ~cli ~workspace:layout.workspace
          [
            "run";
            layout.workspace;
            "--json";
            "--no-color";
            "--jobs";
            "1";
            "--mutant";
            mutant_id;
          ]
        |> check label mutant_id
      in
      run check_fulfilled_report "first expected-mutant run";
      run check_fulfilled_report "second expected-mutant run";
      write_config layout mutant_id [ "dune"; "runtest"; "--force" ];
      run check_unfulfilled_report "unfulfilled expected-mutant run";
      let after = source_digest layout.workspace layout.marker in
      if not (String.equal before after) then
        fail "real CLI expectation runs changed the source workspace")

let () =
  if Engine.Process_supervisor.helper_requested Sys.argv then
    exit (Engine.Process_supervisor.run_helper Sys.argv)
  else (
    Engine.Process_supervisor.configure_helper_executable
      (Unix.realpath Sys.executable_name);
    try
      if Array.length Sys.argv <> 2 then
        fail "expected the ocaml-mutants CLI path as the only argument";
      run_contract (Unix.realpath Sys.argv.(1))
    with Contract_failure message ->
      prerr_endline message;
      exit 1)
