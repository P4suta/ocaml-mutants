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
  marker : string;
  marker_contents : string;
}

let create_layout () =
  let parent =
    Filename.temp_dir ~perms:0o700 "ocaml-mutants-cli-lifecycle-" ".tmp"
    |> Unix.realpath
  in
  let workspace = Filename.concat parent "workspace" in
  Unix.mkdir workspace 0o700;
  let marker = ".cli-lifecycle-contract-owner" in
  let marker_contents =
    Printf.sprintf "owner=cli-lifecycle-contract\nnonce=%s\n"
      (Engine.Util.sha256
         (Printf.sprintf "%s\000%d\000%.17g" parent (Unix.getpid ())
            (Unix.gettimeofday ())))
  in
  write (Filename.concat parent marker) marker_contents;
  write (Filename.concat workspace marker) marker_contents;
  { parent; workspace; marker; marker_contents }

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
  get_ok "cannot remove owned CLI lifecycle workspace"
    (Engine.Util.remove_tree parent);
  if Sys.file_exists parent then
    fail "owned temporary workspace was not removed"

let source_digest layout =
  let skip relative =
    Engine.Workspace_snapshot.default_skip relative
    || String.equal relative ".ocaml-mutants.toml"
    || String.equal relative layout.marker
  in
  get_ok "cannot digest source workspace"
    (Engine.Util.digest_tree ~skip layout.workspace)

let cli_deadline_seconds = 180.

let run_cli ?(extra_env = []) ~cli layout arguments =
  Engine.Process_supervisor.run ~timeout:cli_deadline_seconds
    ~cwd:layout.workspace
    ~env:(("DUNE_CACHE", Some "disabled") :: extra_env)
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

let expect_string label expected json =
  match json with
  | `String actual when String.equal actual expected -> ()
  | actual ->
      fail "%s was %s (expected %S)" label
        (Yojson.Safe.to_string actual)
        expected

let expect_int label expected json =
  match json with
  | `Int actual when actual = expected -> ()
  | actual ->
      fail "%s was %s (expected %d)" label
        (Yojson.Safe.to_string actual)
        expected

let expect_bool label expected json =
  match json with
  | `Bool actual when Bool.equal actual expected -> ()
  | actual ->
      fail "%s was %s (expected %b)" label
        (Yojson.Safe.to_string actual)
        expected

let expect_null label = function
  | `Null -> ()
  | actual -> fail "%s was not null: %s" label (Yojson.Safe.to_string actual)

let contains_substring ~needle value =
  let rec search index =
    if index + String.length needle > String.length value then false
    else if String.sub value index (String.length needle) = needle then true
    else search (index + 1)
  in
  search 0

let count_occurrences ~needle value =
  if String.equal needle "" then invalid_arg "empty occurrence needle";
  let rec count total index =
    if index + String.length needle > String.length value then total
    else if String.sub value index (String.length needle) = needle then
      count (total + 1) (index + String.length needle)
    else count total (index + 1)
  in
  count 0 0

let singleton label = function
  | [ value ] -> value
  | values ->
      fail "%s contained %d entries (expected one)" label (List.length values)

let timeout_stage_flag = "--cli-lifecycle-timeout-stage"
let killing_stage_flag = "--cli-lifecycle-killing-stage"

(** Long enough that a broken supervisor returns a failed contract instead of
    leaving this helper alive indefinitely. Under the contract the helper is
    terminated at [mutant_timeout_seconds], so this guard is never reached. *)
let mutant_timeout_seconds = 2.

let stage_guard_seconds = mutant_timeout_seconds *. 4.

let active_mutant () =
  match Sys.getenv_opt "OCAML_MUTANTS_ACTIVE" with
  | Some value when String.trim value <> "" -> Some value
  | Some _ | None -> None

let run_timeout_stage () =
  match active_mutant () with
  | None -> ()
  | Some mutant ->
      Printf.printf "timeout-contract-stdout:%s\n%!" mutant;
      Printf.eprintf "timeout-contract-stderr:%s\n%!" mutant;
      Unix.sleepf stage_guard_seconds;
      fail "timeout stage outlived the process supervisor deadline"

let run_killing_stage () =
  match active_mutant () with
  | None -> ()
  | Some mutant ->
      Printf.printf "kill-contract-stdout:%s\n%!" mutant;
      Printf.eprintf "kill-contract-stderr:%s\n%!" mutant;
      exit 23

let catalog_ids ?extra_env ~cli layout =
  let process =
    run_cli ?extra_env ~cli layout
      [ "list"; layout.workspace; "--json"; "--no-color" ]
  in
  expect_exit "catalog discovery" 0 process;
  let open Yojson.Safe.Util in
  let ids =
    parse_json "catalog discovery" process
    |> member "mutants" |> to_list
    |> List.filter_map (fun mutant ->
        if String.equal (mutant |> member "path" |> to_string) "subject.ml" then
          Some (mutant |> member "full_id" |> to_string)
        else None)
    |> List.sort_uniq String.compare
  in
  match ids with
  | first :: second :: _ -> (ids, first, second)
  | _ ->
      fail
        "catalog contained %d distinct subject.ml mutants (expected at least \
         two)"
        (List.length ids)

let absent_full_id catalog =
  let present = Hashtbl.create (List.length catalog) in
  List.iter (fun id -> Hashtbl.replace present id ()) catalog;
  let rec choose sequence =
    let candidate =
      Engine.Util.sha256
        (Printf.sprintf "cli-lifecycle-stale-expectation\000%d" sequence)
    in
    if Hashtbl.mem present candidate then choose (sequence + 1) else candidate
  in
  choose 0

let expectation_block = function
  | None -> ""
  | Some (id, reason) ->
      Printf.sprintf {|[[mutation.expect]]
id = %S
reason = %S

|} id reason

let write_config layout ~stage_flag ~stage_name ?expectation ?cache_directory
    executable =
  let cache_directory_line =
    match cache_directory with
    | None -> ""
    | Some directory -> Printf.sprintf "directory = %S\n" directory
  in
  let config =
    Printf.sprintf
      {|version = 1

[mutation]
include = ["subject.ml"]
profile = "balanced"

%s[test]
timeout = %.1f
baseline_runs = 1
parallel_safe = false

[[test.stages]]
name = %S
command = [%S, %S]

[execution]
jobs = 1

[cache]
mode = "off"
%s|}
      (expectation_block expectation)
      mutant_timeout_seconds stage_name executable stage_flag
      cache_directory_line
  in
  write (Filename.concat layout.workspace ".ocaml-mutants.toml") config

let run_json ?extra_env ~cli layout arguments ~label ~exit_code =
  let process =
    run_cli ?extra_env ~cli layout
      ("run" :: layout.workspace :: "--json" :: "--no-color" :: arguments)
  in
  expect_exit label exit_code process;
  parse_json label process

let check_completed_report label report =
  let open Yojson.Safe.Util in
  expect_string (label ^ " document type") "ocaml-mutants.run-report-v1"
    (report |> member "document_type");
  expect_string (label ^ " status") "completed" (report |> member "status");
  expect_null (label ^ " failure") (report |> member "failure")

let check_capture label marker capture =
  let open Yojson.Safe.Util in
  let contents = capture |> member "contents" |> to_string in
  if not (contains_substring ~needle:marker contents) then
    fail "%s did not retain %S: %S" label marker contents;
  expect_bool (label ^ " truncated") false (capture |> member "truncated");
  let total_bytes = capture |> member "total_bytes" |> to_int in
  if total_bytes < String.length contents then
    fail "%s total_bytes=%d was smaller than retained bytes=%d" label
      total_bytes (String.length contents)

let check_timeout_attempt label mutant_id attempt =
  let open Yojson.Safe.Util in
  expect_string (label ^ " outcome") "timeout" (attempt |> member "outcome");
  expect_null (label ^ " error") (attempt |> member "error");
  let duration = attempt |> member "duration_seconds" |> to_float in
  if duration <= 0. then fail "%s duration was not positive" label;
  let stage =
    attempt |> member "stages" |> to_list |> singleton (label ^ " stages")
  in
  expect_string (label ^ " stage name") "confirmed-timeout"
    (stage |> member "name");
  expect_string (label ^ " stage status") "timeout" (stage |> member "status");
  let stdout_marker = "timeout-contract-stdout:" ^ String.sub mutant_id 0 20 in
  let stderr_marker = "timeout-contract-stderr:" ^ String.sub mutant_id 0 20 in
  check_capture (label ^ " stdout") stdout_marker (attempt |> member "stdout");
  check_capture (label ^ " stderr") stderr_marker (attempt |> member "stderr")

let check_confirmed_timeout ~mutant_id report =
  let label = "confirmed timeout" in
  let open Yojson.Safe.Util in
  check_completed_report label report;
  expect_string "timeout selection" ("mutants:" ^ mutant_id)
    (report |> member "selection" |> member "description");
  let summary = report |> member "summary" in
  expect_string "timeout summary kind" "complete" (summary |> member "kind");
  expect_int "timeout executed" 1 (summary |> member "executed");
  expect_int "timeout not_run" 0 (summary |> member "not_run");
  expect_int "timeout count" 1 (summary |> member "timeout");
  expect_int "timeout errors" 0 (summary |> member "error");
  expect_int "timeout inconclusive" 0 (summary |> member "inconclusive");
  let result =
    report |> member "mutants" |> to_list |> singleton "timeout mutants"
  in
  expect_string "timeout mutant ID" mutant_id
    (result |> member "mutant" |> member "full_id");
  expect_string "timeout outcome" "timeout" (result |> member "outcome");
  expect_bool "timeout confirmed" true (result |> member "timeout_confirmed");
  expect_bool "timeout cached" false (result |> member "cached");
  expect_null "timeout expectation" (result |> member "expectation");
  let retry = result |> member "timeout_retry" in
  let retry_fields =
    match retry with
    | `Assoc fields -> List.map fst fields |> List.sort String.compare
    | value ->
        fail "timeout_retry was not an object: %s" (Yojson.Safe.to_string value)
  in
  if retry_fields <> [ "initial_timeout"; "serial_retry" ] then
    fail "timeout_retry did not contain exactly two structured attempts: %s"
      (String.concat "," retry_fields);
  check_timeout_attempt "initial timeout" mutant_id
    (retry |> member "initial_timeout");
  check_timeout_attempt "serial timeout retry" mutant_id
    (retry |> member "serial_retry");
  let short_id = String.sub mutant_id 0 20 in
  let stdout = result |> member "stdout" |> member "contents" |> to_string in
  let stderr = result |> member "stderr" |> member "contents" |> to_string in
  if
    count_occurrences ~needle:("timeout-contract-stdout:" ^ short_id) stdout
    <> 2
  then
    fail "final stdout did not merge exactly both timeout attempts: %S" stdout;
  if
    count_occurrences ~needle:("timeout-contract-stderr:" ^ short_id) stderr
    <> 2
  then
    fail "final stderr did not merge exactly both timeout attempts: %S" stderr

let check_expectation label ~id ~reason ~status report =
  let open Yojson.Safe.Util in
  let evaluation =
    report |> member "expectations" |> to_list
    |> singleton (label ^ " expectation ledger")
  in
  expect_string (label ^ " expectation ID") id (evaluation |> member "mutant_id");
  expect_string
    (label ^ " expectation reason")
    reason
    (evaluation |> member "reason");
  expect_string
    (label ^ " expectation status")
    status
    (evaluation |> member "status");
  expect_null (label ^ " expectation detail") (evaluation |> member "detail")

let check_stale_expectation ~id ~reason report =
  let label = "stale expectation" in
  let open Yojson.Safe.Util in
  check_completed_report label report;
  expect_string "stale selection" "all"
    (report |> member "selection" |> member "description");
  check_expectation label ~id ~reason ~status:"stale" report;
  let summary = report |> member "summary" in
  expect_string "stale summary kind" "complete" (summary |> member "kind");
  expect_int "stale not_run" 0 (summary |> member "not_run");
  expect_int "stale unexpected survivors" 0
    (summary |> member "unexpected_survivors");
  let warnings = report |> member "warnings" |> to_list in
  let warning =
    List.find_opt
      (fun warning ->
        match warning |> member "code" with
        | `String code -> String.equal code "stale-expectation"
        | _ -> false)
      warnings
  in
  match warning with
  | None -> fail "stale expectation had no structured warning"
  | Some warning ->
      let message = warning |> member "message" |> to_string in
      if
        not
          (contains_substring ~needle:id message
          && contains_substring ~needle:reason message)
      then fail "stale warning lost its ID or reason: %S" message

let check_not_evaluated_expectation ~selected_id ~expected_id ~reason report =
  let label = "partial expectation" in
  let open Yojson.Safe.Util in
  check_completed_report label report;
  expect_string "partial selection" ("mutants:" ^ selected_id)
    (report |> member "selection" |> member "description");
  check_expectation label ~id:expected_id ~reason ~status:"not-evaluated" report;
  let summary = report |> member "summary" in
  expect_string "partial-selection summary kind" "complete"
    (summary |> member "kind");
  expect_int "partial-selection executed" 1 (summary |> member "executed");
  expect_int "partial-selection not_run" 0 (summary |> member "not_run");
  expect_int "partial-selection killed" 1 (summary |> member "killed");
  expect_int "partial-selection unfulfilled expectations" 0
    (summary |> member "unfulfilled_expectations");
  let result =
    report |> member "mutants" |> to_list
    |> singleton "partial-selection mutants"
  in
  expect_string "partial-selection mutant ID" selected_id
    (result |> member "mutant" |> member "full_id");
  expect_string "partial-selection outcome" "killed" (result |> member "outcome");
  expect_null "partial-selection result expectation"
    (result |> member "expectation");
  if
    List.exists
      (fun warning ->
        match warning |> member "code" with
        | `String code -> String.equal code "stale-expectation"
        | _ -> false)
      (report |> member "warnings" |> to_list)
  then fail "partial selection incorrectly emitted a stale-expectation warning"

let run_contract cli executable =
  let layout = create_layout () in
  Fun.protect
    ~finally:(fun () -> cleanup layout)
    (fun () ->
      write
        (Filename.concat layout.workspace "dune-project")
        "(lang dune 3.21)\n(name cli_lifecycle_contract)\n";
      write
        (Filename.concat layout.workspace "dune")
        {|(library
 (name subject)
 (modules subject))
|};
      write
        (Filename.concat layout.workspace "subject.ml")
        {|let first = true
let second = false
let choose value = if value then first else second
|};
      let before = source_digest layout in
      let catalog, selected_id, expected_id = catalog_ids ~cli layout in

      write_config layout ~stage_flag:timeout_stage_flag
        ~stage_name:"confirmed-timeout" executable;
      run_json ~cli layout ~label:"confirmed timeout" ~exit_code:0
        [ "--mutant"; selected_id ]
      |> check_confirmed_timeout ~mutant_id:selected_id;

      let stale_id = absent_full_id catalog in
      let stale_reason =
        "Deliberately absent ID used to prove full-selection stale handling."
      in
      write_config layout ~stage_flag:killing_stage_flag
        ~stage_name:"kill-selected" ~expectation:(stale_id, stale_reason)
        executable;
      run_json ~cli layout ~label:"stale expectation" ~exit_code:2 []
      |> check_stale_expectation ~id:stale_id ~reason:stale_reason;

      let partial_reason =
        "Selected out deliberately to prove partial expectation handling."
      in
      write_config layout ~stage_flag:killing_stage_flag
        ~stage_name:"kill-selected"
        ~expectation:(expected_id, partial_reason)
        executable;
      run_json ~cli layout ~label:"not-evaluated expectation" ~exit_code:0
        [ "--mutant"; selected_id ]
      |> check_not_evaluated_expectation ~selected_id ~expected_id
           ~reason:partial_reason;

      let after = source_digest layout in
      if not (String.equal before after) then
        fail "real CLI lifecycle runs changed the source workspace")

(* The run, report, and cache subcommands must resolve the same store: the
   configured [cache.directory] applies to every command, not only [run]. *)
let store_resolution_contract cli executable =
  let layout = create_layout () in
  Fun.protect
    ~finally:(fun () -> cleanup layout)
    (fun () ->
      let cache_root = Filename.concat layout.parent "cache-root" in
      Unix.mkdir cache_root 0o700;
      (* The native store materializes at most one new path component, so the
         default cache root must already exist below the redirected OS root. *)
      Unix.mkdir (Filename.concat cache_root "ocaml-mutants") 0o700;
      let extra_env =
        if Sys.win32 then [ ("LOCALAPPDATA", Some cache_root) ]
        else [ ("XDG_CACHE_HOME", Some cache_root) ]
      in
      write
        (Filename.concat layout.workspace "dune-project")
        "(lang dune 3.21)\n(name store_resolution_contract)\n";
      write
        (Filename.concat layout.workspace "dune")
        {|(library
 (name subject)
 (modules subject))
|};
      write
        (Filename.concat layout.workspace "subject.ml")
        {|let first = true
let second = false
let choose value = if value then first else second
|};
      let _, selected_id, _ = catalog_ids ~extra_env ~cli layout in
      write_config layout ~stage_flag:killing_stage_flag
        ~stage_name:"kill-selected" ~cache_directory:"team-cache" executable;
      run_json ~extra_env ~cli layout ~label:"store-resolution run" ~exit_code:0
        [ "--mutant"; selected_id ]
      |> check_completed_report "store-resolution run";
      let report =
        run_cli ~extra_env ~cli layout
          [ "report"; "latest"; "--json"; "--no-color" ]
      in
      expect_exit "store-resolution report" 0 report;
      let open Yojson.Safe.Util in
      expect_string "store-resolution report document type"
        "ocaml-mutants.run-report-v1"
        (parse_json "store-resolution report" report |> member "document_type");
      let stats =
        run_cli ~extra_env ~cli layout
          [ "cache"; "stats"; "--path"; layout.workspace ]
      in
      expect_exit "store-resolution cache stats" 0 stats;
      if not (contains_substring ~needle:"team-cache" stats.stdout) then
        fail "cache stats did not resolve the configured directory: %S"
          stats.stdout)

let () =
  if Engine.Process_supervisor.helper_requested Sys.argv then
    exit (Engine.Process_supervisor.run_helper Sys.argv)
  else if
    Array.length Sys.argv = 2 && String.equal Sys.argv.(1) timeout_stage_flag
  then run_timeout_stage ()
  else if
    Array.length Sys.argv = 2 && String.equal Sys.argv.(1) killing_stage_flag
  then run_killing_stage ()
  else (
    Engine.Process_supervisor.configure_helper_executable
      (Unix.realpath Sys.executable_name);
    try
      if Array.length Sys.argv <> 2 then
        fail "expected the ocaml-mutants CLI path as the only argument";
      let cli = Unix.realpath Sys.argv.(1) in
      let executable = Unix.realpath Sys.executable_name in
      run_contract cli executable;
      store_resolution_contract cli executable
    with Contract_failure message ->
      prerr_endline message;
      exit 1)
