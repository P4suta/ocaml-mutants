open Util
module Core = Ocaml_mutants_core

type output = Application_request.output =
  | Terminal of { quiet : bool; color : bool }
  | Json
  | Stryker_json of Stryker_report.thresholds

type selection = Application_request.selection =
  | All
  | Changed
  | Changed_from of string
  | Mutants of string list
  | Shard of Application_request.shard_selection
  | Rerun of { parent_run_id : string; mutant_id : string }

module Workspace = struct
  type snapshot = Workspace_snapshot.t

  type 'a bracket_outcome = 'a Workspace_snapshot.bracket_outcome =
    | Acquisition_failed of Error.t
    | Action_returned of 'a * (unit, Error.t) result
    | Action_raised of exn * Printexc.raw_backtrace * (unit, Error.t) result

  let bracket = Workspace_snapshot.bracket
  let root = Workspace_snapshot.root
  let manifest_digest = Workspace_snapshot.manifest_digest
end

let interrupted phase message =
  Error.create ~phase ~cause:Error.Interrupted_by_user "%s" message

let command_version ~cancel root command =
  let result = Process_supervisor.run ~cancel ~cwd:root ~env:[] command in
  match result.status with
  | Process_supervisor.Cancelled ->
      Error (interrupted Error.Analysis "toolchain detection was interrupted")
  | _ when Process_supervisor.succeeded result ->
      Ok (String.trim (result.stdout ^ result.stderr))
  | _ -> Ok (Process_supervisor.status_string result.status)

let toolchain ~cancel ~root =
  let* ocaml = command_version ~cancel root [ "ocamlc"; "-version" ] in
  let* dune = command_version ~cancel root [ "dune"; "--version" ] in
  let* opam_switch =
    command_version ~cancel root [ "opam"; "switch"; "show"; "--safe" ]
  in
  let* opam_closure =
    command_version ~cancel root
      [ "opam"; "list"; "--installed"; "--columns=name,version"; "--short" ]
  in
  Ok
    (Printf.sprintf "ocaml=%s; dune=%s; opam-switch=%s; opam=%s" ocaml dune
       opam_switch opam_closure)

let canonical_environment () =
  Unix.environment () |> Array.to_list |> List.sort String.compare
  |> String.concat "\000"

let test_command = Test_command.resolve

let mutant_environment ~root mutant =
  [
    ("OCAML_MUTANTS_ACTIVE", Some (Core.Mutant.Id.full (Core.Mutant.id mutant)));
    (* A nested ocaml-mutants process may itself be running under an outer
       readiness probe. Mutant attempts do not collect coverage, so retaining
       that outer hit file would mix the inner catalog into the outer one. *)
    ("OCAML_MUTANTS_HIT_FILE", None);
    (Core.Instrumentation.hit_owner_environment, None);
  ]
  @ Test_command.dune_cache_environment ~root

let duration_exn seconds =
  match Core.Duration.of_seconds seconds with
  | Ok value -> value
  | Error message -> invalid_arg message

type readiness = {
  duration : Core.Duration.t;
  hit_map : Run_store.hit_map_entry list;
}

let read_hit_file ~catalog path =
  let known = Hashtbl.create (Core.Catalog.length catalog) in
  Core.Catalog.to_list catalog
  |> List.iter (fun mutant ->
      Hashtbl.replace known (Core.Mutant.Id.full (Core.Mutant.id mutant)) ());
  match read_file path with
  | Error message ->
      Error
        (Error.create ~phase:Error.Ready_proof ~cause:Error.Io_failure
           ~context:[ ("path", path) ]
           "cannot read runtime hit file: %s" message)
  | Ok contents -> (
      let ids =
        split_lines contents |> List.map String.trim
        |> List.filter (fun value -> value <> "")
        |> List.sort_uniq String.compare
      in
      match List.find_opt (fun id -> not (Hashtbl.mem known id)) ids with
      | None -> Ok ids
      | Some id ->
          Error
            (Error.create ~phase:Error.Ready_proof
               ~cause:Error.Invariant_violation
               ~context:[ ("path", path); ("mutant_id", id) ]
               "runtime hit file contains an unknown mutant ID"))

let remove_hit_file path =
  try Sys.remove (windows_extended_path path) with Sys_error _ -> ()

let dune_exhaustive_stage_name = "dune:@runtest"

let classify_exhaustive_hits config hit_map =
  if config.Config.test.driver <> Config.Dune_driver then hit_map
  else
    let classified = Hashtbl.create 64 in
    List.iter
      (fun (entry : Run_store.hit_map_entry) ->
        if not (String.equal entry.test dune_exhaustive_stage_name) then
          List.iter
            (fun id -> Hashtbl.replace classified id ())
            entry.mutant_ids)
      hit_map;
    List.map
      (fun (entry : Run_store.hit_map_entry) ->
        if String.equal entry.test dune_exhaustive_stage_name then
          {
            entry with
            mutant_ids =
              List.filter
                (fun id -> not (Hashtbl.mem classified id))
                entry.mutant_ids;
          }
        else entry)
      hit_map

let run_readiness ~cancel ~root ~config ~catalog ~build_dir ~timeout =
  let hit_owner = Ocaml_frontend.instrumentation_owner catalog in
  let rec run stage_index duration hit_map = function
    | [] ->
        Ok
          {
            duration = duration_exn duration;
            hit_map = List.rev hit_map |> classify_exhaustive_hits config;
          }
    | (stage : Config.stage) :: rest ->
        let hit_path =
          Filename.concat root
            (Printf.sprintf ".ocaml-mutants-hits-%d-%d.log" (Unix.getpid ())
               stage_index)
        in
        let* () =
          write_file hit_path ""
          |> Result.map_error (fun message ->
              Error.create ~phase:Error.Ready_proof ~cause:Error.Io_failure
                ~context:[ ("path", hit_path) ]
                "cannot initialize runtime hit file: %s" message)
        in
        let result =
          Process_supervisor.run ~cancel
            ?timeout:(Option.map Core.Duration.to_seconds timeout)
            ~cwd:root
            ~env:
              (("OCAML_MUTANTS_ACTIVE", None)
              :: ("OCAML_MUTANTS_HIT_FILE", Some hit_path)
              :: (Core.Instrumentation.hit_owner_environment, Some hit_owner)
              :: Test_command.dune_cache_environment ~root)
            (test_command stage.command
               (Printf.sprintf "%s-stage-%d" build_dir stage_index))
        in
        let hits = read_hit_file ~catalog hit_path in
        remove_hit_file hit_path;
        if result.status = Process_supervisor.Cancelled then
          Error
            (interrupted Error.Ready_proof
               "instrumented readiness proof was interrupted")
        else if Process_supervisor.succeeded result then
          let* mutant_ids = hits in
          run (stage_index + 1)
            (max duration result.duration)
            ({ Run_store.test = stage.name; mutant_ids } :: hit_map)
            rest
        else
          Error
            (Error.create ~phase:Error.Ready_proof ~cause:Error.Baseline_failure
               ~context:
                 [
                   ("stage", stage.name);
                   ("status", Process_supervisor.status_string result.status);
                 ]
               "instrumented readiness stage %S failed:\n%s%s" stage.name
               result.stdout result.stderr)
  in
  run 0 0. [] config.Config.test.stages

let selection_name = function
  | All -> "all"
  | Changed -> "changed-from-upstream"
  | Changed_from revision -> "changed-from:" ^ revision
  | Mutants ids -> "mutants:" ^ String.concat "," ids
  | Shard shard ->
      Printf.sprintf "shard:%s:%s:%d:%d" shard.plan_id shard.input_fingerprint
        shard.index shard.count
  | Rerun rerun ->
      Printf.sprintf "rerun:%s:%s" rerun.parent_run_id rerun.mutant_id

let changed_filter ~cancel ~root = function
  | All -> Ok (fun _ -> true)
  | Changed ->
      let* files = Changed.files ~cancel ~root ~from:None in
      Ok (fun path -> List.mem path files)
  | Changed_from revision ->
      let* files = Changed.files ~cancel ~root ~from:(Some revision) in
      Ok (fun path -> List.mem path files)
  | Mutants _ | Shard _ | Rerun _ -> Ok (fun _ -> true)

let starts_with ~prefix value =
  String.length prefix <= String.length value
  && String.sub value 0 (String.length prefix) = prefix

let select_explicit catalog requested =
  let candidates = Core.Catalog.to_list catalog in
  let rec resolve selected = function
    | [] -> (
        match Core.Catalog.of_list (List.rev selected) with
        | Ok catalog -> Ok catalog
        | Error collision ->
            Error
              (Error.create ~phase:Error.Analysis
                 ~cause:Error.Invariant_violation
                 "explicit mutant selection collided: %a" Core.Catalog.pp_error
                 collision))
    | prefix :: _ when not (Core.Mutant.Id.is_valid_prefix prefix) ->
        Error
          (Error.create ~phase:Error.Cli ~cause:Error.Invalid_input
             "invalid mutant ID prefix %S" prefix)
    | prefix :: rest -> (
        let matches =
          List.filter
            (fun mutant ->
              starts_with ~prefix (Core.Mutant.Id.full (Core.Mutant.id mutant)))
            candidates
        in
        match matches with
        | [] ->
            Error
              (Error.create ~phase:Error.Cli ~cause:Error.Invalid_input
                 "no mutant ID matches %S" prefix)
        | [ mutant ] ->
            if
              List.exists
                (fun existing ->
                  Core.Mutant.Id.equal (Core.Mutant.id existing)
                    (Core.Mutant.id mutant))
                selected
            then resolve selected rest
            else resolve (mutant :: selected) rest
        | _ ->
            Error
              (Error.create ~phase:Error.Cli ~cause:Error.Invalid_input
                 "mutant ID prefix %S is ambiguous (%d matches)" prefix
                 (List.length matches)))
  in
  resolve [] requested

type analysis = {
  snapshot : Workspace.snapshot;
  catalog : Core.Catalog.t;
  skipped : Run_store.skip_summary list;
  workspace_digest : string;
  toolchain : string;
  config : Config.t;
  baseline_config : Config.t;
  tests : Dune_adapter.described_test list;
}

type test_plan = { config : Config.t; baseline_config : Config.t }

let argv_exn values =
  match Core.Nonempty_argv.of_list values with
  | Ok command -> command
  | Error message -> invalid_arg message

let safe_test_component value =
  value <> "" && value <> "." && value <> ".."
  && (not (String.contains value '/'))
  && not (String.contains value '\\')

let dune_stage_of_test (test : Dune_adapter.described_test) =
  let source_dir = Core.Mutant.normalize_path test.source_dir in
  let components =
    String.split_on_char '/' source_dir
    |> List.filter (fun component -> component <> "" && component <> ".")
  in
  if
    (not (Filename.is_relative source_dir))
    || List.exists (String.equal "..") components
    || not (safe_test_component test.name)
  then
    Error
      (Error.create ~phase:Error.Dune ~cause:Error.Decode_failure
         ~context:[ ("test", test.name); ("source_dir", test.source_dir) ]
         "dune test inventory contains an unsafe alias component")
  else
    let alias =
      let name = "runtest-" ^ test.name in
      if components = [] then "@" ^ name
      else "@" ^ String.concat "/" components ^ "/" ^ name
    in
    Ok
      {
        Config.name = "dune:" ^ alias;
        command = argv_exn [ "dune"; "build"; "--force"; alias ];
      }

let resolve_test_plan config tests =
  let use_dune =
    match config.Config.test.driver with
    | Config.Dune_driver -> true
    | Config.Command_driver -> false
    | Config.Auto_driver ->
        List.for_all
          (fun (stage : Config.stage) ->
            Test_command.dune_managed stage.command)
          config.Config.test.stages
  in
  if not use_dune then
    let config =
      { config with test = { config.test with driver = Config.Command_driver } }
    in
    Ok { config; baseline_config = config }
  else
    let* individual =
      let rec convert converted = function
        | [] -> Ok (List.rev converted)
        | test :: rest ->
            let* stage = dune_stage_of_test test in
            convert (stage :: converted) rest
      in
      convert [] tests
    in
    let individual =
      List.sort_uniq
        (fun (left : Config.stage) right -> String.compare left.name right.name)
        individual
    in
    let exhaustive =
      {
        Config.name = dune_exhaustive_stage_name;
        command = argv_exn [ "dune"; "runtest"; "--force" ];
      }
    in
    let effective_test =
      {
        config.Config.test with
        driver = Config.Dune_driver;
        command = exhaustive.command;
        stages = individual @ [ exhaustive ];
      }
    in
    let config = { config with test = effective_test } in
    let baseline_config =
      { config with test = { effective_test with stages = [ exhaustive ] } }
    in
    Ok { config; baseline_config }

let analyze ~cancel ~root ~config ~selection ~snapshot =
  let root = Unix.realpath root in
  if not (Sys.file_exists (Filename.concat root "dune-project")) then
    Error
      (Error.create ~phase:Error.Cli ~cause:Error.Invalid_input
         "%s is not a Dune workspace (dune-project not found)" root)
  else
    let* selected_by_change = changed_filter ~cancel ~root selection in
    let* toolchain = toolchain ~cancel ~root in
    let workspace_digest = Workspace.manifest_digest snapshot in
    let fail error = Error error in
    match Dune_adapter.describe ~cancel ~root:(Workspace.root snapshot) with
    | Error error -> fail error
    | Ok workspace -> (
        if workspace.source_files = [] then
          fail
            (Error.create ~phase:Error.Dune ~cause:Error.Decode_failure
               "dune describe returned no implementation files")
        else
          match
            Dune_adapter.describe_tests ~cancel ~root:(Workspace.root snapshot)
          with
          | Error error -> fail error
          | Ok tests -> (
              let* test_plan = resolve_test_plan config tests in
              let config = test_plan.config in
              match
                Dune_adapter.build_analysis ~cancel
                  ~root:(Workspace.root snapshot)
                  ~build_dir:".ocaml-mutants-analysis"
                  ~cmt_targets:workspace.cmt_targets
              with
              | Error error -> fail error
              | Ok _ -> (
                  let workspace_sources =
                    Hashtbl.create (List.length workspace.source_files)
                  in
                  List.iter
                    (fun path -> Hashtbl.replace workspace_sources path ())
                    workspace.source_files;
                  let selected_source path =
                    Hashtbl.mem workspace_sources path
                    && string_ends_with ~suffix:".ml" path
                    && Dune_adapter.source_role ~workspace ~tests path
                       = Dune_adapter.Production
                    && Glob.selected ~include_:config.Config.mutation.include_
                         ~exclude:config.Config.mutation.exclude path
                    && selected_by_change path
                  in
                  match
                    Ocaml_frontend.discover ~root:(Workspace.root snapshot)
                      ~cmt_files:
                        (Dune_adapter.cmt_files ~root:(Workspace.root snapshot)
                           ~build_dir:".ocaml-mutants-analysis")
                      ~selected_source
                      ~operators:
                        (match selection with
                        | Mutants _ | Shard _ | Rerun _ ->
                            Core.Operator.Family.all
                        | _ when config.Config.mutation.expectations <> [] ->
                            Core.Operator.Family.all
                        | _ -> config.Config.mutation.operators)
                  with
                  | Error error -> fail error
                  | Ok discovery ->
                      let expected_ids = Hashtbl.create 16 in
                      List.iter
                        (fun (expectation : Config.expectation) ->
                          Hashtbl.replace expected_ids expectation.id ())
                        config.Config.mutation.expectations;
                      let profiled =
                        Core.Catalog.filter
                          (fun mutant ->
                            let expected =
                              Hashtbl.mem expected_ids
                                (Core.Mutant.Id.full (Core.Mutant.id mutant))
                            in
                            let configured_family =
                              List.mem
                                (Core.Mutant.family mutant)
                                config.Config.mutation.operators
                            in
                            expected
                            || configured_family
                               && Core.Operator.Profile.includes
                                    config.Config.mutation.profile
                                    (Core.Operator.Rule.profile
                                       (Core.Mutant.rule mutant)))
                          discovery.catalog
                      in
                      let* catalog =
                        match selection with
                        | Mutants requested ->
                            select_explicit discovery.catalog requested
                        | Shard shard ->
                            let actual_fingerprint =
                              Shard_plan.input_fingerprint ~workspace_digest
                                ~toolchain ~config ~catalog:profiled
                            in
                            if
                              not
                                (String.equal actual_fingerprint
                                   shard.input_fingerprint)
                            then
                              Error
                                (Error.create ~phase:Error.Analysis
                                   ~cause:Error.Invalid_input
                                   ~context:
                                     [
                                       ( "planned_fingerprint",
                                         shard.input_fingerprint );
                                       ("actual_fingerprint", actual_fingerprint);
                                     ]
                                   "shard plan no longer matches workspace or \
                                    resolved configuration")
                            else select_explicit profiled shard.mutant_ids
                        | Rerun rerun ->
                            select_explicit discovery.catalog
                              [ rerun.mutant_id ]
                        | _ -> Ok profiled
                      in
                      let skipped : Run_store.skip_summary list =
                        List.map
                          (fun (skip : Ocaml_frontend.skip_summary) ->
                            {
                              Run_store.reason =
                                Ocaml_frontend.skip_reason_name skip.reason;
                              count = skip.count;
                              examples = skip.examples;
                            })
                          discovery.skipped
                        |> List.sort
                             (fun (left : Run_store.skip_summary) right ->
                               String.compare left.Run_store.reason right.reason)
                      in
                      Ok
                        {
                          snapshot;
                          catalog;
                          skipped;
                          workspace_digest;
                          toolchain;
                          config;
                          baseline_config = test_plan.baseline_config;
                          tests;
                        })))

let catalog_json analysis selection profile =
  let selection = selection_name selection in
  `Assoc
    [
      ("document_type", `String "ocaml-mutants.catalog-v2");
      ("schema_version", `Int 2);
      ( "workspace",
        `Assoc
          [
            ("digest", `String analysis.workspace_digest);
            ("toolchain", `String analysis.toolchain);
          ] );
      ( "input_fingerprint",
        `Assoc
          [
            ( "digest",
              `String
                (sha256
                   (String.concat "\000"
                      [
                        "ocaml-mutants-catalog-input-v2";
                        analysis.workspace_digest;
                        analysis.toolchain;
                        Core.Operator.Profile.name profile;
                        selection;
                      ])) );
            ("workspace_digest", `String analysis.workspace_digest);
          ] );
      ("profile", `String (Core.Operator.Profile.name profile));
      ("selection", `String selection);
      ( "mutants",
        `List
          (Core.Catalog.to_list analysis.catalog
          |> List.map (fun mutant ->
              `Assoc
                [
                  ("id", `String (Core.Mutant.Id.short (Core.Mutant.id mutant)));
                  ( "full_id",
                    `String (Core.Mutant.Id.full (Core.Mutant.id mutant)) );
                  ("lineage_id", `String (Core.Mutant.lineage_id mutant));
                  ("path", `String (Core.Mutant.path mutant));
                  ("range", Run_store.range_to_json (Core.Mutant.range mutant));
                  ( "family",
                    `String
                      (Core.Operator.Family.name (Core.Mutant.family mutant)) );
                  ( "rule",
                    `String
                      (Core.Operator.Rule.stable_name (Core.Mutant.rule mutant))
                  );
                ])) );
      ( "skips",
        `List
          (List.map
             (fun (skip : Run_store.skip_summary) ->
               `Assoc
                 [
                   ("reason", `String skip.reason);
                   ("count", `Int skip.count);
                   ( "examples",
                     `List (List.map (fun value -> `String value) skip.examples)
                   );
                 ])
             analysis.skipped) );
    ]

let list_mutants ~cancel ~root ~config ~selection ~output =
  Workspace_snapshot.with_snapshot root (fun snapshot ->
      let* analysis = analyze ~cancel ~root ~config ~selection ~snapshot in
      match output with
      | Json ->
          Yojson.Safe.pretty_to_channel stdout
            (catalog_json analysis selection
               analysis.config.Config.mutation.profile);
          Ok 0
      | Terminal { quiet = false; _ } ->
          Report.print_catalog Format.std_formatter analysis.catalog;
          Report.print_skipped Format.std_formatter analysis.skipped;
          Ok 0
      | Terminal { quiet = true; _ } -> Ok 0
      | Stryker_json _ ->
          Error
            (Error.make Error.Usage
               "Stryker-compatible output is only available for run reports"))

let create_shard_plan ~cancel ~root ~config ~selection ~shard_count ~durations =
  Workspace_snapshot.with_snapshot root (fun snapshot ->
      let* analysis = analyze ~cancel ~root ~config ~selection ~snapshot in
      Shard_plan.create ~workspace_digest:analysis.workspace_digest
        ~toolchain:analysis.toolchain ~config:analysis.config
        ~catalog:analysis.catalog ~shard_count ~durations
      |> Result.map_error (fun message ->
          Error.create ~phase:Error.Cli ~cause:Error.Invalid_input "%s" message))

type deep_diagnostic = {
  mutants : int;
  tests : int;
  baseline_seconds : float;
  timeout_seconds : float;
}

let doctor_deep ~cancel ~root ~config =
  Workspace_snapshot.with_snapshot root (fun snapshot ->
      let snapshot_root = Workspace.root snapshot in
      let* analysis = analyze ~cancel ~root ~config ~selection:All ~snapshot in
      let config = analysis.config in
      match
        Baseline.run ~cancel ~root:snapshot_root
          ~config:analysis.baseline_config
          ~build_dir:".ocaml-mutants-doctor-baseline"
      with
      | Baseline.Incomplete incomplete -> Error incomplete.error
      | Baseline.Completed complete ->
          let baseline = Core.Duration.to_seconds complete.slowest in
          let* timeout =
            match config.test.timeout with
            | Some timeout when Core.Duration.to_seconds timeout > baseline ->
                Ok timeout
            | Some _ ->
                Error
                  (Error.create ~phase:Error.Baseline_proof
                     ~cause:Error.Invalid_input
                     "configured timeout is not above the measured baseline")
            | None -> Ok (duration_exn (max 10. (baseline *. 5.)))
          in
          let* _ =
            Ocaml_frontend.instrument_files ~root:snapshot_root analysis.catalog
          in
          let* _ =
            run_readiness ~cancel ~root:snapshot_root ~config
              ~catalog:analysis.catalog
              ~build_dir:".ocaml-mutants-doctor-instrumented"
              ~timeout:(Some timeout)
          in
          Ok
            {
              mutants = Core.Catalog.length analysis.catalog;
              tests = List.length analysis.tests;
              baseline_seconds = baseline;
              timeout_seconds = Core.Duration.to_seconds timeout;
            })

let original_sources ~root catalog =
  let table = Hashtbl.create 32 in
  let rec read = function
    | [] -> Ok table
    | mutant :: rest -> (
        let path = Core.Mutant.path mutant in
        if Hashtbl.mem table path then read rest
        else
          match read_file (Filename.concat root path) with
          | Ok contents ->
              Hashtbl.add table path (Core.Source.of_string contents);
              read rest
          | Error message ->
              Error
                (Error.create ~phase:Error.Analysis ~cause:Error.Io_failure
                   ~context:[ ("path", path) ]
                   "cannot retain original source: %s" message))
  in
  read (Core.Catalog.to_list catalog)

type attempt = {
  outcome : Core.Outcome.t;
  duration : float;
  stages : Run_store.stage_result list;
  stdout : Run_store.captured;
  stderr : Run_store.captured;
}

let replace_literal ~needle value =
  let needle_length = String.length needle in
  if needle_length = 0 then value
  else
    let replacement = String.make needle_length '*' in
    let buffer = Buffer.create (String.length value) in
    let rec copy offset =
      match String.index_from_opt value offset needle.[0] with
      | None ->
          Buffer.add_substring buffer value offset (String.length value - offset)
      | Some index ->
          Buffer.add_substring buffer value offset (index - offset);
          if
            index + needle_length <= String.length value
            && String.sub value index needle_length = needle
          then (
            Buffer.add_string buffer replacement;
            copy (index + needle_length))
          else (
            Buffer.add_char buffer value.[index];
            copy (index + 1))
    in
    copy 0;
    Buffer.contents buffer

let redact literals value =
  List.fold_left
    (fun value needle -> replace_literal ~needle value)
    value literals

let retain_capture ~limit ~truncated ~total_bytes contents =
  let length = String.length contents in
  let contents =
    if length <= limit then contents
    else
      let head = limit / 2 in
      let tail = limit - head in
      String.sub contents 0 head ^ String.sub contents (length - tail) tail
  in
  Run_store.captured
    ~truncated:(truncated || length > limit || total_bytes > length)
    ~total_bytes contents

let merge_captured ~limit captures =
  let total_bytes =
    List.fold_left
      (fun total captured -> total + captured.Run_store.total_bytes)
      0 captures
  in
  let contents =
    List.map (fun captured -> captured.Run_store.contents) captures
    |> String.concat ""
  in
  retain_capture ~limit
    ~truncated:
      (List.exists (fun captured -> captured.Run_store.truncated) captures)
    ~total_bytes contents

let run_attempt ~cancel ~root ~config ~worker_id ~timeout ~stages mutant =
  let rec run stage_index duration stages stdout stderr = function
    | [] ->
        {
          outcome = Core.Outcome.Survived;
          duration;
          stages = List.rev stages;
          stdout =
            merge_captured ~limit:config.Config.privacy.stdout_limit_bytes
              (List.rev stdout);
          stderr =
            merge_captured ~limit:config.Config.privacy.stderr_limit_bytes
              (List.rev stderr);
        }
    | (stage : Config.stage) :: rest -> (
        let process =
          Process_supervisor.run ~cancel
            ~timeout:(Core.Duration.to_seconds timeout)
            ~stdout_limit:config.Config.privacy.stdout_limit_bytes
            ~stderr_limit:config.Config.privacy.stderr_limit_bytes ~cwd:root
            ~env:(mutant_environment ~root mutant)
            (test_command stage.command
               (Printf.sprintf ".ocaml-mutants-worker-%d-stage-%d" worker_id
                  stage_index))
        in
        let stage_result =
          {
            Run_store.name = stage.name;
            status = Process_supervisor.status_string process.status;
            duration = duration_exn process.duration;
          }
        in
        let captured_stdout =
          Run_store.captured ~truncated:process.stdout_truncated
            ~total_bytes:process.stdout_bytes
            (redact config.Config.privacy.redactions process.stdout)
        in
        let captured_stderr =
          Run_store.captured ~truncated:process.stderr_truncated
            ~total_bytes:process.stderr_bytes
            (redact config.Config.privacy.redactions process.stderr)
        in
        let finish outcome =
          {
            outcome;
            duration = duration +. process.duration;
            stages = List.rev (stage_result :: stages);
            stdout =
              merge_captured ~limit:config.Config.privacy.stdout_limit_bytes
                (List.rev (captured_stdout :: stdout));
            stderr =
              merge_captured ~limit:config.Config.privacy.stderr_limit_bytes
                (List.rev (captured_stderr :: stderr));
          }
        in
        match process.status with
        | Process_supervisor.Exited 0 ->
            run (stage_index + 1)
              (duration +. process.duration)
              (stage_result :: stages)
              (captured_stdout :: stdout)
              (captured_stderr :: stderr)
              rest
        | Process_supervisor.Exited _ | Process_supervisor.Signaled _ ->
            finish Core.Outcome.Killed
        | Process_supervisor.Timed_out -> finish Core.Outcome.Timeout
        | Process_supervisor.Cancelled ->
            finish (Core.Outcome.Error "interrupted")
        | Process_supervisor.Spawn_error message ->
            finish (Core.Outcome.Error message))
  in
  run 0 0. [] [] [] stages

let result_of_attempt mutant attempt =
  {
    Run_store.mutant;
    outcome = attempt.outcome;
    duration = duration_exn attempt.duration;
    cached = false;
    evidence_origin = Run_store.Execution;
    stages = attempt.stages;
    timeout_confirmed = false;
    timeout_retry = None;
    expected_reason = None;
    stdout = attempt.stdout;
    stderr = attempt.stderr;
  }

let retry_attempt_of_attempt (attempt : attempt) : Run_store.retry_attempt =
  {
    outcome = attempt.outcome;
    duration = duration_exn attempt.duration;
    stages = attempt.stages;
    stdout = attempt.stdout;
    stderr = attempt.stderr;
  }

let retry_attempt_of_result (result : Run_store.mutant_result) :
    Run_store.retry_attempt =
  {
    outcome = result.outcome;
    duration = result.duration;
    stages = result.stages;
    stdout = result.stdout;
    stderr = result.stderr;
  }

let outcome_label (result : Run_store.mutant_result) =
  match result.Run_store.outcome with
  | Core.Outcome.Killed -> "killed"
  | Core.Outcome.Survived -> "survived"
  | Core.Outcome.Timeout ->
      if result.timeout_confirmed then "timeout" else "timeout-unconfirmed"
  | Core.Outcome.Inconclusive _ -> "inconclusive"
  | Core.Outcome.Error _ -> "error"

let requires_timeout_confirmation (result : Run_store.mutant_result) =
  result.outcome = Core.Outcome.Timeout && not result.timeout_confirmed

let describe_result result =
  let mutant = result.Run_store.mutant in
  Format.asprintf "%s %s:%a %s (%.1fs)%s%s" (outcome_label result)
    (Core.Mutant.path mutant) Core.Source_range.pp (Core.Mutant.range mutant)
    (Core.Operator.Rule.stable_name (Core.Mutant.rule mutant))
    (Core.Duration.to_seconds result.duration)
    (if result.cached then " cached" else "")
    (match result.expected_reason with None -> "" | Some _ -> " expected")

type stage_selection = { stages : Config.stage list; omitted : bool }

let stages_for_mutant config hit_map mutant =
  let id = Core.Mutant.Id.full (Core.Mutant.id mutant) in
  let covering stage =
    List.exists
      (fun (entry : Run_store.hit_map_entry) ->
        String.equal entry.test stage.Config.name
        && List.mem id entry.mutant_ids)
      hit_map
  in
  let covering, remaining = List.partition covering config.Config.test.stages in
  let exhaustive =
    if config.Config.test.driver = Config.Dune_driver then
      List.find_opt
        (fun (stage : Config.stage) ->
          String.equal stage.name dune_exhaustive_stage_name)
        config.Config.test.stages
    else None
  in
  match (config.Config.execution.mode, hit_map, exhaustive) with
  | Config.Strict, _, Some exhaustive | Config.Fast, [], Some exhaustive ->
      let covering =
        List.filter
          (fun (stage : Config.stage) ->
            not (String.equal stage.name dune_exhaustive_stage_name))
          covering
      in
      (* The final @runtest alias executes every Dune-owned test rule. It is the
         exhaustive remainder, so individual non-covering aliases need not run a
         second time before it. *)
      { stages = covering @ [ exhaustive ]; omitted = false }
  | Config.Fast, _, Some exhaustive
    when List.exists
           (fun (stage : Config.stage) ->
             String.equal stage.name exhaustive.name)
           covering ->
      { stages = [ exhaustive ]; omitted = false }
  | Config.Fast, _, Some _ -> { stages = covering; omitted = remaining <> [] }
  | Config.Strict, _, None | Config.Fast, [], None ->
      { stages = covering @ remaining; omitted = false }
  | Config.Fast, _, None -> { stages = covering; omitted = remaining <> [] }

let run_mutants ~cancel ~root ~config ~store ~journal ~key ~sources ~fresh
    ~reuse_allowed ~timeout ~hit_map catalog =
  let mutants = Core.Catalog.to_list catalog |> Array.of_list in
  let result_slots = Array.make (Array.length mutants) None in
  let next = Atomic.make 0 in
  let expectations = Hashtbl.create 16 in
  List.iter
    (fun (expectation : Config.expectation) ->
      Hashtbl.replace expectations expectation.id expectation.reason)
    config.Config.mutation.expectations;
  let expected_reason mutant =
    Hashtbl.find_opt expectations (Core.Mutant.Id.full (Core.Mutant.id mutant))
  in
  let cache_enabled =
    reuse_allowed
    && Config.historical_reuse_enabled config.Config.cache.historical_reuse
  in
  let jobs =
    if
      List.for_all
        (fun (stage : Config.stage) -> Test_command.dune_managed stage.command)
        config.Config.test.stages
      || config.Config.test.parallel_safe
    then
      min
        (Core.Positive_int.to_int config.Config.execution.jobs)
        (max 1 (Array.length mutants))
    else 1
  in
  let progress = Mutex.create () in
  let completed = ref 0 in
  let cache_hits = ref 0 in
  let resume_hits = ref 0 in
  let started = Unix.gettimeofday () in
  let journal_error = ref None in
  let cache_error = ref None in
  let warnings = ref [] in
  let save_now result =
    (* Crash recovery is always on and independent from historical reuse. *)
    (match Run_store.checkpoint_mutant journal result with
    | Ok () -> ()
    | Error error -> if !journal_error = None then journal_error := Some error);
    if
      cache_enabled
      && result.Run_store.expected_reason = None
      && Run_store.cacheable_result result
    then
      match Run_store.save_mutant store ~key result with
      | Ok () -> ()
      | Error error -> if !cache_error = None then cache_error := Some error
  in
  let settle ~resumed ~historical result =
    Mutex.protect progress (fun () ->
        incr completed;
        if resumed then incr resume_hits;
        if historical then incr cache_hits;
        let elapsed_seconds = max 0. (Unix.gettimeofday () -. started) in
        let eta_seconds =
          if !completed = 0 then None
          else
            Some
              (elapsed_seconds /. float_of_int !completed
              *. float_of_int (Array.length mutants - !completed))
        in
        Event_bus.emit
          (Event_bus.Mutant_settled
             {
               result;
               coverage = Run_store.result_coverage_from_hit_map hit_map result;
             });
        Event_bus.emit
          (Event_bus.Progress
             {
               phase = "mutation";
               completed = !completed;
               total = Array.length mutants;
               workers = jobs;
               cache_hits = !cache_hits;
               resume_hits = !resume_hits;
               elapsed_seconds;
               eta_seconds;
             });
        save_now result)
  in
  Event_bus.emit
    (Event_bus.Phase_started
       { phase = "mutation"; total = Some (Array.length mutants) });
  let worker worker_id () =
    let rec loop () =
      let index = Atomic.fetch_and_add next 1 in
      if index < Array.length mutants && not (Cancel.is_requested cancel) then (
        let mutant = mutants.(index) in
        let source = Hashtbl.find sources (Core.Mutant.path mutant) in
        let expected = expected_reason mutant in
        let resumed =
          if fresh then None
          else
            match
              Run_store.load_checkpoint journal ~source ~expected:mutant
            with
            | Ok result -> result
            | Error error ->
                Mutex.protect progress (fun () ->
                    if !journal_error = None then journal_error := Some error);
                None
        in
        let cached =
          match resumed with
          | Some _ as result -> result
          | None ->
              if (not cache_enabled) || expected <> None then None
              else Run_store.load_mutant store ~key ~source ~expected:mutant
        in
        let resumed_hit = Option.is_some resumed in
        let historical_hit = (not resumed_hit) && Option.is_some cached in
        let result =
          match cached with
          | Some cached ->
              let evidence_origin =
                if resumed_hit then
                  if
                    Run_store.result_evidence_level cached = Run_store.Estimated
                  then Run_store.Checkpoint_estimated
                  else Run_store.Checkpoint_resume
                else
                  match config.Config.cache.historical_reuse with
                  | Config.Reuse_estimated -> Run_store.Historical_estimated
                  | Config.Reuse_off | Config.Reuse_exact ->
                      Run_store.Historical_exact
              in
              {
                cached with
                cached = true;
                Run_store.expected_reason = expected;
                evidence_origin;
              }
          | None ->
              let selection = stages_for_mutant config hit_map mutant in
              let attempt =
                run_attempt ~cancel ~root ~config ~worker_id ~timeout
                  ~stages:selection.stages mutant
              in
              let result =
                {
                  (result_of_attempt mutant attempt) with
                  expected_reason = expected;
                }
              in
              if selection.omitted && result.outcome <> Core.Outcome.Killed then
                { result with evidence_origin = Run_store.Fast_estimated }
              else result
        in
        result_slots.(index) <- Some result;
        if not (requires_timeout_confirmation result) then
          settle ~resumed:resumed_hit ~historical:historical_hit result;
        loop ())
    in
    loop ()
  in
  let domains =
    List.init
      (max 0 (jobs - 1))
      (fun index -> Domain.spawn (worker (index + 1)))
  in
  Fun.protect
    ~finally:(fun () -> List.iter Domain.join domains)
    (fun () -> worker 0 ());
  let executed = Array.to_list result_slots |> List.filter_map Fun.id in
  let pending_confirmation =
    List.length (List.filter requires_timeout_confirmation executed)
  in
  if pending_confirmation > 0 && not (Cancel.is_requested cancel) then
    Printf.eprintf "ocaml-mutants: confirming %d timeouts serially\n%!"
      pending_confirmation;
  let confirmations = ref 0 in
  let results =
    List.map
      (fun first ->
        if not (requires_timeout_confirmation first) then first
        else if Cancel.is_requested cancel then (
          settle ~resumed:false ~historical:false first;
          first)
        else
          let selection = stages_for_mutant config hit_map first.mutant in
          let confirmation =
            run_attempt ~cancel ~root ~config ~worker_id:0 ~timeout
              ~stages:selection.stages first.mutant
          in
          let confirmed = result_of_attempt first.mutant confirmation in
          let retried =
            {
              confirmed with
              duration =
                duration_exn
                  (Core.Duration.to_seconds first.duration
                  +. confirmation.duration);
              timeout_retry =
                Some
                  {
                    Run_store.initial_timeout = retry_attempt_of_result first;
                    serial_retry = retry_attempt_of_attempt confirmation;
                  };
              stdout =
                merge_captured ~limit:config.Config.privacy.stdout_limit_bytes
                  [ first.stdout; confirmation.stdout ];
              stderr =
                merge_captured ~limit:config.Config.privacy.stderr_limit_bytes
                  [ first.stderr; confirmation.stderr ];
              expected_reason = first.Run_store.expected_reason;
            }
          in
          let final =
            match confirmation.outcome with
            | Core.Outcome.Timeout -> { retried with timeout_confirmed = true }
            | Core.Outcome.Error message when not (Cancel.is_requested cancel)
              ->
                {
                  retried with
                  outcome =
                    Core.Outcome.Inconclusive
                      ("timeout confirmation failed: " ^ message);
                }
            | _ -> retried
          in
          let final =
            if selection.omitted && final.outcome <> Core.Outcome.Killed then
              { final with evidence_origin = Run_store.Fast_estimated }
            else final
          in
          Mutex.protect progress (fun () ->
              incr confirmations;
              Printf.eprintf "ocaml-mutants: [confirm %d/%d] %s\n%!"
                !confirmations pending_confirmation (describe_result final));
          settle ~resumed:false ~historical:false final;
          final)
      executed
  in
  let journal_completion_error =
    if
      (not (Cancel.is_requested cancel))
      && List.length results = Array.length mutants
      && !journal_error = None
    then
      match Run_store.complete_journal journal with
      | Ok () -> None
      | Error error -> Some error
    else None
  in
  let persistence_error =
    match (!journal_error, journal_completion_error, !cache_error) with
    | Some error, _, _ | None, Some error, _ | None, None, Some error ->
        Some error
    | None, None, None -> None
  in
  (results, Cancel.is_requested cancel, persistence_error, List.rev !warnings)

let cache_mode_name = function
  | Config.Auto -> "auto"
  | Config.On -> "on"
  | Config.Off -> "off"

let path_has_separator value =
  String.contains value '/' || String.contains value '\\'

let regular_file path =
  try (Unix.stat path).st_kind = Unix.S_REG
  with Unix.Unix_error _ | Sys_error _ -> false

let resolve_executable ~root program =
  let direct =
    if Filename.is_relative program then Filename.concat root program
    else program
  in
  if path_has_separator program then
    if regular_file direct then Some direct else None
  else
    let separator = if Sys.win32 then ';' else ':' in
    let directories =
      match Sys.getenv_opt "PATH" with
      | None -> []
      | Some value -> String.split_on_char separator value
    in
    let extensions =
      if not Sys.win32 then [ "" ]
      else
        let configured =
          Option.value (Sys.getenv_opt "PATHEXT") ~default:".COM;.EXE;.BAT;.CMD"
        in
        "" :: String.split_on_char ';' configured
    in
    let windows_current_directory =
      if Sys.win32 then
        List.find_map
          (fun extension ->
            let candidate = Filename.concat root (program ^ extension) in
            if regular_file candidate then Some candidate else None)
          extensions
      else None
    in
    let rec find = function
      | [] -> None
      | directory :: rest -> (
          let directory =
            if directory = "" then root
            else if Filename.is_relative directory then
              Filename.concat root directory
            else directory
          in
          let found =
            List.find_map
              (fun extension ->
                let candidate =
                  Filename.concat directory (program ^ extension)
                in
                if regular_file candidate then Some candidate else None)
              extensions
          in
          match found with Some _ as found -> found | None -> find rest)
    in
    Option.fold ~none:(find directories) ~some:Option.some
      windows_current_directory

let digest_labeled_paths paths =
  paths
  |> List.sort_uniq (fun (left, _) (right, _) -> String.compare left right)
  |> List.map (fun (label, path) ->
      match digest_file path with
      | Ok digest -> (label ^ "=" ^ digest, true)
      | Error message -> (label ^ "=unavailable:" ^ message, false))
  |> List.split

let suffix_after_build_context target =
  let normalized = Core.Mutant.normalize_path target in
  let marker = "/default/" in
  let rec find index =
    if index + String.length marker > String.length normalized then None
    else if String.sub normalized index (String.length marker) = marker then
      Some (index + String.length marker)
    else find (index + 1)
  in
  match find 0 with
  | Some offset ->
      Some (String.sub normalized offset (String.length normalized - offset))
  | None when String.starts_with ~prefix:"default/" normalized ->
      Some (String.sub normalized 8 (String.length normalized - 8))
  | None -> None

let direct_build_artifacts ~root (analysis : analysis) =
  let baseline_context =
    Filename.concat root ".ocaml-mutants-baseline-stage-0"
  in
  let tests, tests_complete =
    analysis.tests
    |> List.map (fun (test : Dune_adapter.described_test) ->
        match suffix_after_build_context test.target with
        | None ->
            ( "test:" ^ test.source_dir ^ "/" ^ test.name
              ^ "=unavailable:target has no default build context",
              false )
        | Some relative -> (
            let path =
              Filename.concat baseline_context ("default/" ^ relative)
            in
            let label = "test:" ^ test.source_dir ^ "/" ^ test.name in
            match digest_file path with
            | Ok digest -> (label ^ "=" ^ digest, true)
            | Error message -> (label ^ "=unavailable:" ^ message, false)))
    |> List.split
  in
  let ppx_roots =
    [
      Filename.concat baseline_context "default/.ppx";
      Filename.concat root ".ocaml-mutants-analysis/default/.ppx";
    ]
  in
  let ppx_paths, ppx_enumeration_complete =
    ppx_roots
    |> List.mapi (fun index directory ->
        try
          ( files_recursive directory
            |> List.map (fun relative ->
                ( Printf.sprintf "ppx:%d:%s" index
                    (Core.Mutant.normalize_path relative),
                  Filename.concat directory relative )),
            true )
        with Unix.Unix_error _ | Sys_error _ -> ([], false))
    |> List.split
  in
  let ppx, ppx_complete = digest_labeled_paths (List.concat ppx_paths) in
  ( tests @ ppx,
    List.for_all Fun.id
      (tests_complete @ ppx_complete @ ppx_enumeration_complete) )

let cache_key ~run_id ~root ~analysis ~selection ~config ~timeout =
  let executable_digest, executable_complete =
    match digest_file Sys.executable_name with
    | Ok value -> (value, true)
    | Error message -> ("unavailable:" ^ message, false)
  in
  let indexed label values =
    (label ^ ".count", string_of_int (List.length values))
    :: List.mapi
         (fun index value -> (Printf.sprintf "%s.%d" label index, value))
         values
  in
  let stages =
    config.Config.test.stages
    |> List.mapi (fun stage_index (stage : Config.stage) ->
        let prefix = Printf.sprintf "test.stage.%d" stage_index in
        (prefix ^ ".name", stage.name)
        :: indexed (prefix ^ ".argv") (Core.Nonempty_argv.to_list stage.command))
    |> List.concat
  in
  let stage_executables, stage_executables_complete =
    config.Config.test.stages
    |> List.mapi (fun index (stage : Config.stage) ->
        let program =
          match Core.Nonempty_argv.to_list stage.command with
          | program :: _ -> program
          | [] -> assert false
        in
        match resolve_executable ~root program with
        | None ->
            (Printf.sprintf "%d:%s=unavailable:not found" index program, false)
        | Some path -> (
            match digest_file path with
            | Ok digest -> (Printf.sprintf "%d:%s=%s" index program digest, true)
            | Error message ->
                ( Printf.sprintf "%d:%s=unavailable:%s" index program message,
                  false )))
    |> List.split
  in
  let build_artifacts, build_artifacts_complete =
    if config.Config.test.driver = Config.Dune_driver then
      direct_build_artifacts ~root analysis
    else ([], true)
  in
  let catalog_ids =
    Core.Catalog.to_list analysis.catalog
    |> List.map (fun mutant -> Core.Mutant.Id.full (Core.Mutant.id mutant))
  in
  let external_inputs, external_complete =
    config.Config.test.external_inputs
    |> List.map (fun configured ->
        let path =
          if Filename.is_relative configured then
            Filename.concat root configured
          else configured
        in
        let digest =
          try
            if (Unix.stat path).st_kind = Unix.S_DIR then digest_tree path
            else digest_file path
          with
          | Unix.Unix_error (error, operation, argument) ->
              Error
                (Printf.sprintf "%s(%s): %s" operation argument
                   (Unix.error_message error))
          | Sys_error message -> Error message
        in
        match digest with
        | Ok value -> (configured ^ "=" ^ value, true)
        | Error message -> (configured ^ "=unavailable:" ^ message, false))
    |> List.split
  in
  let complete =
    executable_complete
    && List.for_all Fun.id external_complete
    && List.for_all Fun.id stage_executables_complete
    && build_artifacts_complete
  in
  let fields =
    [
      ("snapshot.manifest_abi", "1");
      ("workspace.digest", analysis.workspace_digest);
      ("selection.description", selection_name selection);
      ("mutation.profile", Core.Operator.Profile.name config.mutation.profile);
      ("timeout.seconds", string_of_float (Core.Duration.to_seconds timeout));
      ( "test.baseline_runs",
        string_of_int (Core.Positive_int.to_int config.test.baseline_runs) );
      ("test.parallel_safe", string_of_bool config.test.parallel_safe);
      ("test.reproducible", string_of_bool config.test.reproducible);
      ("execution.mode", Config.execution_mode_name config.execution.mode);
      ("resolved.config", Config.to_toml config);
      ("environment", canonical_environment ());
      ("toolchain", analysis.toolchain);
      ("executable.digest", executable_digest);
      ("rule.abi", "5");
      ("instrumentation.abi", "5");
      ("cache.abi", "4");
    ]
    @ indexed "selection.catalog_id" catalog_ids
    @ indexed "mutation.include" config.mutation.include_
    @ indexed "mutation.exclude" config.mutation.exclude
    @ indexed "mutation.operator"
        (List.map Core.Operator.Family.name config.mutation.operators)
    @ indexed "test.external_input" external_inputs
    @ indexed "test.executable" stage_executables
    @ indexed "build.artifact" build_artifacts
    @ ("test.stage.count", string_of_int (List.length config.test.stages))
      :: stages
  in
  let inputs =
    fields |> List.concat_map (fun (label, value) -> [ label; value ])
  in
  let inputs =
    if complete then inputs
    else "fingerprint-failure-run" :: Core.Run_id.to_string run_id :: inputs
  in
  (Run_store.run_key inputs, complete)

let unevaluated_expectations config =
  List.map
    (fun (expectation : Config.expectation) ->
      {
        Run_store.mutant_id = expectation.id;
        reason = expectation.reason;
        status = Run_store.Expectation_not_evaluated;
      })
    config.Config.mutation.expectations

let evaluate_expectations ~selection ~catalog ~results config =
  let catalog_ids = Hashtbl.create (Core.Catalog.length catalog) in
  Core.Catalog.to_list catalog
  |> List.iter (fun mutant ->
      Hashtbl.replace catalog_ids
        (Core.Mutant.Id.full (Core.Mutant.id mutant))
        ());
  let executed = Hashtbl.create (List.length results) in
  List.iter
    (fun (result : Run_store.mutant_result) ->
      Hashtbl.replace executed
        (Core.Mutant.Id.full (Core.Mutant.id result.mutant))
        result)
    results;
  List.map
    (fun (expectation : Config.expectation) ->
      let status =
        match Hashtbl.find_opt executed expectation.id with
        | Some result -> Run_store.expectation_status_of_result result
        | None when Hashtbl.mem catalog_ids expectation.id ->
            Run_store.Expectation_not_evaluated
        | None -> (
            match selection with
            | All -> Run_store.Expectation_stale
            | Changed | Changed_from _ | Mutants _ | Shard _ | Rerun _ ->
                Run_store.Expectation_not_evaluated)
      in
      {
        Run_store.mutant_id = expectation.id;
        reason = expectation.reason;
        status;
      })
    config.Config.mutation.expectations

type draft = {
  metadata : finished_at:string -> Run_store.metadata;
  results : Run_store.mutant_result list;
  completeness : Run_store.completeness;
  expectations : Run_store.expectation_evaluation list;
  skipped : Run_store.skip_summary list;
  warnings : Run_store.warning list;
  output : output;
  read_source : path:string -> (string, string) result;
}

let verdict_of_error error =
  match Error.cause error with
  | Error.Interrupted_by_user -> Application.Interrupted error
  | _ -> Application.Failed error

let unavailable_source ~path =
  Error (Printf.sprintf "source is unavailable after snapshot cleanup: %s" path)

let fallback_workspace_digest root =
  let root =
    try Unix.realpath root with Unix.Unix_error _ | Sys_error _ -> root
  in
  sha256 ("workspace-root\000" ^ root)

let make_final_metadata ~finished_at ~id ~started_at ~analysis ~selection
    ~config ~baseline_duration ~baseline_stages ~hit_map ~timeout ~key =
  let resolved_config = Config.to_report_yojson config in
  {
    Run_store.id;
    started_at;
    finished_at;
    workspace_digest = analysis.workspace_digest;
    toolchain = analysis.toolchain;
    profile = config.Config.mutation.profile;
    selection = selection_name selection;
    test_command = config.Config.test.command;
    baseline_duration;
    baseline_stages;
    hit_map;
    timeout;
    cache_mode = cache_mode_name config.Config.cache.mode;
    execution_mode = Config.execution_mode_name config.execution.mode;
    historical_reuse =
      Config.historical_reuse_name config.cache.historical_reuse;
    cache_key = key;
    resolved_config;
    input_fingerprint = key;
    config_digest = sha256 (Yojson.Safe.to_string ~std:true resolved_config);
  }

let preanalysis_failure ~run_id ~started_at ~root ~config ~selection ~output
    error =
  let metadata ~finished_at =
    let resolved_config = Config.to_report_yojson config in
    {
      Run_store.id = run_id;
      started_at;
      finished_at;
      workspace_digest = fallback_workspace_digest root;
      toolchain = "";
      profile = config.Config.mutation.profile;
      selection = selection_name selection;
      test_command = config.Config.test.command;
      baseline_duration = None;
      baseline_stages = [];
      hit_map = [];
      timeout = None;
      cache_mode = cache_mode_name config.Config.cache.mode;
      execution_mode = Config.execution_mode_name config.execution.mode;
      historical_reuse =
        Config.historical_reuse_name config.cache.historical_reuse;
      cache_key = "unavailable";
      resolved_config;
      input_fingerprint = "unavailable";
      config_digest = sha256 (Yojson.Safe.to_string ~std:true resolved_config);
    }
  in
  {
    Application.draft =
      {
        metadata;
        results = [];
        completeness = Run_store.Partial [];
        expectations = unevaluated_expectations config;
        skipped = [];
        warnings = [];
        output;
        read_source = unavailable_source;
      };
    verdict = verdict_of_error error;
    cleanup_errors = [];
  }

let analyzed_failure ~run_id ~started_at ~analysis ~config ~selection ~output
    ~read_source ~baseline_duration ~baseline_stages ~timeout ~key error =
  let metadata ~finished_at =
    make_final_metadata ~finished_at ~id:run_id ~started_at ~analysis ~selection
      ~config ~baseline_duration ~baseline_stages ~hit_map:[] ~timeout ~key
  in
  {
    Application.draft =
      {
        metadata;
        results = [];
        completeness = Run_store.Partial (Core.Catalog.to_list analysis.catalog);
        expectations =
          evaluate_expectations ~selection ~catalog:analysis.catalog ~results:[]
            config;
        skipped = analysis.skipped;
        warnings = [];
        output;
        read_source;
      };
    verdict = verdict_of_error error;
    cleanup_errors = [];
  }

let retained_source_reader sources ~path =
  match Hashtbl.find_opt sources path with
  | Some source -> Ok (Core.Source.to_string source)
  | None -> unavailable_source ~path

let completed_verdict ~catalog ~results ~not_run ~expectations =
  ignore expectations;
  if not_run <> [] then Application.Completed Application.Contract_failure
  else
    let outcomes =
      List.map (fun result -> (result.Run_store.mutant, result.outcome)) results
    in
    match Core.Run_results.of_complete_list catalog outcomes with
    | Error error ->
        Application.Failed
          (Error.create ~phase:Error.Execution ~cause:Error.Invariant_violation
             "%a" Core.Run_results.pp_error error)
    | Ok complete ->
        let summary = Core.Summary.of_results complete in
        if
          Core.Summary.error summary > 0
          || Core.Summary.inconclusive summary > 0
        then Application.Completed Application.Contract_failure
        else if
          List.exists
            (fun result ->
              result.Run_store.expected_reason = None
              && result.outcome = Core.Outcome.Survived)
            results
        then Application.Completed Application.Unexpected_survivor
        else Application.Completed Application.All_detected

let prepare_snapshot_action ~cancel ~store ~reservation ~run_id ~started_at
    ~root ~config ~fresh ~selection ~output snapshot =
  Event_bus.emit
    (Event_bus.Run_started { run_id = Core.Run_id.to_string run_id });
  Event_bus.emit (Event_bus.Phase_started { phase = "analysis"; total = None });
  match analyze ~cancel ~root ~config ~selection ~snapshot with
  | Error error ->
      preanalysis_failure ~run_id ~started_at ~root ~config ~selection ~output
        error
  | Ok analysis -> (
      let config = analysis.config in
      let snapshot_root = Workspace.root analysis.snapshot in
      match original_sources ~root:snapshot_root analysis.catalog with
      | Error error ->
          analyzed_failure ~run_id ~started_at ~analysis ~config ~selection
            ~output ~read_source:unavailable_source ~baseline_duration:None
            ~baseline_stages:[] ~timeout:None ~key:"unavailable" error
      | Ok sources -> (
          let read_source = retained_source_reader sources in
          Event_bus.emit
            (Event_bus.Phase_started
               {
                 phase = "baseline";
                 total =
                   Some
                     (Core.Positive_int.to_int config.Config.test.baseline_runs
                     * List.length analysis.baseline_config.Config.test.stages);
               });
          match
            Baseline.run ~cancel ~root:snapshot_root
              ~config:analysis.baseline_config
              ~build_dir:".ocaml-mutants-baseline"
          with
          | Baseline.Incomplete incomplete ->
              analyzed_failure ~run_id ~started_at ~analysis ~config ~selection
                ~output ~read_source
                ~baseline_duration:(Baseline.slowest incomplete.evidence)
                ~baseline_stages:(Baseline.stages incomplete.evidence)
                ~timeout:config.Config.test.timeout ~key:"unavailable"
                incomplete.error
          | Baseline.Completed complete -> (
              let baseline_stages = Baseline.stages complete.evidence in
              let baseline_duration = complete.slowest in
              let timeout_result =
                match config.Config.test.timeout with
                | Some timeout
                  when Core.Duration.to_seconds timeout
                       <= Core.Duration.to_seconds baseline_duration ->
                    Error
                      (Error.create ~phase:Error.Baseline_proof
                         ~cause:Error.Invalid_input
                         ~context:
                           [
                             ( "slowest_baseline_seconds",
                               string_of_float
                                 (Core.Duration.to_seconds baseline_duration) );
                             ( "timeout_seconds",
                               string_of_float
                                 (Core.Duration.to_seconds timeout) );
                           ]
                         "configured timeout must be greater than the slowest \
                          baseline")
                | Some timeout -> Ok timeout
                | None ->
                    Ok
                      (duration_exn
                         (max 10.
                            (Core.Duration.to_seconds baseline_duration *. 5.)))
              in
              match timeout_result with
              | Error error ->
                  analyzed_failure ~run_id ~started_at ~analysis ~config
                    ~selection ~output ~read_source
                    ~baseline_duration:(Some baseline_duration) ~baseline_stages
                    ~timeout:config.Config.test.timeout ~key:"unavailable" error
              | Ok timeout -> (
                  let key, fingerprint_complete =
                    cache_key ~run_id ~root:snapshot_root ~analysis ~selection
                      ~config ~timeout
                  in
                  Event_bus.emit
                    (Event_bus.Phase_started
                       {
                         phase = "instrumentation";
                         total =
                           Some
                             (List.length
                                (Core.Catalog.to_list analysis.catalog));
                       });
                  match
                    Ocaml_frontend.instrument_files ~root:snapshot_root
                      analysis.catalog
                  with
                  | Error error ->
                      analyzed_failure ~run_id ~started_at ~analysis ~config
                        ~selection ~output ~read_source
                        ~baseline_duration:(Some baseline_duration)
                        ~baseline_stages ~timeout:(Some timeout) ~key error
                  | Ok _ -> (
                      Event_bus.emit
                        (Event_bus.Phase_started
                           { phase = "readiness"; total = None });
                      match
                        run_readiness ~cancel ~root:snapshot_root ~config
                          ~catalog:analysis.catalog
                          ~build_dir:".ocaml-mutants-instrumented"
                          ~timeout:(Some timeout)
                      with
                      | Error error ->
                          analyzed_failure ~run_id ~started_at ~analysis ~config
                            ~selection ~output ~read_source
                            ~baseline_duration:(Some baseline_duration)
                            ~baseline_stages ~timeout:(Some timeout) ~key error
                      | Ok readiness -> (
                          match
                            Run_store.open_journal store reservation ~key
                              ~fresh:(fresh || not fingerprint_complete)
                          with
                          | Error error ->
                              analyzed_failure ~run_id ~started_at ~analysis
                                ~config ~selection ~output ~read_source
                                ~baseline_duration:(Some baseline_duration)
                                ~baseline_stages ~timeout:(Some timeout) ~key
                                error
                          | Ok journal ->
                              let ( results,
                                    was_interrupted,
                                    cache_error,
                                    warnings ) =
                                run_mutants ~cancel ~root:snapshot_root ~config
                                  ~store ~journal ~key ~sources ~fresh
                                  ~reuse_allowed:fingerprint_complete ~timeout
                                  ~hit_map:readiness.hit_map analysis.catalog
                              in
                              let executed_ids =
                                Hashtbl.create (List.length results)
                              in
                              List.iter
                                (fun result ->
                                  Hashtbl.replace executed_ids
                                    (Core.Mutant.Id.full
                                       (Core.Mutant.id result.Run_store.mutant))
                                    ())
                                results;
                              let not_run =
                                Core.Catalog.to_list analysis.catalog
                                |> List.filter (fun mutant ->
                                    not
                                      (Hashtbl.mem executed_ids
                                         (Core.Mutant.Id.full
                                            (Core.Mutant.id mutant))))
                              in
                              let completeness =
                                match not_run with
                                | [] -> Run_store.Complete
                                | mutants -> Run_store.Partial mutants
                              in
                              let expectations =
                                evaluate_expectations ~selection
                                  ~catalog:analysis.catalog ~results config
                              in
                              let stale_expectations =
                                List.filter
                                  (fun (evaluation :
                                         Run_store.expectation_evaluation) ->
                                    evaluation.status
                                    = Run_store.Expectation_stale)
                                  expectations
                              in
                              let warnings =
                                (if fingerprint_complete then warnings
                                 else
                                   {
                                     Run_store.code = "fingerprint-incomplete";
                                     message =
                                       "an input digest failed; cache and \
                                        resume reuse were disabled for this \
                                        run";
                                   }
                                   :: warnings)
                                @ List.map
                                    (fun (evaluation :
                                           Run_store.expectation_evaluation) ->
                                      {
                                        Run_store.code = "stale-expectation";
                                        message =
                                          Printf.sprintf "%s: %s"
                                            evaluation.mutant_id
                                            evaluation.reason;
                                      })
                                    stale_expectations
                              in
                              let verdict =
                                if was_interrupted then
                                  Application.Interrupted
                                    (interrupted Error.Execution "interrupted")
                                else
                                  match cache_error with
                                  | Some error -> Application.Failed error
                                  | None ->
                                      completed_verdict
                                        ~catalog:analysis.catalog ~results
                                        ~not_run ~expectations
                              in
                              let metadata ~finished_at =
                                make_final_metadata ~finished_at ~id:run_id
                                  ~started_at ~analysis ~selection ~config
                                  ~baseline_duration:(Some baseline_duration)
                                  ~baseline_stages ~hit_map:readiness.hit_map
                                  ~timeout:(Some timeout) ~key
                              in
                              {
                                Application.draft =
                                  {
                                    metadata;
                                    results;
                                    completeness;
                                    expectations;
                                    skipped = analysis.skipped;
                                    warnings;
                                    output;
                                    read_source;
                                  };
                                verdict;
                                cleanup_errors = [];
                              }))))))

let prepare_in_snapshot ~cancel ~store ~reservation ~started_at ~root ~config
    ~fresh ~selection ~output ~snapshot =
  let run_id = Run_store.reservation_id reservation in
  prepare_snapshot_action ~cancel ~store ~reservation ~run_id ~started_at ~root
    ~config ~fresh ~selection ~output snapshot

let prepare_failure ~cancel:_ ~store:_ ~reservation ~started_at ~root ~config
    ~fresh:_ ~selection ~output error =
  let run_id = Run_store.reservation_id reservation in
  preanalysis_failure ~run_id ~started_at ~root ~config ~selection ~output error

let run_of_draft ~finished_at ~resolution draft =
  let status =
    match Application.report_status resolution with
    | Application.Report_completed -> Run_store.Completed
    | Application.Report_interrupted -> Run_store.Interrupted
    | Application.Report_failed error -> Run_store.Failed error
  in
  {
    Run_store.metadata = draft.metadata ~finished_at;
    status;
    results = draft.results;
    completeness = draft.completeness;
    expectations = draft.expectations;
    skipped = draft.skipped;
    warnings = draft.warnings;
  }

let projection_error projection =
  Error.create ~phase:Error.Reporting ~cause:Error.Invariant_violation
    "could not project the native run report: %a" Stryker_report.pp_error
    projection

let render output ~read_source run =
  match output with
  | Json -> Ok (Run_store.run_to_string run)
  | Stryker_json thresholds ->
      Result.map_error projection_error
        (Stryker_report.to_string ~thresholds ~read_source run)
  | Terminal { quiet = false; color } ->
      Ok (Format.asprintf "%a" (Report.print_run ~color) run)
  | Terminal { quiet = true; _ } -> Ok ""

let emit_advisories advisories =
  List.iter
    (fun advisory ->
      try
        Format.eprintf "ocaml-mutants: non-authoritative advisory: %a@."
          Error.pp advisory
      with _ -> ())
    advisories

let output_advisory ~operation exception_ =
  Error.create ~phase:Error.Reporting ~cause:Error.Io_failure
    ~context:
      [ ("operation", operation); ("exception", Printexc.to_string exception_) ]
    "authoritative run report was published but output %s failed" operation

let emit_after_publish_with ~write ~flush rendered =
  if rendered = "" then []
  else
    let attempt operation action =
      try
        action ();
        []
      with exception_ -> [ output_advisory ~operation exception_ ]
    in
    let write_advisories = attempt "write" (fun () -> write rendered) in
    let flush_advisories = attempt "flush" flush in
    write_advisories @ flush_advisories

module For_testing = struct
  let emit_after_publish = emit_after_publish_with
  let redact = redact
  let classify_exhaustive_hits = classify_exhaustive_hits

  let resolved_test_plan config tests =
    Result.map
      (fun plan -> (plan.config, plan.baseline_config))
      (resolve_test_plan config tests)

  let selected_stages config hit_map mutant =
    let selection = stages_for_mutant config hit_map mutant in
    ( List.map (fun (stage : Config.stage) -> stage.name) selection.stages,
      selection.omitted )

  let settlement_ready result = not (requires_timeout_confirmation result)
  let mutant_environment = mutant_environment
end

let commit_reserved ~store ~reservation ~finished_at ~resolution draft =
  let* staged = Run_store.stage_run store reservation in
  let* finalization = Run_store.finalize_run staged in
  let resolution =
    List.fold_left Application.with_failure resolution
      finalization.cleanup_errors
  in
  let final_run = run_of_draft ~finished_at ~resolution draft in
  match render draft.output ~read_source:draft.read_source final_run with
  | Error reporting ->
      let resolution = Application.with_failure resolution reporting in
      let final_run = run_of_draft ~finished_at ~resolution draft in
      let* published =
        Run_store.publish_run finalization.publication final_run
      in
      emit_advisories published.advisories;
      Ok resolution
  | Ok rendered ->
      let* published =
        Run_store.publish_run finalization.publication final_run
      in
      let output_advisories =
        emit_after_publish_with ~write:print_string
          ~flush:(fun () -> flush stdout)
          rendered
      in
      emit_advisories (published.advisories @ output_advisories);
      Ok resolution
