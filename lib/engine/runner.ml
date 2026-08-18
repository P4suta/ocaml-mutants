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
  Ok (Printf.sprintf "ocaml=%s; dune=%s" ocaml dune)

let canonical_environment () =
  Unix.environment () |> Array.to_list |> List.sort String.compare
  |> String.concat "\000"

let default_command command =
  Core.Nonempty_argv.to_list command = [ "dune"; "runtest"; "--force" ]

let test_command command build_dir =
  if default_command command then
    [ "dune"; "runtest"; "--build-dir"; build_dir; "--force" ]
  else Core.Nonempty_argv.to_list command

let duration_exn seconds =
  match Core.Duration.of_seconds seconds with
  | Ok value -> value
  | Error message -> invalid_arg message

let run_readiness ~cancel ~root ~config ~build_dir ~timeout =
  let rec run stage_index duration = function
    | [] -> Ok (duration_exn duration)
    | (stage : Config.stage) :: rest ->
        let result =
          Process_supervisor.run ~cancel
            ?timeout:(Option.map Core.Duration.to_seconds timeout)
            ~cwd:root
            ~env:
              [
                ("OCAML_MUTANTS_ACTIVE", None); ("DUNE_CACHE", Some "disabled");
              ]
            (test_command stage.command
               (Printf.sprintf "%s-stage-%d" build_dir stage_index))
        in
        if result.status = Process_supervisor.Cancelled then
          Error
            (interrupted Error.Ready_proof
               "instrumented readiness proof was interrupted")
        else if Process_supervisor.succeeded result then
          run (stage_index + 1) (max duration result.duration) rest
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
  run 0 0. config.Config.test.stages

let selection_name = function
  | All -> "all"
  | Changed -> "changed-from-upstream"
  | Changed_from revision -> "changed-from:" ^ revision
  | Mutants ids -> "mutants:" ^ String.concat "," ids

let changed_filter ~cancel ~root = function
  | All -> Ok (fun _ -> true)
  | Changed ->
      let* files = Changed.files ~cancel ~root ~from:None in
      Ok (fun path -> List.mem path files)
  | Changed_from revision ->
      let* files = Changed.files ~cancel ~root ~from:(Some revision) in
      Ok (fun path -> List.mem path files)
  | Mutants _ -> Ok (fun _ -> true)

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
}

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
                        | Mutants _ -> Core.Operator.Family.all
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
                        })))

let catalog_json analysis selection profile =
  `Assoc
    [
      ("document_type", `String "ocaml-mutants.catalog-v1");
      ("schema_version", `Int 1);
      ( "workspace",
        `Assoc
          [
            ("digest", `String analysis.workspace_digest);
            ("toolchain", `String analysis.toolchain);
          ] );
      ("profile", `String (Core.Operator.Profile.name profile));
      ("selection", `String (selection_name selection));
      ( "mutants",
        `List
          (Core.Catalog.to_list analysis.catalog
          |> List.map (fun mutant ->
              `Assoc
                [
                  ("id", `String (Core.Mutant.Id.short (Core.Mutant.id mutant)));
                  ( "full_id",
                    `String (Core.Mutant.Id.full (Core.Mutant.id mutant)) );
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
            (catalog_json analysis selection config.Config.mutation.profile);
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

let merge_captured captures =
  let limit = Process_supervisor.capture_capacity_bytes in
  let total_bytes =
    List.fold_left
      (fun total captured -> total + captured.Run_store.total_bytes)
      0 captures
  in
  let contents =
    List.map (fun captured -> captured.Run_store.contents) captures
    |> String.concat ""
  in
  let retained =
    if String.length contents <= limit then contents
    else
      let half = limit / 2 in
      String.sub contents 0 half
      ^ String.sub contents (String.length contents - half) half
  in
  Run_store.captured
    ~truncated:
      (total_bytes > limit
      || List.exists (fun captured -> captured.Run_store.truncated) captures)
    ~total_bytes retained

let run_attempt ~cancel ~root ~config ~worker_id ~timeout mutant =
  let rec run stage_index duration stages stdout stderr = function
    | [] ->
        {
          outcome = Core.Outcome.Survived;
          duration;
          stages = List.rev stages;
          stdout = merge_captured (List.rev stdout);
          stderr = merge_captured (List.rev stderr);
        }
    | (stage : Config.stage) :: rest -> (
        let process =
          Process_supervisor.run ~cancel
            ~timeout:(Core.Duration.to_seconds timeout)
            ~cwd:root
            ~env:
              [
                ( "OCAML_MUTANTS_ACTIVE",
                  Some (Core.Mutant.Id.short (Core.Mutant.id mutant)) );
                ("DUNE_CACHE", Some "disabled");
              ]
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
            ~total_bytes:process.stdout_bytes process.stdout
        in
        let captured_stderr =
          Run_store.captured ~truncated:process.stderr_truncated
            ~total_bytes:process.stderr_bytes process.stderr
        in
        let finish outcome =
          {
            outcome;
            duration = duration +. process.duration;
            stages = List.rev (stage_result :: stages);
            stdout = merge_captured (List.rev (captured_stdout :: stdout));
            stderr = merge_captured (List.rev (captured_stderr :: stderr));
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
  run 0 0. [] [] [] config.Config.test.stages

let result_of_attempt mutant attempt =
  {
    Run_store.mutant;
    outcome = attempt.outcome;
    duration = duration_exn attempt.duration;
    cached = false;
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

let run_mutants ~cancel ~root ~config ~store ~key ~sources ~fresh ~timeout
    catalog =
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
    Config.cache_enabled config.Config.cache.mode ~command:config.test.command
  in
  let jobs =
    if
      List.for_all
        (fun (stage : Config.stage) -> default_command stage.command)
        config.Config.test.stages
      || config.Config.test.parallel_safe
    then
      min
        (Core.Positive_int.to_int config.Config.execution.jobs)
        (max 1 (Array.length mutants))
    else 1
  in
  let worker worker_id () =
    let rec loop () =
      let index = Atomic.fetch_and_add next 1 in
      if index < Array.length mutants && not (Cancel.is_requested cancel) then (
        let mutant = mutants.(index) in
        let source = Hashtbl.find sources (Core.Mutant.path mutant) in
        let cached =
          if fresh || (not cache_enabled) || expected_reason mutant <> None then
            None
          else Run_store.load_mutant store ~key ~source ~expected:mutant
        in
        let result =
          match cached with
          | Some cached -> cached
          | None ->
              run_attempt ~cancel ~root ~config ~worker_id ~timeout mutant
              |> result_of_attempt mutant
        in
        result_slots.(index) <- Some result;
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
  let results =
    Array.to_list result_slots |> List.filter_map Fun.id
    |> List.map (fun first ->
        if
          first.Run_store.cached
          || first.outcome <> Core.Outcome.Timeout
          || Cancel.is_requested cancel
        then first
        else
          let confirmation =
            run_attempt ~cancel ~root ~config ~worker_id:0 ~timeout first.mutant
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
              stdout = merge_captured [ first.stdout; confirmation.stdout ];
              stderr = merge_captured [ first.stderr; confirmation.stderr ];
            }
          in
          match confirmation.outcome with
          | Core.Outcome.Timeout -> { retried with timeout_confirmed = true }
          | Core.Outcome.Error message when not (Cancel.is_requested cancel) ->
              {
                retried with
                outcome =
                  Core.Outcome.Inconclusive
                    ("timeout confirmation failed: " ^ message);
              }
          | _ -> retried)
  in
  let results =
    List.map
      (fun result ->
        {
          result with
          Run_store.expected_reason = expected_reason result.Run_store.mutant;
        })
      results
  in
  let cache_error = ref None in
  let warnings = ref [] in
  if cache_enabled then
    List.iter
      (fun result ->
        if
          result.Run_store.expected_reason = None
          && (result.Run_store.outcome <> Core.Outcome.Timeout
             || result.timeout_confirmed)
          && not (Core.Outcome.is_error result.outcome)
        then
          match Run_store.save_mutant store ~key result with
          | Ok () -> ()
          | Error error -> (
              match config.Config.cache.mode with
              | Config.On ->
                  if !cache_error = None then cache_error := Some error
              | Config.Auto ->
                  warnings :=
                    {
                      Run_store.code = "cache-write-failed";
                      message = Error.message error;
                    }
                    :: !warnings
              | Config.Off -> ()))
      results;
  (results, Cancel.is_requested cancel, !cache_error, List.rev !warnings)

let cache_mode_name = function
  | Config.Auto -> "auto"
  | Config.On -> "on"
  | Config.Off -> "off"

let cache_key ~analysis ~selection ~config ~timeout =
  let executable_digest =
    match digest_file Sys.executable_name with
    | Ok value -> value
    | Error _ -> "unavailable"
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
  let catalog_ids =
    Core.Catalog.to_list analysis.catalog
    |> List.map (fun mutant -> Core.Mutant.Id.full (Core.Mutant.id mutant))
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
      ("environment", canonical_environment ());
      ("toolchain", analysis.toolchain);
      ("executable.digest", executable_digest);
      ("rule.abi", "4");
      ("instrumentation.abi", "3");
      ("cache.abi", "3");
    ]
    @ indexed "selection.catalog_id" catalog_ids
    @ indexed "mutation.include" config.mutation.include_
    @ indexed "mutation.exclude" config.mutation.exclude
    @ indexed "mutation.operator"
        (List.map Core.Operator.Family.name config.mutation.operators)
    @ ("test.stage.count", string_of_int (List.length config.test.stages))
      :: stages
  in
  fields
  |> List.concat_map (fun (label, value) -> [ label; value ])
  |> Run_store.run_key

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
            | Changed | Changed_from _ | Mutants _ ->
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
    ~config ~baseline_duration ~baseline_stages ~timeout ~key =
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
    timeout;
    cache_mode = cache_mode_name config.Config.cache.mode;
    cache_key = key;
  }

let preanalysis_failure ~run_id ~started_at ~root ~config ~selection ~output
    error =
  let metadata ~finished_at =
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
      timeout = None;
      cache_mode = cache_mode_name config.Config.cache.mode;
      cache_key = "unavailable";
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
      ~config ~baseline_duration ~baseline_stages ~timeout ~key
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
  let expectation_failure =
    List.exists
      (fun (evaluation : Run_store.expectation_evaluation) ->
        Run_store.expectation_status_is_failure evaluation.status)
      expectations
  in
  if expectation_failure || not_run <> [] then
    Application.Completed Application.Contract_failure
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

let prepare_snapshot_action ~cancel ~store ~run_id ~started_at ~root ~config
    ~fresh ~selection ~output snapshot =
  match analyze ~cancel ~root ~config ~selection ~snapshot with
  | Error error ->
      preanalysis_failure ~run_id ~started_at ~root ~config ~selection ~output
        error
  | Ok analysis -> (
      let snapshot_root = Workspace.root analysis.snapshot in
      match original_sources ~root:snapshot_root analysis.catalog with
      | Error error ->
          analyzed_failure ~run_id ~started_at ~analysis ~config ~selection
            ~output ~read_source:unavailable_source ~baseline_duration:None
            ~baseline_stages:[] ~timeout:None ~key:"unavailable" error
      | Ok sources -> (
          let read_source = retained_source_reader sources in
          match
            Baseline.run ~cancel ~root:snapshot_root ~config
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
                  let key = cache_key ~analysis ~selection ~config ~timeout in
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
                      match
                        run_readiness ~cancel ~root:snapshot_root ~config
                          ~build_dir:".ocaml-mutants-instrumented"
                          ~timeout:(Some timeout)
                      with
                      | Error error ->
                          analyzed_failure ~run_id ~started_at ~analysis ~config
                            ~selection ~output ~read_source
                            ~baseline_duration:(Some baseline_duration)
                            ~baseline_stages ~timeout:(Some timeout) ~key error
                      | Ok _ ->
                          let results, was_interrupted, cache_error, warnings =
                            run_mutants ~cancel ~root:snapshot_root ~config
                              ~store ~key ~sources ~fresh ~timeout
                              analysis.catalog
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
                                evaluation.status = Run_store.Expectation_stale)
                              expectations
                          in
                          let warnings =
                            warnings
                            @ List.map
                                (fun (evaluation :
                                       Run_store.expectation_evaluation) ->
                                  {
                                    Run_store.code = "stale-expectation";
                                    message =
                                      Printf.sprintf "%s: %s"
                                        evaluation.mutant_id evaluation.reason;
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
                                  completed_verdict ~catalog:analysis.catalog
                                    ~results ~not_run ~expectations
                          in
                          let metadata ~finished_at =
                            make_final_metadata ~finished_at ~id:run_id
                              ~started_at ~analysis ~selection ~config
                              ~baseline_duration:(Some baseline_duration)
                              ~baseline_stages ~timeout:(Some timeout) ~key
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
                          })))))

let prepare_in_snapshot ~cancel ~store ~reservation ~started_at ~root ~config
    ~fresh ~selection ~output ~snapshot =
  let run_id = Run_store.reservation_id reservation in
  prepare_snapshot_action ~cancel ~store ~run_id ~started_at ~root ~config
    ~fresh ~selection ~output snapshot

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
