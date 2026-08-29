open Cmdliner
open Ocaml_mutants_engine

module Services = struct
  module Signals = Signal_service.System
  module Workspace = Runner.Workspace

  module Clock = struct
    let now = Util.timestamp
  end

  module Store = Run_store

  type draft = Runner.draft

  let prepare_in_snapshot = Runner.prepare_in_snapshot
  let prepare_failure = Runner.prepare_failure
  let commit_reserved = Runner.commit_reserved
  let list_mutants = Runner.list_mutants
end

module App = Application.Make (Services)
module Request = Application_request

let ( let* ) value continuation = Result.bind value continuation
let presplit_command = ref None

(* The `--` command tail applies to the explicit `run` subcommand and to the
   implicit default-command invocation alike; every other subcommand keeps `--`
   as an ordinary end-of-options token. cmdliner 2.x resolves subcommand names
   exactly (prefix matching stays off unless CMDLINER_LEGACY_PREFIXES is set),
   so exact membership below agrees with its dispatch. *)
let split_run_argv ~subcommands:_ argv =
  if Array.length argv < 2 || not (String.equal argv.(1) "run") then argv
  else
    let rec find index =
      if index = Array.length argv then None
      else if argv.(index) = "--" then Some index
      else find (index + 1)
    in
    match find 2 with
    | None -> argv
    | Some index ->
        presplit_command :=
          Some
            (Array.sub argv (index + 1) (Array.length argv - index - 1)
            |> Array.to_list);
        Array.sub argv 0 index

let print_error error =
  Format.eprintf "ocaml-mutants: %a@." Error.pp error;
  Error.exit_code error

let use_result = function Ok code -> code | Error error -> print_error error

let standard_exit_infos =
  [
    Cmd.Exit.info 0 ~doc:"The command completed successfully.";
    Cmd.Exit.info 2 ~doc:"A usage or infrastructure error occurred.";
  ]

let interruptible_exit_infos =
  standard_exit_infos
  @ [ Cmd.Exit.info 130 ~doc:"The command was interrupted." ]

let run_exit_infos =
  [
    Cmd.Exit.info 0
      ~doc:"Mutation measurement completed, regardless of survivor count.";
    Cmd.Exit.info 2 ~doc:"A usage, baseline, tool, or process error occurred.";
    Cmd.Exit.info 130 ~doc:"The run was interrupted.";
  ]

let command_info ?(exits = standard_exit_infos) name ~doc =
  Cmd.info name ~doc ~exits

let path_argument =
  let doc = "Dune workspace to inspect." in
  Arg.(value & pos 0 string "." & info [] ~docv:"PATH" ~doc)

let include_options =
  Arg.(
    value & opt_all string []
    & info [ "include" ] ~docv:"GLOB"
        ~doc:"Include source paths matching $(docv). May be repeated.")

let exclude_options =
  Arg.(
    value & opt_all string []
    & info [ "exclude" ] ~docv:"GLOB"
        ~doc:"Exclude source paths matching $(docv). May be repeated.")

let operator_options =
  Arg.(
    value & opt_all string []
    & info [ "operator" ] ~docv:"NAME"
        ~doc:"Enable mutation operator $(docv). May be repeated.")

let profile_option =
  Arg.(
    value
    & opt (some string) None
    & info [ "profile" ] ~docv:"PROFILE"
        ~doc:
          "Select the mutation tier: balanced (default; every family except \
           if-branch and sequence-deletion), strong (adds if-branch), or all \
           (adds sequence-deletion).")

let mutant_options =
  Arg.(
    value & opt_all string []
    & info [ "mutant" ] ~docv:"FULL_OR_UNIQUE_PREFIX"
        ~doc:
          "Run only the mutant matching this full ID or unique prefix. May be \
           repeated.")

let jobs_option =
  Arg.(
    value
    & opt (some int) None
    & info [ "j"; "jobs" ] ~docv:"N" ~doc:"Use $(docv) mutation workers.")

let timeout_option =
  Arg.(
    value
    & opt (some float) None
    & info [ "timeout" ] ~docv:"SECONDS" ~doc:"Set the per-mutant timeout.")

let fresh_option =
  Arg.(value & flag & info [ "fresh" ] ~doc:"Ignore cached mutant outcomes.")

let shard_plan_option =
  Arg.(
    value
    & opt (some string) None
    & info [ "shard-plan" ] ~docv:"PLAN.json"
        ~doc:"Execute one assignment from a validated shard plan.")

let shard_index_option =
  Arg.(
    value
    & opt (some int) None
    & info [ "shard" ] ~docv:"INDEX"
        ~doc:"Zero-based shard index from --shard-plan.")

let cache_mode_option =
  let values =
    [ ("auto", Config.Auto); ("on", Config.On); ("off", Config.Off) ]
  in
  Arg.(
    value
    & opt (some (enum values)) None
    & info [ "cache-mode" ] ~docv:"MODE"
        ~doc:"Override the configured outcome cache policy: auto, on, or off.")

let changed_option =
  Arg.(
    value & flag
    & info [ "changed" ]
        ~doc:"Only mutate files changed from the upstream merge-base.")

let changed_from_option =
  Arg.(
    value
    & opt (some string) None
    & info [ "changed-from" ] ~docv:"REV"
        ~doc:"Only mutate files changed from $(docv).")

let json_option =
  Arg.(
    value & flag
    & info [ "json" ] ~doc:"Write schema-versioned JSON to standard output.")

type events_output = No_events | Jsonl_events

let events_option =
  Arg.(
    value
    & opt (enum [ ("none", No_events); ("jsonl", Jsonl_events) ]) No_events
    & info [ "events" ] ~docv:"FORMAT"
        ~doc:
          "Emit the versioned execution event stream. FORMAT is none or jsonl; \
           jsonl reserves standard output for events.")

let stryker_json_option =
  Arg.(
    value & flag
    & info [ "stryker-json" ]
        ~doc:
          "Write a Mutation Testing Report Schema v2 projection to standard \
           output. The native run report remains the stored authoritative \
           record.")

let threshold_high_option =
  Arg.(
    value
    & opt (some int) None
    & info [ "threshold-high" ] ~docv:"PERCENT"
        ~doc:"Set the required high score threshold for --stryker-json.")

let threshold_low_option =
  Arg.(
    value
    & opt (some int) None
    & info [ "threshold-low" ] ~docv:"PERCENT"
        ~doc:"Set the required low score threshold for --stryker-json.")

let quiet_option =
  Arg.(
    value & flag & info [ "q"; "quiet" ] ~doc:"Suppress normal terminal output.")

let no_color_option =
  Arg.(value & flag & info [ "no-color" ] ~doc:"Disable ANSI colors.")

type color_mode = Color_auto | Color_always | Color_never

let color_mode_option =
  Arg.(
    value
    & opt
        (enum
           [
             ("auto", Color_auto);
             ("always", Color_always);
             ("never", Color_never);
           ])
        Color_auto
    & info [ "color" ] ~docv:"WHEN" ~doc:"Color output: auto, always, or never.")

let decode_operators values =
  let rec loop decoded = function
    | [] -> Ok (List.rev decoded)
    | value :: rest -> (
        match Ocaml_mutants_core.Operator.of_string value with
        | Ok operator -> loop (operator :: decoded) rest
        | Error message -> Error (Error.make Error.Usage "%s" message))
  in
  if values = [] then Ok None else Result.map Option.some (loop [] values)

let terminal_color ~no_color ~color_mode =
  if no_color then false
  else
    match color_mode with
    | Color_never -> false
    | Color_always -> true
    | Color_auto ->
        Sys.getenv_opt "NO_COLOR" = None
        && Sys.getenv_opt "CI" = None
        && Unix.isatty Unix.stdout

let output ~json ~quiet ~no_color ~color_mode =
  let color = terminal_color ~no_color ~color_mode in
  if json && quiet then
    Error (Error.make Error.Usage "--json and --quiet cannot be used together")
  else if json then Ok Request.Json
  else Ok (Request.Terminal { quiet; color })

let run_output ~json ~stryker_json ~threshold_high ~threshold_low ~quiet
    ~no_color ~color_mode =
  if not stryker_json then
    if threshold_high <> None || threshold_low <> None then
      Error
        (Error.make Error.Usage
           "--threshold-high and --threshold-low require --stryker-json")
    else output ~json ~quiet ~no_color ~color_mode
  else if json || quiet then
    Error
      (Error.make Error.Usage
         "--stryker-json cannot be combined with --json or --quiet")
  else
    match (threshold_high, threshold_low) with
    | Some high, Some low -> (
        match Stryker_report.thresholds ~high ~low with
        | Ok thresholds -> Ok (Request.Stryker_json thresholds)
        | Error message -> Error (Error.make Error.Usage "%s" message))
    | _ ->
        Error
          (Error.make Error.Usage
             "--stryker-json requires both --threshold-high and --threshold-low")

let validate_mutant_prefixes mutants =
  let rec validate = function
    | [] -> Ok ()
    | prefix :: rest ->
        if Ocaml_mutants_core.Mutant.Id.is_valid_prefix prefix then
          validate rest
        else
          Error
            (Error.make Error.Usage
               "--mutant expects a non-empty lowercase hexadecimal prefix of \
                at most 64 characters (got %S)"
               prefix)
  in
  validate mutants

let selection ~changed ~changed_from ~mutants ~operators =
  let* () = validate_mutant_prefixes mutants in
  match (changed, changed_from, mutants, operators) with
  | _, _, _ :: _, _ :: _ ->
      Error
        (Error.make Error.Usage "--mutant cannot be combined with --operator")
  | true, _, _ :: _, _ | _, Some _, _ :: _, _ ->
      Error
        (Error.make Error.Usage
           "--mutant cannot be combined with changed-file selection")
  | true, Some _, [], _ ->
      Error
        (Error.make Error.Usage
           "--changed and --changed-from cannot be used together")
  | true, None, [], _ -> Ok Request.Changed
  | false, Some revision, [], _ -> Ok (Request.Changed_from revision)
  | false, None, _ :: _, _ -> Ok (Request.Mutants mutants)
  | false, None, [], _ -> Ok Request.All

let load_shard_plan path =
  let* contents =
    Util.read_file path
    |> Result.map_error (fun message ->
        Error.make Error.Usage "cannot read shard plan %s: %s" path message)
  in
  Shard_plan.of_string contents
  |> Result.map_error (fun message ->
      Error.make Error.Usage "invalid shard plan %s: %s" path message)

let run_selection ~changed ~changed_from ~mutants ~operators ~shard_plan
    ~shard_index =
  match (shard_plan, shard_index) with
  | None, None -> selection ~changed ~changed_from ~mutants ~operators
  | Some _, None | None, Some _ ->
      Error
        (Error.make Error.Usage
           "--shard-plan and --shard must be specified together")
  | Some path, Some index ->
      if changed || changed_from <> None || mutants <> [] then
        Error
          (Error.make Error.Usage
             "shard execution cannot be combined with changed or --mutant \
              selection")
      else
        let* plan = load_shard_plan path in
        let* assignment =
          Shard_plan.assignment plan index
          |> Result.map_error (fun message ->
              Error.make Error.Usage "%s" message)
        in
        Ok
          (Request.Shard
             {
               plan_id = plan.plan_id;
               input_fingerprint = plan.input_fingerprint;
               index;
               count = List.length plan.assignments;
               mutant_ids = assignment.mutant_ids;
             })

let configuration ~root ~include_ ~exclude ~operators ~profile ~command ~timeout
    ~jobs ~cache_mode =
  match Config.load_with_metadata root with
  | Error message -> Error (Error.make Error.Usage "%s" message)
  | Ok loaded ->
      List.iter
        (fun warning -> Format.eprintf "ocaml-mutants: warning: %s@." warning)
        loaded.Config.warnings;
      let config = loaded.config in
      let open Result in
      let* operators = decode_operators operators in
      let* profile =
        match profile with
        | None -> Ok None
        | Some value -> (
            match Ocaml_mutants_core.Operator.Profile.of_string value with
            | Ok profile -> Ok (Some profile)
            | Error message -> Error (Error.make Error.Usage "%s" message))
      in
      let* jobs =
        match jobs with
        | None -> Ok None
        | Some value -> (
            match Ocaml_mutants_core.Positive_int.of_int value with
            | Ok value -> Ok (Some value)
            | Error _ ->
                Error (Error.make Error.Usage "--jobs must be at least one"))
      in
      let* timeout =
        match timeout with
        | None -> Ok None
        | Some value -> (
            match Ocaml_mutants_core.Duration.of_seconds value with
            | Ok value when Ocaml_mutants_core.Duration.to_seconds value > 0. ->
                Ok (Some value)
            | Ok _ | Error _ ->
                Error
                  (Error.make Error.Usage
                     "--timeout must be finite and positive"))
      in
      let* command =
        match command with
        | None -> Ok None
        | Some values -> (
            match Ocaml_mutants_core.Nonempty_argv.of_list values with
            | Ok value -> Ok (Some value)
            | Error message -> Error (Error.make Error.Usage "%s" message))
      in
      let overrides =
        {
          Config.include_ = (if include_ = [] then None else Some include_);
          exclude = (if exclude = [] then None else Some exclude);
          operators;
          profile;
          command;
          timeout = Option.map Option.some timeout;
          jobs;
          cache_mode;
        }
      in
      Ok (Config.apply config overrides)

let run_action path include_ exclude operators profile mutants jobs timeout
    cache_mode fresh changed changed_from json stryker_json threshold_high
    threshold_low quiet no_color color_mode events shard_plan shard_index =
  let root = Unix.realpath path in
  let execute () =
    let* command =
      match !presplit_command with
      | Some [] ->
          Error (Error.make Error.Usage "`--` must be followed by a command")
      | value -> Ok value
    in
    let* config =
      configuration ~root ~include_ ~exclude ~operators ~profile ~command
        ~timeout ~jobs ~cache_mode
    in
    let* selection =
      run_selection ~changed ~changed_from ~mutants ~operators ~shard_plan
        ~shard_index
    in
    let* output =
      match events with
      | Jsonl_events when json || stryker_json ->
          Error
            (Error.make Error.Usage
               "--events jsonl cannot be combined with --json or --stryker-json")
      | Jsonl_events -> Ok (Request.Terminal { quiet = true; color = false })
      | No_events ->
          run_output ~json ~stryker_json ~threshold_high ~threshold_low ~quiet
            ~no_color ~color_mode
    in
    App.run ~root ~config ~fresh ~selection ~output
  in
  let sink =
    match events with
    | No_events -> Event_bus.plain stderr
    | Jsonl_events -> Event_bus.jsonl stdout
  in
  let result =
    Event_bus.with_sink sink (fun () ->
        let result = execute () in
        let exit_code =
          match result with
          | Ok code -> code
          | Error error -> Error.exit_code error
        in
        Event_bus.emit (Event_bus.Run_finished { exit_code });
        result)
  in
  use_result result

let run_term =
  Term.(
    const run_action $ path_argument $ include_options $ exclude_options
    $ operator_options $ profile_option $ mutant_options $ jobs_option
    $ timeout_option $ cache_mode_option $ fresh_option $ changed_option
    $ changed_from_option $ json_option $ stryker_json_option
    $ threshold_high_option $ threshold_low_option $ quiet_option
    $ no_color_option $ color_mode_option $ events_option $ shard_plan_option
    $ shard_index_option)

let run_command =
  Cmd.v
    (command_info ~exits:run_exit_infos "run"
       ~doc:"Discover, instrument, and test mutants in an isolated snapshot.")
    run_term

let list_action path include_ exclude operators profile mutants changed
    changed_from json quiet no_color color_mode =
  let root = Unix.realpath path in
  let result =
    let* config =
      configuration ~root ~include_ ~exclude ~operators ~profile ~command:None
        ~timeout:None ~jobs:None ~cache_mode:None
    in
    let* selection = selection ~changed ~changed_from ~mutants ~operators in
    let* output = output ~json ~quiet ~no_color ~color_mode in
    App.list_mutants ~root ~config ~selection ~output
  in
  use_result result

let list_term =
  Term.(
    const list_action $ path_argument $ include_options $ exclude_options
    $ operator_options $ profile_option $ mutant_options $ changed_option
    $ changed_from_option $ json_option $ quiet_option $ no_color_option
    $ color_mode_option)

let list_command =
  Cmd.v
    (command_info ~exits:interruptible_exit_infos "list"
       ~doc:"List discovered mutants without running tests.")
    list_term

let store_path_option =
  let doc =
    "Resolve the workspace configuration and report store below $(docv)."
  in
  Arg.(value & opt string "." & info [ "path" ] ~docv:"PATH" ~doc)

let store_for_with_warnings ~warn ~root =
  match Config.load_with_metadata root with
  | Error message -> Error (Error.make Error.Usage "%s" message)
  | Ok loaded ->
      if warn then
        List.iter
          (fun warning -> Format.eprintf "ocaml-mutants: warning: %s@." warning)
          loaded.Config.warnings;
      Run_store.create ~workspace:root
        ?directory:loaded.config.Config.cache.directory ()

let store_for ~root = store_for_with_warnings ~warn:true ~root

let report_formats_option =
  Arg.(
    value & opt_all string []
    & info [ "format" ] ~docv:"FORMAT"
        ~doc:
          "Generate terminal, json, html, markdown, sarif, or stryker. May be \
           repeated.")

let report_output_option =
  Arg.(
    value
    & opt (some string) None
    & info [ "o"; "output" ] ~docv:"PATH"
        ~doc:
          "Write one artifact to PATH, or multiple formats below directory \
           PATH.")

let decode_report_formats ~json names =
  if json && names <> [] then
    Error (Error.make Error.Usage "--json cannot be combined with --format")
  else
    let names =
      if json then [ "json" ] else if names = [] then [ "terminal" ] else names
    in
    let rec decode formats = function
      | [] -> Ok (List.rev formats)
      | name :: rest -> (
          match Artifact_report.of_string name with
          | Ok format -> decode (format :: formats) rest
          | Error message -> Error (Error.make Error.Usage "%s" message))
    in
    decode [] names

let report_action path id json no_color color_mode formats output_path
    threshold_high threshold_low =
  let root = Unix.realpath path in
  let result =
    let* formats = decode_report_formats ~json formats in
    let uses_stryker = List.mem Artifact_report.Stryker formats in
    let* stryker_thresholds =
      match (uses_stryker, threshold_high, threshold_low) with
      | false, None, None -> Ok None
      | false, _, _ ->
          Error
            (Error.make Error.Usage
               "--threshold-high/--threshold-low require --format stryker")
      | true, Some high, Some low ->
          Stryker_report.thresholds ~high ~low
          |> Result.map Option.some
          |> Result.map_error (fun message ->
              Error.make Error.Usage "%s" message)
      | true, _, _ ->
          Error
            (Error.make Error.Usage
               "--format stryker requires --threshold-high and --threshold-low")
    in
    let* store = store_for ~root in
    let* run = Run_store.load_run store id in
    let color = output_path = None && terminal_color ~no_color ~color_mode in
    let* rendered =
      let rec render rendered = function
        | [] -> Ok (List.rev rendered)
        | format :: rest ->
            let* contents =
              Artifact_report.render ~root ~color ?stryker_thresholds format run
            in
            render ((format, contents) :: rendered) rest
      in
      render [] formats
    in
    match (rendered, output_path) with
    | [ (_, contents) ], None ->
        print_string contents;
        flush stdout;
        Ok 0
    | [ (_, contents) ], Some "-" ->
        print_string contents;
        flush stdout;
        Ok 0
    | [ (_, contents) ], Some path ->
        Util.atomic_write path contents
        |> Result.map (fun () ->
            Printf.printf "Wrote %s\n" path;
            0)
        |> Result.map_error (fun message ->
            Error.create ~phase:Error.Reporting ~cause:Error.Io_failure
              "cannot write report artifact: %s" message)
    | _ :: _ :: _, None ->
        Error
          (Error.make Error.Usage
             "multiple --format values require --output DIRECTORY")
    | multiple, Some directory ->
        let* () =
          Util.mkdir_p directory
          |> Result.map_error (fun message ->
              Error.create ~phase:Error.Reporting ~cause:Error.Io_failure
                "cannot create report directory: %s" message)
        in
        let run_id =
          Ocaml_mutants_core.Run_id.to_string run.Run_store.metadata.id
        in
        let rec write = function
          | [] -> Ok 0
          | (format, contents) :: rest ->
              let filename =
                Printf.sprintf "%s.%s.%s" run_id
                  (Artifact_report.name format)
                  (Artifact_report.extension format)
              in
              let output = Filename.concat directory filename in
              let* () =
                Util.atomic_write output contents
                |> Result.map_error (fun message ->
                    Error.create ~phase:Error.Reporting ~cause:Error.Io_failure
                      "cannot write %s: %s" output message)
              in
              Printf.printf "Wrote %s\n" output;
              write rest
        in
        write multiple
    | [], _ -> assert false
  in
  use_result result

let report_term =
  let id = Arg.(value & pos 0 string "latest" & info [] ~docv:"RUN_ID") in
  Term.(
    const report_action $ store_path_option $ id $ json_option $ no_color_option
    $ color_mode_option $ report_formats_option $ report_output_option
    $ threshold_high_option $ threshold_low_option)

let report_command =
  Cmd.v
    (command_info "report" ~doc:"Render a stored run (latest by default).")
    report_term

let check_exit_infos =
  [
    Cmd.Exit.info 0 ~doc:"The saved evidence satisfies policy.";
    Cmd.Exit.info 1 ~doc:"Valid evidence violates policy.";
    Cmd.Exit.info 2
      ~doc:"Evidence is broken, incomplete, or uses a disallowed estimate.";
  ]

let check_action path id against json _no_color _color_mode =
  let root = Unix.realpath path in
  let result =
    let* loaded =
      Config.load_with_metadata root
      |> Result.map_error (fun message -> Error.make Error.Usage "%s" message)
    in
    List.iter
      (fun warning -> Format.eprintf "ocaml-mutants: warning: %s@." warning)
      loaded.Config.warnings;
    let* store =
      Run_store.create ~workspace:root
        ?directory:loaded.config.Config.cache.directory ()
    in
    let* run = Run_store.load_run store id in
    let* reference_score =
      match against with
      | None -> Ok None
      | Some reference_id ->
          let* reference = Run_store.load_run store reference_id in
          Ok (Run_store.summary reference).score
    in
    let evaluation =
      Policy.evaluate ~policy:loaded.config.policy ?reference_score run
    in
    if json then print_string (Policy.to_string evaluation)
    else Format.printf "%a%!" Policy.pp evaluation;
    Ok evaluation.exit_code
  in
  use_result result

let check_command =
  let id = Arg.(value & pos 0 string "latest" & info [] ~docv:"RUN_ID") in
  let against =
    Arg.(
      value
      & opt (some string) None
      & info [ "against" ] ~docv:"RUN_ID"
          ~doc:"Compare score-drop policy against this saved run.")
  in
  Cmd.v
    (command_info ~exits:check_exit_infos "check"
       ~doc:"Apply configured policy to authoritative saved evidence.")
    Term.(
      const check_action $ store_path_option $ id $ against $ json_option
      $ no_color_option $ color_mode_option)

let stdio_is_tty () = Unix.isatty Unix.stdin && Unix.isatty Unix.stdout

let ui_action path id no_color color_mode =
  if not (stdio_is_tty ()) then (
    Format.eprintf
      "ocaml-mutants: ui requires an interactive stdin/stdout TTY; use \
       `ocaml-mutants report` for plain text@.";
    2)
  else
    let root = Unix.realpath path in
    match store_for ~root with
    | Error error -> print_error error
    | Ok store -> (
        let runs =
          if String.equal id "latest" then Run_store.list_runs store
          else Result.map (fun run -> [ run ]) (Run_store.load_run store id)
        in
        match runs with
        | Error error -> print_error error
        | Ok runs -> (
            try
              Tui.run_history
                ~color:(terminal_color ~no_color ~color_mode)
                runs;
              0
            with exception_ ->
              print_error
                (Error.create ~phase:Error.Cli ~cause:Error.Io_failure
                   ~context:[ ("exception", Printexc.to_string exception_) ]
                 "terminal UI failed")))

let interactive_action path =
  if not (stdio_is_tty ()) then (
    Format.eprintf
      "ocaml-mutants: no interactive TTY detected; invoke `ocaml-mutants run` \
       to measure or `ocaml-mutants check` to enforce policy@.";
    2)
  else
    let root = Unix.realpath path in
    let history_warning (id, error) =
      ( "stored-report-invalid",
        Format.asprintf "run %s was omitted from history: %a" id Error.pp error )
    in
    let load_history () =
      match Config.load_with_metadata root with
      | Error message -> Error (Error.make Error.Usage "%s" message)
      | Ok loaded ->
          let* store =
            Run_store.create ~workspace:root
              ?directory:loaded.config.Config.cache.directory ()
          in
          let runs, rejected = Run_store.list_runs_best_effort store in
          let warnings =
            List.map
              (fun message -> ("config-compatibility", message))
              loaded.Config.warnings
            @ List.map history_warning rejected
          in
          Ok (runs, warnings)
    in
    let runs, initial_warnings, initial_error =
      match load_history () with
      | Ok (runs, warnings) -> (runs, warnings, None)
      | Error error -> ([], [], Some (Format.asprintf "%a" Error.pp error))
    in
    let reload () = load_history () in
    let color =
      terminal_color ~no_color:false ~color_mode:Color_auto
    in
    let start ~cancel ~emit =
      Event_bus.with_sink (Event_bus.callback emit) (fun () ->
          match Config.load_with_metadata root with
          | Error message -> Error (Error.make Error.Usage "%s" message)
          | Ok loaded ->
              List.iter
                (fun message ->
                  Event_bus.emit
                    (Event_bus.Warning
                       { code = "config-compatibility"; message }))
                loaded.Config.warnings;
              App.run_with_cancel ~cancel ~root ~config:loaded.config
                ~fresh:false ~selection:Request.All
                ~output:(Request.Terminal { quiet = true; color = false }))
    in
    (try
       Tui.run_interactive ?initial_error ~initial_warnings ~color runs ~reload
         ~start;
       0
     with exception_ ->
       print_error
         (Error.create ~phase:Error.Cli ~cause:Error.Io_failure
            ~context:[ ("exception", Printexc.to_string exception_) ]
            "interactive terminal UI failed"))

let ui_command =
  let id = Arg.(value & pos 0 string "latest" & info [] ~docv:"RUN_ID") in
  Cmd.v
    (command_info "ui" ~doc:"Browse run history and actionable mutants.")
    Term.(
      const ui_action $ store_path_option $ id $ no_color_option
      $ color_mode_option)

let with_cli_cancel action =
  let cancel = Cancel.create () in
  try
    let interrupt =
      Signal_service.System.install Sys.sigint (fun () -> Cancel.request cancel)
    in
    let terminate =
      try
        Signal_service.System.install Sys.sigterm (fun () ->
            Cancel.request cancel)
      with exception_ ->
        Signal_service.System.restore interrupt;
        raise exception_
    in
    Fun.protect
      (fun () -> action cancel)
      ~finally:(fun () ->
        Signal_service.System.restore terminate;
        Signal_service.System.restore interrupt)
  with exception_ ->
    Error
      (Error.create ~phase:Error.Cli ~cause:Error.Io_failure
         ~context:[ ("exception", Printexc.to_string exception_) ]
         "cannot install cancellation handlers")

let numeric_version value =
  match String.split_on_char '.' (String.trim value) with
  | major :: minor :: _ -> (
      match (int_of_string_opt major, int_of_string_opt minor) with
      | Some major, Some minor -> Some (major, minor)
      | _ -> None)
  | _ -> None

let doctor_action path deep =
  let root = Unix.realpath path in
  let command name argv =
    let result = Process_supervisor.run ~cwd:root ~env:[] argv in
    let detail =
      let output = String.trim (result.stdout ^ result.stderr) in
      if output = "" then Process_supervisor.status_string result.status
      else output
    in
    (name, Process_supervisor.succeeded result, detail)
  in
  let ocaml = command "ocamlc" [ "ocamlc"; "-version" ] in
  let dune = command "dune" [ "dune"; "--version" ] in
  let version_check name minimum maximum (_, launched, detail) =
    let valid =
      launched
      && Option.fold ~none:false
           ~some:(fun version -> version >= minimum && version < maximum)
           (numeric_version detail)
    in
    ( name,
      valid,
      if valid then detail
      else
        Printf.sprintf "%s (required >=%d.%d,<%d.%d)" detail (fst minimum)
          (snd minimum) (fst maximum) (snd maximum) )
  in
  let config_check =
    match Config.load_with_metadata root with
    | Ok loaded ->
        ( "config-v2",
          true,
          match loaded.origin with
          | Config.Version_2 -> "version 2"
          | Config.Version_1 -> "v1 compatibility; run config migrate --write"
          | Config.Defaults -> "zero-config Dune defaults" )
    | Error message -> ("config-v2", false, message)
  in
  let cache_check =
    match store_for ~root with
    | Ok store -> ("cache-capability", true, Run_store.directory store)
    | Error error -> ("cache-capability", false, Error.message error)
  in
  let inventory_check =
    match
      with_cli_cancel (fun cancel -> Dune_adapter.describe_tests ~cancel ~root)
    with
    | Ok tests ->
        ( "dune-tests",
          true,
          Printf.sprintf "%d test aliases" (List.length tests) )
    | Error error -> ("dune-tests", false, Error.message error)
  in
  let terminal_check =
    match Tui.backend_diagnostic () with
    | Ok detail -> ("terminal-backend", true, detail)
    | Error detail -> ("terminal-backend", false, detail)
  in
  let checks =
    [
      ( "dune-project",
        Sys.file_exists (Filename.concat root "dune-project"),
        if Sys.file_exists (Filename.concat root "dune-project") then root
        else "not found; run from a Dune workspace" );
      version_check "ocamlc" (5, 4) (5, 6) ocaml;
      version_check "dune" (3, 22) (4, 0) dune;
      command "git" [ "git"; "--version" ];
      config_check;
      inventory_check;
      cache_check;
      terminal_check;
    ]
  in
  let ok = ref true in
  List.iter
    (fun (name, passed, detail) ->
      if not passed then ok := false;
      Printf.printf "%-16s %s  %s\n" name
        (if passed then "ok" else "FAIL")
        detail)
    checks;
  Printf.printf "platform         %s\n" Sys.os_type;
  (if deep then
     match Config.load root with
     | Error message ->
         ok := false;
         Printf.printf "%-16s FAIL  %s\n" "deep" message
     | Ok config -> (
         match
           with_cli_cancel (fun cancel ->
               Runner.doctor_deep ~cancel ~root ~config)
         with
         | Error error ->
             ok := false;
             Printf.printf "%-16s FAIL  %s\n" "deep" (Error.message error);
             List.iter
               (fun (key, value) -> Printf.printf "  %s: %s\n" key value)
               (Error.context error);
             Printf.printf "  next: %s\n" (Error.remediation error)
         | Ok diagnostic ->
             Printf.printf
               "%-16s ok  snapshot+analysis+baseline+instrumentation; \
                mutants=%d tests=%d baseline=%.2fs timeout=%.2fs\n"
               "deep" diagnostic.mutants diagnostic.tests
               diagnostic.baseline_seconds diagnostic.timeout_seconds));
  if !ok then 0 else 2

let doctor_command =
  let deep =
    Arg.(
      value & flag
      & info [ "deep" ]
          ~doc:
            "Verify snapshot, analysis, baseline, instrumentation, and \
             readiness without running mutants.")
  in
  Cmd.v
    (command_info "doctor" ~doc:"Check the local Dune and OCaml environment.")
    Term.(const doctor_action $ path_argument $ deep)

let config_init_action path write =
  let root = Unix.realpath path in
  let config_path = Filename.concat root ".ocaml-mutants.toml" in
  if not write then (
    print_string Config.example;
    0)
  else if Sys.file_exists config_path then (
    Format.eprintf "%s already exists@." config_path;
    2)
  else
    match Util.atomic_write config_path Config.example with
    | Ok () ->
        Printf.printf "Created %s\n" config_path;
        0
    | Error message ->
        print_error (Error.make Error.Tool "cannot create config: %s" message)

let config_show_action path =
  let root = Unix.realpath path in
  match Config.load_with_metadata root with
  | Error message -> print_error (Error.make Error.Usage "%s" message)
  | Ok loaded ->
      List.iter
        (fun warning -> Format.eprintf "ocaml-mutants: warning: %s@." warning)
        loaded.warnings;
      print_string (Config.to_toml loaded.config);
      0

let config_check_action path =
  let root = Unix.realpath path in
  match Config.load_with_metadata root with
  | Error message -> print_error (Error.make Error.Usage "%s" message)
  | Ok loaded ->
      List.iter
        (fun warning -> Format.eprintf "ocaml-mutants: warning: %s@." warning)
        loaded.warnings;
      Printf.printf "Configuration is valid (resolved version 2)\n";
      0

let diff_text ~path before after =
  let prefixed prefix value =
    Util.split_lines value
    |> List.map (fun line -> prefix ^ line)
    |> String.concat "\n"
  in
  Printf.sprintf "--- %s\n+++ %s (v2)\n%s\n%s\n" path path (prefixed "-" before)
    (prefixed "+" after)

let config_migrate_action path write =
  let root = Unix.realpath path in
  let config_path = Filename.concat root ".ocaml-mutants.toml" in
  match Util.read_file config_path with
  | Error message ->
      print_error
        (Error.create ~phase:Error.Cli ~cause:Error.Io_failure
           "cannot read %s: %s" config_path message)
  | Ok before -> (
      match Config.parse_with_metadata ~file:config_path before with
      | Error message -> print_error (Error.make Error.Usage "%s" message)
      | Ok loaded ->
          let after = Config.to_toml loaded.config in
          if String.equal before after then (
            Printf.printf "%s is already canonical version 2\n" config_path;
            0)
          else (
            print_string (diff_text ~path:config_path before after);
            if not write then 0
            else
              match Util.atomic_write config_path after with
              | Ok () ->
                  Printf.printf "Migrated %s to version 2\n" config_path;
                  0
              | Error message ->
                  print_error
                    (Error.create ~phase:Error.Cli ~cause:Error.Io_failure
                       "cannot migrate config atomically: %s" message)))

let config_command =
  let write =
    Arg.(
      value & flag
      & info [ "write" ]
          ~doc:"Atomically write the displayed version 2 configuration.")
  in
  let init =
    Cmd.v
      (command_info "init"
         ~doc:"Detect defaults and display or write a version 2 config.")
      Term.(const config_init_action $ store_path_option $ write)
  in
  let show =
    Cmd.v
      (command_info "show" ~doc:"Print the fully resolved version 2 config.")
      Term.(const config_show_action $ store_path_option)
  in
  let check =
    Cmd.v
      (command_info "check" ~doc:"Validate configuration without running.")
      Term.(const config_check_action $ store_path_option)
  in
  let migrate =
    Cmd.v
      (command_info "migrate"
         ~doc:"Show or atomically apply the v1-to-v2 migration diff.")
      Term.(const config_migrate_action $ store_path_option $ write)
  in
  Cmd.group
    (command_info "config" ~doc:"Create, inspect, validate, or migrate config.")
    [ init; show; check; migrate ]

let with_store path action =
  let root = Unix.realpath path in
  match store_for ~root with
  | Error error -> print_error error
  | Ok store -> action store

let cache_stats_action path =
  with_store path (fun store ->
      let files, bytes = Run_store.stats store in
      Printf.printf "%s\n%d files, %Ld bytes\n"
        (Run_store.directory store)
        files bytes;
      0)

let cache_gc_action path days =
  with_store path (fun store ->
      match Run_store.gc store ~older_than_days:days with
      | Ok count ->
          Printf.printf "Removed %d expired cache files\n" count;
          0
      | Error error -> print_error error)

let cache_clean_action path =
  with_store path (fun store ->
      match Run_store.clean store with
      | Ok () ->
          Printf.printf "Cleaned %s\n" (Run_store.directory store);
          0
      | Error error -> print_error error)

let cache_command =
  let stats =
    Cmd.v
      (command_info "stats" ~doc:"Show cache usage.")
      Term.(const cache_stats_action $ store_path_option)
  in
  let days =
    Arg.(
      value & opt int 30
      & info [ "days" ] ~docv:"N" ~doc:"Remove entries older than $(docv) days.")
  in
  let gc =
    Cmd.v
      (command_info "gc" ~doc:"Remove expired cache entries.")
      Term.(const cache_gc_action $ store_path_option $ days)
  in
  let clean =
    Cmd.v
      (command_info "clean" ~doc:"Remove all cached runs and outcomes.")
      Term.(const cache_clean_action $ store_path_option)
  in
  Cmd.group
    (command_info "cache" ~doc:"Inspect or maintain the mutation cache.")
    [ stats; gc; clean ]

let approve ~yes prompt =
  if yes then true
  else if not (stdio_is_tty ()) then (
    Format.eprintf
      "ocaml-mutants: confirmation requires a TTY; review the diff and repeat \
       with --yes@.";
    false)
  else (
    Printf.printf "%s [y/N] %!" prompt;
    match String.lowercase_ascii (String.trim (read_line ())) with
    | "y" | "yes" -> true
    | _ -> false)

let load_stored_mutant ~root ~run_id ~id =
  let* store = store_for ~root in
  let* run = Run_store.load_run store run_id in
  let* result = Mutant_workflow.find run id in
  Ok (run, result)

let mutant_show_action path run_id id =
  let root = Unix.realpath path in
  use_result
    (let* _, result = load_stored_mutant ~root ~run_id ~id in
     print_string (Mutant_workflow.show result);
     Ok 0)

let mutant_rerun_action path run_id id no_color color_mode =
  let root = Unix.realpath path in
  let result =
    let* parent, result = load_stored_mutant ~root ~run_id ~id in
    let* loaded =
      Config.load_with_metadata root
      |> Result.map_error (fun message -> Error.make Error.Usage "%s" message)
    in
    let config =
      {
        loaded.config with
        execution = { loaded.config.execution with mode = Config.Strict };
        cache =
          {
            loaded.config.cache with
            mode = Config.Off;
            historical_reuse = Config.Reuse_off;
          };
      }
    in
    let mutant_id =
      Ocaml_mutants_core.Mutant.Id.full
        (Ocaml_mutants_core.Mutant.id result.Run_store.mutant)
    in
    App.run ~root ~config ~fresh:true
      ~selection:
        (Request.Rerun
           {
             parent_run_id =
               Ocaml_mutants_core.Run_id.to_string parent.metadata.id;
             mutant_id;
           })
      ~output:
        (Request.Terminal
           { quiet = false; color = terminal_color ~no_color ~color_mode })
  in
  use_result result

let mutant_apply_action path run_id id yes =
  let root = Unix.realpath path in
  let result =
    let* _, result = load_stored_mutant ~root ~run_id ~id in
    print_string (Mutant_workflow.patch result.mutant);
    if not (approve ~yes "Apply this mutant without invoking Git?") then
      Error (Error.make Error.Usage "mutant apply was not confirmed")
    else
      let* () = Mutant_workflow.apply ~root result.mutant in
      Printf.printf "Applied %s; private undo record created\n"
        (Ocaml_mutants_core.Mutant.Id.full
           (Ocaml_mutants_core.Mutant.id result.mutant));
      Ok 0
  in
  use_result result

let mutant_revert_action path id yes =
  let root = Unix.realpath path in
  let result =
    let* patch = Mutant_workflow.revert_patch ~root ~id in
    print_string patch;
    if not (approve ~yes "Revert this applied mutant without invoking Git?")
    then Error (Error.make Error.Usage "mutant revert was not confirmed")
    else
      let* () = Mutant_workflow.revert ~root ~id in
      Printf.printf "Reverted mutant %s\n" id;
      Ok 0
  in
  use_result result

let mutant_expect_action path run_id id reason yes =
  let root = Unix.realpath path in
  let result =
    let* _, result = load_stored_mutant ~root ~run_id ~id in
    let full_id =
      Ocaml_mutants_core.Mutant.Id.full
        (Ocaml_mutants_core.Mutant.id result.Run_store.mutant)
    in
    let* loaded =
      Config.load_with_metadata root
      |> Result.map_error (fun message -> Error.make Error.Usage "%s" message)
    in
    let* edit =
      Mutant_workflow.prepare_expectation ~root ~config:loaded ~id:full_id
        ~reason
    in
    print_string (diff_text ~path:edit.path edit.before edit.after);
    if not (approve ~yes "Save this expectation atomically?") then
      Error (Error.make Error.Usage "expectation edit was not confirmed")
    else
      let* () = Mutant_workflow.commit_expectation edit in
      Printf.printf "Expected mutant %s\n" full_id;
      Ok 0
  in
  use_result result

let mutant_command =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"ID") in
  let run_id =
    Arg.(
      value & opt string "latest"
      & info [ "run" ] ~docv:"RUN_ID" ~doc:"Select a saved parent run.")
  in
  let yes =
    Arg.(
      value & flag
      & info [ "yes" ]
          ~doc:"Confirm the exact displayed source or configuration edit.")
  in
  let show =
    Cmd.v
      (command_info "show" ~doc:"Show one mutant and its complete evidence.")
      Term.(const mutant_show_action $ store_path_option $ run_id $ id)
  in
  let rerun =
    Cmd.v
      (command_info ~exits:run_exit_infos "rerun"
         ~doc:"Run only this mutant in strict mode as a child run.")
      Term.(
        const mutant_rerun_action $ store_path_option $ run_id $ id
        $ no_color_option $ color_mode_option)
  in
  let apply =
    Cmd.v
      (command_info "apply"
         ~doc:"Apply a mutant after digest/range checks and create undo data.")
      Term.(const mutant_apply_action $ store_path_option $ run_id $ id $ yes)
  in
  let revert =
    Cmd.v
      (command_info "revert"
         ~doc:"Restore an applied mutant using its private guarded undo data.")
      Term.(const mutant_revert_action $ store_path_option $ id $ yes)
  in
  let reason =
    Arg.(
      required
      & opt (some string) None
      & info [ "reason" ] ~docv:"TEXT"
          ~doc:"Required explanation for the expected mutant.")
  in
  let expect =
    Cmd.v
      (command_info "expect"
         ~doc:"Append a strict full-ID expectation while preserving TOML text.")
      Term.(
        const mutant_expect_action $ store_path_option $ run_id $ id $ reason
        $ yes)
  in
  Cmd.group
    (command_info "mutant"
       ~doc:"Inspect, rerun, apply, revert, or expect mutants.")
    [ show; rerun; apply; revert; expect ]

let plan_action path include_ exclude operators profile changed changed_from
    shard_count durations_from output_path =
  let root = Unix.realpath path in
  let result =
    if shard_count < 1 then
      Error (Error.make Error.Usage "--shards must be at least one")
    else
      let* config =
        configuration ~root ~include_ ~exclude ~operators ~profile ~command:None
          ~timeout:None ~jobs:None ~cache_mode:None
      in
      let* selection =
        selection ~changed ~changed_from ~mutants:[] ~operators
      in
      let* durations =
        match durations_from with
        | None -> Ok []
        | Some run_id ->
            let* store = store_for ~root in
            let* run = Run_store.load_run store run_id in
            Ok
              (List.map
                 (fun result ->
                   ( Ocaml_mutants_core.Mutant.Id.full
                       (Ocaml_mutants_core.Mutant.id result.Run_store.mutant),
                     Ocaml_mutants_core.Duration.to_seconds result.duration ))
                 run.results)
      in
      let* plan =
        with_cli_cancel (fun cancel ->
            Runner.create_shard_plan ~cancel ~root ~config ~selection
              ~shard_count ~durations)
      in
      let contents = Shard_plan.to_string plan in
      match output_path with
      | None | Some "-" ->
          print_string contents;
          flush stdout;
          Ok 0
      | Some path ->
          Util.atomic_write path contents
          |> Result.map (fun () ->
              Printf.printf "Wrote deterministic shard plan %s\n" path;
              0)
          |> Result.map_error (fun message ->
              Error.create ~phase:Error.Reporting ~cause:Error.Io_failure
                "cannot write shard plan: %s" message)
  in
  use_result result

let plan_command =
  let shard_count =
    Arg.(
      required
      & opt (some int) None
      & info [ "shards" ] ~docv:"N" ~doc:"Create exactly N shards.")
  in
  let durations_from =
    Arg.(
      value
      & opt (some string) None
      & info [ "durations-from" ] ~docv:"RUN_ID"
          ~doc:"Balance using observed durations from this saved run.")
  in
  let output_path =
    Arg.(
      value
      & opt (some string) None
      & info [ "o"; "output" ] ~docv:"PLAN.json"
          ~doc:"Write the plan atomically; default is standard output.")
  in
  Cmd.v
    (command_info ~exits:interruptible_exit_infos "plan"
       ~doc:"Create a deterministic, fingerprinted CI shard plan.")
    Term.(
      const plan_action $ path_argument $ include_options $ exclude_options
      $ operator_options $ profile_option $ changed_option $ changed_from_option
      $ shard_count $ durations_from $ output_path)

let load_run_reference store reference =
  if Sys.file_exists reference && not (Sys.is_directory reference) then
    let* contents =
      Util.read_file reference
      |> Result.map_error (fun message ->
          Error.create ~phase:Error.Reporting ~cause:Error.Io_failure
            "cannot read shard report %s: %s" reference message)
    in
    try
      Run_store.run_of_json (Yojson.Safe.from_string contents)
      |> Result.map_error (fun message ->
          Error.create ~phase:Error.Reporting ~cause:Error.Decode_failure
            "invalid shard report %s: %s" reference message)
    with Yojson.Json_error message ->
      Error
        (Error.create ~phase:Error.Reporting ~cause:Error.Decode_failure
           "invalid shard report %s: %s" reference message)
  else Run_store.load_run store reference

let merge_action path plan_path reports =
  let root = Unix.realpath path in
  let result =
    if reports = [] then
      Error (Error.make Error.Usage "merge requires every shard report")
    else
      let* plan = load_shard_plan plan_path in
      let* store = store_for ~root in
      let rec load loaded = function
        | [] -> Ok (List.rev loaded)
        | reference :: rest ->
            let* run = load_run_reference store reference in
            load (run :: loaded) rest
      in
      let* runs = load [] reports in
      let started_at = Util.timestamp () in
      let* reservation = Run_store.reserve store ~started_at in
      let fail error =
        match Run_store.abandon_reservation store reservation with
        | Ok () -> Error error
        | Error cleanup -> Error (Error.suppress error cleanup)
      in
      match
        Shard_plan.merge ~plan
          ~id:(Run_store.reservation_id reservation)
          ~finished_at:(Util.timestamp ()) runs
      with
      | Error message ->
          fail
            (Error.create ~phase:Error.Reporting ~cause:Error.Invalid_input "%s"
               message)
      | Ok merged -> (
          match Run_store.stage_run store reservation with
          | Error error -> fail error
          | Ok staged -> (
              match Run_store.finalize_run staged with
              | Error error -> fail error
              | Ok finalization ->
                  let merged =
                    if finalization.cleanup_errors = [] then merged
                    else
                      {
                        merged with
                        warnings =
                          merged.warnings
                          @ List.map
                              (fun error ->
                                {
                                  Run_store.code = "merge-cleanup-advisory";
                                  message = Error.message error;
                                })
                              finalization.cleanup_errors;
                      }
                  in
                  let* published =
                    Run_store.publish_run finalization.publication merged
                  in
                  List.iter
                    (fun advisory ->
                      Format.eprintf "ocaml-mutants: advisory: %a@." Error.pp
                        advisory)
                    published.advisories;
                  Printf.printf "Merged complete run %s\n"
                    (Ocaml_mutants_core.Run_id.to_string
                       published.run.metadata.id);
                  Ok 0))
  in
  use_result result

let merge_command =
  let plan =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PLAN.json")
  in
  let reports =
    Arg.(
      value & pos_right 0 string []
      & info [] ~docv:"REPORT_OR_RUN_ID"
          ~doc:"All shard reports, as paths or saved run IDs.")
  in
  Cmd.v
    (command_info "merge"
       ~doc:"Validate and merge every result from one deterministic plan.")
    Term.(const merge_action $ store_path_option $ plan $ reports)

let subcommands =
  [
    run_command;
    check_command;
    plan_command;
    merge_command;
    mutant_command;
    ui_command;
    list_command;
    report_command;
    doctor_command;
    config_command;
    cache_command;
  ]

let default_action () =
  interactive_action "."

let default_term = Term.(const default_action $ const ())

let main_command =
  let doc = "type-aware mutation testing for Dune projects" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "ocaml-mutants reads compiler Typedtrees from a normal Dune build, \
         instruments one isolated workspace snapshot, and never edits the \
         source workspace.";
      `S Manpage.s_bugs;
      `P "Report issues at https://github.com/P4suta/ocaml-mutants/issues.";
    ]
  in
  Cmd.group ~default:default_term
    (Cmd.info "ocaml-mutants" ~version:"1.0.0" ~doc ~man
       ~exits:standard_exit_infos)
    subcommands

let () =
  if Process_supervisor.helper_requested Sys.argv then
    exit (Process_supervisor.run_helper Sys.argv)
  else
    let executable =
      try Unix.realpath Sys.executable_name
      with Unix.Unix_error _ -> Sys.executable_name
    in
    Process_supervisor.configure_helper_executable executable;
    let argv =
      split_run_argv ~subcommands:(List.map Cmd.name subcommands) Sys.argv
    in
    let code =
      match Cmd.eval_value ~argv main_command with
      | Ok (`Ok code) -> code
      | Ok (`Help | `Version) -> 0
      | Error (`Parse | `Term | `Exn) -> 2
    in
    exit code
