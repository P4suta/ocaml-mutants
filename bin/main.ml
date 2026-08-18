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

let split_run_argv argv =
  if Array.length argv < 2 || argv.(1) <> "run" then argv
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
    Cmd.Exit.info 0 ~doc:"Every mutant was detected.";
    Cmd.Exit.info 1 ~doc:"One or more mutants survived.";
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
        ~doc:"Select balanced, strong, or all mutation rules.")

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

let decode_operators values =
  let rec loop decoded = function
    | [] -> Ok (List.rev decoded)
    | value :: rest -> (
        match Ocaml_mutants_core.Operator.of_string value with
        | Ok operator -> loop (operator :: decoded) rest
        | Error message -> Error (Error.make Error.Usage "%s" message))
  in
  if values = [] then Ok None else Result.map Option.some (loop [] values)

let terminal_color ~no_color =
  (not no_color)
  && Sys.getenv_opt "NO_COLOR" = None
  && Sys.getenv_opt "CI" = None
  && Unix.isatty Unix.stdout

let output ~json ~quiet ~no_color =
  let color = terminal_color ~no_color in
  if json && quiet then
    Error (Error.make Error.Usage "--json and --quiet cannot be used together")
  else if json then Ok Request.Json
  else Ok (Request.Terminal { quiet; color })

let run_output ~json ~stryker_json ~threshold_high ~threshold_low ~quiet
    ~no_color =
  if not stryker_json then
    if threshold_high <> None || threshold_low <> None then
      Error
        (Error.make Error.Usage
           "--threshold-high and --threshold-low require --stryker-json")
    else output ~json ~quiet ~no_color
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

let validate_configured_mutant_selection ~mutants config =
  let all_families = Ocaml_mutants_core.Operator.Family.all in
  let configured = config.Config.mutation.operators in
  let complete_operator_set =
    List.length configured = List.length all_families
    && List.for_all (fun family -> List.mem family configured) all_families
  in
  if mutants <> [] && not complete_operator_set then
    Error
      (Error.make Error.Usage
         "--mutant cannot be combined with mutation.operators from the \
          configuration")
  else Ok ()

let configuration ~root ~include_ ~exclude ~operators ~profile ~command ~timeout
    ~jobs ~cache_mode =
  match Config.load root with
  | Error message -> Error (Error.make Error.Usage "%s" message)
  | Ok config ->
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
    threshold_low quiet no_color =
  let root = Unix.realpath path in
  let result =
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
    let* () = validate_configured_mutant_selection ~mutants config in
    let* selection = selection ~changed ~changed_from ~mutants ~operators in
    let* output =
      run_output ~json ~stryker_json ~threshold_high ~threshold_low ~quiet
        ~no_color
    in
    App.run ~root ~config ~fresh ~selection ~output
  in
  use_result result

let run_term =
  Term.(
    const run_action $ path_argument $ include_options $ exclude_options
    $ operator_options $ profile_option $ mutant_options $ jobs_option
    $ timeout_option $ cache_mode_option $ fresh_option $ changed_option
    $ changed_from_option $ json_option $ stryker_json_option
    $ threshold_high_option $ threshold_low_option $ quiet_option
    $ no_color_option)

let run_command =
  Cmd.v
    (command_info ~exits:run_exit_infos "run"
       ~doc:"Discover, instrument, and test mutants in an isolated snapshot.")
    run_term

let list_action path include_ exclude operators profile mutants changed
    changed_from json quiet no_color =
  let root = Unix.realpath path in
  let result =
    let* config =
      configuration ~root ~include_ ~exclude ~operators ~profile ~command:None
        ~timeout:None ~jobs:None ~cache_mode:None
    in
    let* () = validate_configured_mutant_selection ~mutants config in
    let* selection = selection ~changed ~changed_from ~mutants ~operators in
    let* output = output ~json ~quiet ~no_color in
    App.list_mutants ~root ~config ~selection ~output
  in
  use_result result

let list_term =
  Term.(
    const list_action $ path_argument $ include_options $ exclude_options
    $ operator_options $ profile_option $ mutant_options $ changed_option
    $ changed_from_option $ json_option $ quiet_option $ no_color_option)

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

let store_for ~root =
  match Config.load root with
  | Error message -> Error (Error.make Error.Usage "%s" message)
  | Ok config ->
      Run_store.create ~workspace:root ?directory:config.Config.cache.directory
        ()

let report_action path id json no_color =
  let root = Unix.realpath path in
  match store_for ~root with
  | Error error -> print_error error
  | Ok store -> (
      match Run_store.load_run store id with
      | Error error -> print_error error
      | Ok run ->
          if json then print_string (Run_store.run_to_string run)
          else
            Report.print_run ~color:(terminal_color ~no_color)
              Format.std_formatter run;
          0)

let report_term =
  let id = Arg.(value & pos 0 string "latest" & info [] ~docv:"RUN_ID") in
  Term.(
    const report_action $ store_path_option $ id $ json_option $ no_color_option)

let report_command =
  Cmd.v
    (command_info "report" ~doc:"Render a stored run (latest by default).")
    report_term

let doctor_action path =
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
  let checks =
    [
      ( "dune-project",
        Sys.file_exists (Filename.concat root "dune-project"),
        if Sys.file_exists (Filename.concat root "dune-project") then root
        else "not found" );
      command "ocamlc" [ "ocamlc"; "-version" ];
      command "dune" [ "dune"; "--version" ];
      command "git" [ "git"; "--version" ];
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
  if !ok then 0 else 2

let doctor_command =
  Cmd.v
    (command_info "doctor" ~doc:"Check the local Dune and OCaml environment.")
    Term.(const doctor_action $ path_argument)

let init_action path =
  let root = Unix.realpath path in
  let config_path = Filename.concat root ".ocaml-mutants.toml" in
  if Sys.file_exists config_path then (
    Format.eprintf "%s already exists@." config_path;
    2)
  else
    match Util.atomic_write config_path Config.example with
    | Ok () ->
        Printf.printf "Created %s\n" config_path;
        0
    | Error message ->
        print_error (Error.make Error.Tool "cannot create config: %s" message)

let init_command =
  Cmd.v
    (command_info "init"
       ~doc:"Write a documented .ocaml-mutants.toml starter file.")
    Term.(const init_action $ path_argument)

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
      `P
        "Report issues at \
         https://github.com/ocaml-mutants/ocaml-mutants/issues.";
    ]
  in
  Cmd.group ~default:run_term
    (Cmd.info "ocaml-mutants" ~version:"0.1.0" ~doc ~man ~exits:run_exit_infos)
    [
      run_command;
      list_command;
      report_command;
      doctor_command;
      init_command;
      cache_command;
    ]

let () =
  if Process_supervisor.helper_requested Sys.argv then
    exit (Process_supervisor.run_helper Sys.argv)
  else
    let executable =
      try Unix.realpath Sys.executable_name
      with Unix.Unix_error _ -> Sys.executable_name
    in
    Process_supervisor.configure_helper_executable executable;
    let argv = split_run_argv Sys.argv in
    let code =
      match Cmd.eval_value ~argv main_command with
      | Ok (`Ok code) -> code
      | Ok (`Help | `Version) -> 0
      | Error (`Parse | `Term | `Exn) -> 2
    in
    exit code
