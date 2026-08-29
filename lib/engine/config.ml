module Core = Ocaml_mutants_core

type cache_mode = Auto | On | Off
type historical_reuse = Reuse_off | Reuse_exact | Reuse_estimated
type execution_mode = Strict | Fast
type test_driver = Auto_driver | Dune_driver | Command_driver

type artifact_format =
  | Terminal
  | Native_json
  | Html
  | Markdown
  | Sarif
  | Stryker

type source_embedding = Context | All_source | No_source
type expectation = { id : string; reason : string }
type stage = { name : string; command : Core.Nonempty_argv.t }

type mutation = {
  include_ : string list;
  exclude : string list;
  operators : Core.Operator.Family.t list;
  profile : Core.Operator.Profile.t;
  expectations : expectation list;
}

type test = {
  driver : test_driver;
  command : Core.Nonempty_argv.t;
  stages : stage list;
  timeout : Core.Duration.t option;
  baseline_runs : Core.Positive_int.t;
  parallel_safe : bool;
  external_inputs : string list;
  reproducible : bool;
}

type execution = { mode : execution_mode; jobs : Core.Positive_int.t }

type cache = {
  mode : cache_mode;
  directory : string option;
  historical_reuse : historical_reuse;
}

type policy = {
  require_complete : bool;
  max_unexpected_survivors : int;
  minimum_score : float option;
  maximum_score_drop : float option;
  allow_estimated : bool;
}

type report = { formats : artifact_format list; directory : string option }

type privacy = {
  stdout_limit_bytes : int;
  stderr_limit_bytes : int;
  redactions : string list;
  source_embedding : source_embedding;
}

type t = {
  version : int;
  mutation : mutation;
  test : test;
  execution : execution;
  cache : cache;
  policy : policy;
  report : report;
  privacy : privacy;
}

type overrides = {
  include_ : string list option;
  exclude : string list option;
  operators : Core.Operator.Family.t list option;
  profile : Core.Operator.Profile.t option;
  command : Core.Nonempty_argv.t option;
  timeout : Core.Duration.t option option;
  jobs : Core.Positive_int.t option;
  cache_mode : cache_mode option;
}

let get_ok = function Ok value -> value | Error message -> invalid_arg message

let default_command =
  get_ok (Core.Nonempty_argv.of_list [ "dune"; "runtest"; "--force" ])

let defaults =
  {
    version = 2;
    mutation =
      {
        include_ = [ "**/*.ml" ];
        exclude = [ "**/test/**"; "**/tests/**"; "**/_build/**" ];
        operators = Core.Operator.Family.all;
        profile = Core.Operator.Profile.Balanced;
        expectations = [];
      };
    test =
      {
        driver = Auto_driver;
        command = default_command;
        stages = [ { name = "full"; command = default_command } ];
        timeout = None;
        baseline_runs = get_ok (Core.Positive_int.of_int 3);
        parallel_safe = false;
        external_inputs = [];
        reproducible = true;
      };
    execution =
      {
        mode = Strict;
        jobs =
          get_ok
            (Core.Positive_int.of_int
               (max 1 (Domain.recommended_domain_count () - 1)));
      };
    cache = { mode = Off; directory = None; historical_reuse = Reuse_off };
    policy =
      {
        require_complete = true;
        max_unexpected_survivors = 0;
        minimum_score = None;
        maximum_score_drop = None;
        allow_estimated = false;
      };
    report = { formats = [ Terminal; Native_json ]; directory = None };
    privacy =
      {
        stdout_limit_bytes = 65536;
        stderr_limit_bytes = 65536;
        redactions = [];
        source_embedding = Context;
      };
  }

let empty_overrides =
  {
    include_ = None;
    exclude = None;
    operators = None;
    profile = None;
    command = None;
    timeout = None;
    jobs = None;
    cache_mode = None;
  }

module Location_index = struct
  type location = { line : int; column : int }
  type t = (string, location) Hashtbl.t

  let remove_comment line =
    let rec loop quoted escaped index =
      if index = String.length line then line
      else
        match line.[index] with
        | '"' when not escaped -> loop (not quoted) false (index + 1)
        | '#' when not quoted -> String.sub line 0 index
        | '\\' when quoted -> loop quoted (not escaped) (index + 1)
        | _ -> loop quoted false (index + 1)
    in
    loop false false 0

  let first_non_space line =
    let rec loop index =
      if index = String.length line then 0
      else match line.[index] with ' ' | '\t' -> loop (index + 1) | _ -> index
    in
    loop 0

  let create contents =
    let index = Hashtbl.create 32 in
    let table_array_indices = Hashtbl.create 8 in
    let section = ref "" in
    Util.split_lines contents
    |> List.iteri (fun line_index raw ->
        let line = remove_comment raw |> String.trim in
        if String.length line >= 3 && line.[0] = '[' then (
          let last = String.length line - 1 in
          if line.[last] = ']' then (
            let table_array =
              String.length line >= 4 && line.[1] = '[' && line.[last - 1] = ']'
            in
            let opening, closing = if table_array then (2, 2) else (1, 1) in
            let name =
              String.sub line opening (String.length line - opening - closing)
              |> String.trim
            in
            section :=
              if table_array then (
                let row =
                  Option.value
                    (Hashtbl.find_opt table_array_indices name)
                    ~default:0
                in
                Hashtbl.replace table_array_indices name (row + 1);
                Printf.sprintf "%s.%d" name row)
              else name;
            Hashtbl.replace index !section
              { line = line_index + 1; column = first_non_space raw + 1 }))
        else
          match String.index_opt line '=' with
          | None -> ()
          | Some equals ->
              let key = String.sub line 0 equals |> String.trim in
              if key <> "" then
                let path =
                  if !section = "" then key else !section ^ "." ^ key
                in
                Hashtbl.replace index path
                  { line = line_index + 1; column = first_non_space raw + 1 });
    index

  let find index path =
    let rec parent reversed = function
      | [] | [ _ ] -> List.rev reversed
      | component :: rest -> parent (component :: reversed) rest
    in
    let rec lookup = function
      | [] -> None
      | path -> (
          match Hashtbl.find_opt index (String.concat "." path) with
          | Some _ as location -> location
          | None -> lookup (parent [] path))
    in
    Option.value (lookup path) ~default:{ line = 1; column = 1 }
end

type diagnostic = { path : string list; message : string }
type 'a validation = ('a, diagnostic list) result

let valid value = Ok value
let invalid path message = Error [ { path; message } ]

let map function_ = function
  | Ok value -> Ok (function_ value)
  | Error errors -> Error errors

let both left right =
  match (left, right) with
  | Ok left, Ok right -> Ok (left, right)
  | Error left, Error right -> Error (left @ right)
  | Error errors, Ok _ | Ok _, Error errors -> Error errors

let ( let+ ) value function_ = map function_ value
let ( and+ ) = both

let type_name = function
  | Otoml.TomlString _ -> "string"
  | Otoml.TomlInteger _ -> "integer"
  | Otoml.TomlFloat _ -> "float"
  | Otoml.TomlBoolean _ -> "boolean"
  | Otoml.TomlArray _ -> "array"
  | Otoml.TomlTable _ -> "table"
  | Otoml.TomlInlineTable _ -> "inline table"
  | Otoml.TomlTableArray _ -> "table array"
  | Otoml.TomlOffsetDateTime _ | Otoml.TomlLocalDateTime _
  | Otoml.TomlLocalDate _ | Otoml.TomlLocalTime _ ->
      "date/time"

let rec lookup value path =
  match path with
  | [] -> Some value
  | key :: rest -> (
      match value with
      | Otoml.TomlTable entries | Otoml.TomlInlineTable entries ->
          Option.bind (List.assoc_opt key entries) (fun value ->
              lookup value rest)
      | _ -> None)

let optional root path decode =
  match lookup root path with
  | None -> valid None
  | Some value -> map Option.some (decode path value)

let string path = function
  | Otoml.TomlString value -> valid value
  | value ->
      invalid path (Printf.sprintf "expected string, got %s" (type_name value))

let integer path = function
  | Otoml.TomlInteger value -> valid value
  | value ->
      invalid path (Printf.sprintf "expected integer, got %s" (type_name value))

let number path = function
  | Otoml.TomlFloat value -> valid value
  | Otoml.TomlInteger value -> valid (float_of_int value)
  | value ->
      invalid path (Printf.sprintf "expected number, got %s" (type_name value))

let boolean path = function
  | Otoml.TomlBoolean value -> valid value
  | value ->
      invalid path (Printf.sprintf "expected boolean, got %s" (type_name value))

let strings path = function
  | Otoml.TomlArray values ->
      List.fold_left
        (fun result value ->
          match (result, string path value) with
          | Ok values, Ok value -> Ok (value :: values)
          | Error left, Error right -> Error (left @ right)
          | Error errors, Ok _ | Ok _, Error errors -> Error errors)
        (Ok []) values
      |> map List.rev
  | value ->
      invalid path
        (Printf.sprintf "expected string array, got %s" (type_name value))

let nonempty_strings path value =
  match strings path value with
  | Error _ as error -> error
  | Ok values when List.for_all (fun value -> String.trim value <> "") values ->
      valid values
  | Ok _ -> invalid path "array entries must be non-empty strings"

let positive_int path value =
  match integer path value with
  | Error _ as error -> error
  | Ok value -> (
      match Core.Positive_int.of_int value with
      | Ok value -> valid value
      | Error message -> invalid path message)

let nonnegative_int path value =
  match integer path value with
  | Error _ as error -> error
  | Ok value when value >= 0 -> valid value
  | Ok _ -> invalid path "expected a non-negative integer"

let percentage path value =
  match number path value with
  | Error _ as error -> error
  | Ok value when Float.is_finite value && value >= 0. && value <= 100. ->
      valid value
  | Ok _ -> invalid path "expected a finite percentage between 0 and 100"

let duration path value =
  match number path value with
  | Error _ as error -> error
  | Ok value -> (
      match Core.Duration.of_seconds value with
      | Ok value when Core.Duration.to_seconds value > 0. -> valid value
      | Ok _ -> invalid path "duration must be positive"
      | Error message -> invalid path message)

let timeout path = function
  | Otoml.TomlString "auto" -> valid None
  | Otoml.TomlString value ->
      invalid path
        (Printf.sprintf "expected \"auto\" or a positive number, got %S" value)
  | value -> map Option.some (duration path value)

let jobs path = function
  | Otoml.TomlString "auto" -> valid defaults.execution.jobs
  | Otoml.TomlString value ->
      invalid path
        (Printf.sprintf "expected \"auto\" or a positive integer, got %S" value)
  | value -> positive_int path value

let command path value =
  match strings path value with
  | Error _ as error -> error
  | Ok values -> (
      match Core.Nonempty_argv.of_list values with
      | Ok value -> valid value
      | Error message -> invalid path message)

let required table path key decode =
  match lookup table [ key ] with
  | None -> invalid (path @ [ key ]) "missing required key"
  | Some value -> decode (path @ [ key ]) value

let row_keys path ~allowed entries =
  let errors =
    List.filter_map
      (fun (key, _) ->
        if List.mem key allowed then None
        else
          Some { path = path @ [ key ]; message = "unknown configuration key" })
      entries
  in
  if errors = [] then valid () else Error errors

let valid_full_id value =
  String.length value = 64
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
       value

let expectation path = function
  | Otoml.TomlTable entries as table -> (
      match
        both
          (row_keys path ~allowed:[ "id"; "reason" ] entries)
          (both
             (required table path "id" string)
             (required table path "reason" string))
      with
      | Error _ as error -> error
      | Ok ((), (id, reason)) ->
          if not (valid_full_id id) then
            invalid (path @ [ "id" ])
              "expected a 64-character lowercase hexadecimal mutant ID"
          else if String.trim reason = "" then
            invalid (path @ [ "reason" ]) "expectation reason must not be empty"
          else valid { id; reason })
  | value ->
      invalid path (Printf.sprintf "expected table, got %s" (type_name value))

let stage path = function
  | Otoml.TomlTable entries as table -> (
      match
        both
          (row_keys path ~allowed:[ "name"; "command" ] entries)
          (both
             (required table path "name" string)
             (required table path "command" command))
      with
      | Error _ as error -> error
      | Ok ((), (name, command)) ->
          if String.trim name = "" then
            invalid (path @ [ "name" ]) "stage name must not be empty"
          else valid { name; command })
  | value ->
      invalid path (Printf.sprintf "expected table, got %s" (type_name value))

let table_array row path = function
  | Otoml.TomlTableArray values ->
      let rec decode index decoded errors = function
        | [] ->
            if errors = [] then valid (List.rev decoded)
            else Error (List.rev errors)
        | value :: rest -> (
            match row (path @ [ string_of_int index ]) value with
            | Ok value -> decode (index + 1) (value :: decoded) errors rest
            | Error row_errors ->
                decode (index + 1) decoded
                  (List.rev_append row_errors errors)
                  rest)
      in
      decode 0 [] [] values
  | value ->
      invalid path
        (Printf.sprintf "expected table array, got %s" (type_name value))

let require_unique path ~field ~description key values =
  let seen = Hashtbl.create (List.length values) in
  let errors = ref [] in
  List.iteri
    (fun index value ->
      let key = key value in
      match Hashtbl.find_opt seen key with
      | None -> Hashtbl.add seen key index
      | Some first_index ->
          errors :=
            {
              path = path @ [ string_of_int index; field ];
              message =
                Printf.sprintf "duplicate %s (first declared at index %d)"
                  description first_index;
            }
            :: !errors)
    values;
  match List.rev !errors with [] -> valid values | errors -> Error errors

let expectations path value =
  match table_array expectation path value with
  | Error _ as error -> error
  | Ok values ->
      require_unique path ~field:"id" ~description:"expectation mutant ID"
        (fun expectation -> expectation.id)
        values

let stages path value =
  match table_array stage path value with
  | Ok [] -> invalid path "at least one test stage is required"
  | Error _ as error -> error
  | Ok values ->
      require_unique path ~field:"name" ~description:"test stage name"
        (fun stage -> stage.name)
        values

let operators path value =
  match strings path value with
  | Error _ as error -> error
  | Ok names ->
      List.fold_left
        (fun result name ->
          match (result, Core.Operator.Family.of_string name) with
          | Ok values, Ok value -> Ok (value :: values)
          | Error errors, Ok _ -> Error errors
          | Ok _, Error message -> Error [ { path; message } ]
          | Error errors, Error message -> Error ({ path; message } :: errors))
        (Ok []) names
      |> map List.rev

let cache_mode path value =
  match string path value with
  | Error _ as error -> error
  | Ok "auto" -> valid Auto
  | Ok "on" -> valid On
  | Ok "off" -> valid Off
  | Ok value ->
      invalid path
        (Printf.sprintf "expected \"auto\", \"on\", or \"off\", got %S" value)

let historical_reuse path value =
  match string path value with
  | Error _ as error -> error
  | Ok "off" -> valid Reuse_off
  | Ok "exact" -> valid Reuse_exact
  | Ok "estimated" -> valid Reuse_estimated
  | Ok value ->
      invalid path
        (Printf.sprintf "expected \"off\", \"exact\", or \"estimated\", got %S"
           value)

let execution_mode path value =
  match string path value with
  | Error _ as error -> error
  | Ok "strict" -> valid Strict
  | Ok "fast" -> valid Fast
  | Ok value ->
      invalid path
        (Printf.sprintf "expected \"strict\" or \"fast\", got %S" value)

let test_driver path value =
  match string path value with
  | Error _ as error -> error
  | Ok "auto" -> valid Auto_driver
  | Ok "dune" -> valid Dune_driver
  | Ok "command" -> valid Command_driver
  | Ok value ->
      invalid path
        (Printf.sprintf "expected \"auto\", \"dune\", or \"command\", got %S"
           value)

let artifact_format path value =
  match string path value with
  | Error _ as error -> error
  | Ok "terminal" -> valid Terminal
  | Ok "json" -> valid Native_json
  | Ok "html" -> valid Html
  | Ok "markdown" -> valid Markdown
  | Ok "sarif" -> valid Sarif
  | Ok "stryker" -> valid Stryker
  | Ok value ->
      invalid path
        (Printf.sprintf
           "expected terminal, json, html, markdown, sarif, or stryker, got %S"
           value)

let artifact_format_name = function
  | Terminal -> "terminal"
  | Native_json -> "json"
  | Html -> "html"
  | Markdown -> "markdown"
  | Sarif -> "sarif"
  | Stryker -> "stryker"

let artifact_formats path = function
  | Otoml.TomlArray values -> (
      let decoded =
        List.fold_left
          (fun result value ->
            match (result, artifact_format path value) with
            | Ok formats, Ok format -> Ok (format :: formats)
            | Error left, Error right -> Error (left @ right)
            | Error errors, Ok _ | Ok _, Error errors -> Error errors)
          (Ok []) values
        |> map List.rev
      in
      match decoded with
      | Ok [] -> invalid path "at least one report format is required"
      | Ok formats ->
          let names =
            List.map (fun format -> artifact_format_name format) formats
          in
          if
            List.length names
            = List.length (List.sort_uniq String.compare names)
          then valid formats
          else invalid path "report formats must be unique"
      | Error _ as error -> error)
  | value ->
      invalid path
        (Printf.sprintf "expected string array, got %s" (type_name value))

let source_embedding path value =
  match string path value with
  | Error _ as error -> error
  | Ok "context" -> valid Context
  | Ok "all" -> valid All_source
  | Ok "none" -> valid No_source
  | Ok value ->
      invalid path
        (Printf.sprintf "expected \"context\", \"all\", or \"none\", got %S"
           value)

let profile path value =
  match string path value with
  | Error _ as error -> error
  | Ok value -> (
      match Core.Operator.Profile.of_string value with
      | Ok profile -> valid profile
      | Error message -> invalid path message)

let known_keys version =
  if version = 1 then
    [
      ([], [ "version"; "mutation"; "test"; "execution"; "cache" ]);
      ( [ "mutation" ],
        [ "include"; "exclude"; "operators"; "profile"; "expect" ] );
      ( [ "test" ],
        [ "command"; "stages"; "timeout"; "baseline_runs"; "parallel_safe" ] );
      ([ "execution" ], [ "jobs" ]);
      ([ "cache" ], [ "mode"; "directory" ]);
    ]
  else
    [
      ( [],
        [
          "version";
          "mutation";
          "test";
          "execution";
          "cache";
          "policy";
          "report";
          "privacy";
        ] );
      ( [ "mutation" ],
        [ "include"; "exclude"; "operators"; "profile"; "expect" ] );
      ( [ "test" ],
        [
          "driver";
          "command";
          "stages";
          "timeout";
          "baseline_runs";
          "parallel_safe";
          "external_inputs";
          "reproducible";
        ] );
      ([ "execution" ], [ "mode"; "jobs" ]);
      ([ "cache" ], [ "mode"; "directory"; "historical_reuse" ]);
      ( [ "policy" ],
        [
          "require_complete";
          "max_unexpected_survivors";
          "minimum_score";
          "maximum_score_drop";
          "allow_estimated";
        ] );
      ([ "report" ], [ "formats"; "directory" ]);
      ( [ "privacy" ],
        [
          "stdout_limit_bytes";
          "stderr_limit_bytes";
          "redactions";
          "source_embedding";
        ] );
    ]

let unknown_keys ~version root =
  let errors = ref [] in
  List.iter
    (fun (path, allowed) ->
      match lookup root path with
      | None -> ()
      | Some (Otoml.TomlTable entries) ->
          List.iter
            (fun (key, _) ->
              if not (List.mem key allowed) then
                errors :=
                  {
                    path = path @ [ key ];
                    message = "unknown configuration key";
                  }
                  :: !errors)
            entries
      | Some value when path <> [] ->
          errors :=
            {
              path;
              message =
                Printf.sprintf "expected table, got %s" (type_name value);
            }
            :: !errors
      | Some _ -> ())
    (known_keys version);
  List.rev !errors

let decode_version root =
  match lookup root [ "version" ] with
  | None -> invalid [ "version" ] "missing required key (expected version = 2)"
  | Some value -> (
      match integer [ "version" ] value with
      | Ok ((1 | 2) as version) -> valid version
      | Ok value ->
          invalid [ "version" ]
            (Printf.sprintf "unsupported config version %d" value)
      | Error _ as error -> error)

let decode root =
  match decode_version root with
  | Error _ as error -> error
  | Ok version ->
      let version_check =
        match lookup root [ "version" ] with
        | Some _ -> valid ()
        | None -> assert false
      in
      let+ () = version_check
      and+ include_ = optional root [ "mutation"; "include" ] strings
      and+ exclude = optional root [ "mutation"; "exclude" ] strings
      and+ operators = optional root [ "mutation"; "operators" ] operators
      and+ profile = optional root [ "mutation"; "profile" ] profile
      and+ expectations = optional root [ "mutation"; "expect" ] expectations
      and+ command = optional root [ "test"; "command" ] command
      and+ stages = optional root [ "test"; "stages" ] stages
      and+ () =
        match
          (lookup root [ "test"; "command" ], lookup root [ "test"; "stages" ])
        with
        | Some _, Some _ ->
            invalid [ "test" ]
              "test.command and test.stages cannot be used together"
        | _ -> valid ()
      and+ driver = optional root [ "test"; "driver" ] test_driver
      and+ timeout = optional root [ "test"; "timeout" ] timeout
      and+ baseline_runs =
        optional root [ "test"; "baseline_runs" ] positive_int
      and+ parallel_safe = optional root [ "test"; "parallel_safe" ] boolean
      and+ external_inputs =
        optional root [ "test"; "external_inputs" ] nonempty_strings
      and+ reproducible = optional root [ "test"; "reproducible" ] boolean
      and+ execution_mode = optional root [ "execution"; "mode" ] execution_mode
      and+ jobs = optional root [ "execution"; "jobs" ] jobs
      and+ mode = optional root [ "cache"; "mode" ] cache_mode
      and+ historical_reuse =
        optional root [ "cache"; "historical_reuse" ] historical_reuse
      and+ directory = optional root [ "cache"; "directory" ] string
      and+ require_complete =
        optional root [ "policy"; "require_complete" ] boolean
      and+ max_unexpected_survivors =
        optional root [ "policy"; "max_unexpected_survivors" ] nonnegative_int
      and+ minimum_score =
        optional root [ "policy"; "minimum_score" ] percentage
      and+ maximum_score_drop =
        optional root [ "policy"; "maximum_score_drop" ] percentage
      and+ allow_estimated =
        optional root [ "policy"; "allow_estimated" ] boolean
      and+ formats = optional root [ "report"; "formats" ] artifact_formats
      and+ report_directory = optional root [ "report"; "directory" ] string
      and+ stdout_limit_bytes =
        optional root [ "privacy"; "stdout_limit_bytes" ] nonnegative_int
      and+ stderr_limit_bytes =
        optional root [ "privacy"; "stderr_limit_bytes" ] nonnegative_int
      and+ redactions =
        optional root [ "privacy"; "redactions" ] nonempty_strings
      and+ source_embedding =
        optional root [ "privacy"; "source_embedding" ] source_embedding
      in
      let historical_reuse =
        match (version, historical_reuse, mode) with
        | 1, _, Some On -> Reuse_exact
        | 1, _, Some (Auto | Off) | 1, _, None -> Reuse_off
        | 2, Some value, _ -> value
        | 2, None, _ -> defaults.cache.historical_reuse
        | _ -> assert false
      in
      {
        version = 2;
        mutation =
          {
            include_ = Option.value include_ ~default:defaults.mutation.include_;
            exclude = Option.value exclude ~default:defaults.mutation.exclude;
            operators =
              Option.value operators ~default:defaults.mutation.operators;
            profile = Option.value profile ~default:defaults.mutation.profile;
            expectations =
              Option.value expectations ~default:defaults.mutation.expectations;
          };
        test =
          {
            driver =
              (match (version, driver, command, stages) with
              | _, Some driver, _, _ -> driver
              | 1, None, _, Some _
              | 1, None, Some _, None ->
                  (* v1 had no driver abstraction: an explicitly configured
                     argv or ordered stage list was always executed verbatim.
                     Preserve that behavior while normalizing the in-memory
                     representation to v2, so migration cannot silently turn
                     [dune build] into the inventory-driven [@runtest] plan. *)
                  Command_driver
              | 1, None, None, None | 2, None, _, _ -> defaults.test.driver
              | _ -> assert false);
            command =
              (match (command, stages) with
              | Some command, None -> command
              | None, Some ({ command; _ } :: _) -> command
              | None, Some [] | None, None -> defaults.test.command
              | Some _, Some _ -> assert false);
            stages =
              (match (command, stages) with
              | Some command, None -> [ { name = "test"; command } ]
              | None, Some stages -> stages
              | None, None -> defaults.test.stages
              | Some _, Some _ -> assert false);
            timeout =
              (match timeout with
              | None -> defaults.test.timeout
              | Some value -> value);
            baseline_runs =
              Option.value baseline_runs ~default:defaults.test.baseline_runs;
            parallel_safe =
              Option.value parallel_safe ~default:defaults.test.parallel_safe;
            external_inputs =
              Option.value external_inputs
                ~default:defaults.test.external_inputs;
            reproducible =
              Option.value reproducible ~default:defaults.test.reproducible;
          };
        execution =
          {
            mode = Option.value execution_mode ~default:defaults.execution.mode;
            jobs = Option.value jobs ~default:defaults.execution.jobs;
          };
        cache =
          {
            mode =
              (match (version, mode, historical_reuse) with
              | 1, Some value, _ -> value
              | 1, None, _ -> Auto
              | 2, Some value, _ -> value
              | 2, None, Reuse_off -> Off
              | 2, None, (Reuse_exact | Reuse_estimated) -> On
              | _ -> assert false);
            directory =
              (match directory with
              | None -> defaults.cache.directory
              | Some value -> Some value);
            historical_reuse;
          };
        policy =
          {
            require_complete =
              Option.value require_complete
                ~default:defaults.policy.require_complete;
            max_unexpected_survivors =
              Option.value max_unexpected_survivors
                ~default:defaults.policy.max_unexpected_survivors;
            minimum_score;
            maximum_score_drop;
            allow_estimated =
              Option.value allow_estimated
                ~default:defaults.policy.allow_estimated;
          };
        report =
          {
            formats = Option.value formats ~default:defaults.report.formats;
            directory = report_directory;
          };
        privacy =
          {
            stdout_limit_bytes =
              Option.value stdout_limit_bytes
                ~default:defaults.privacy.stdout_limit_bytes;
            stderr_limit_bytes =
              Option.value stderr_limit_bytes
                ~default:defaults.privacy.stderr_limit_bytes;
            redactions =
              Option.value redactions ~default:defaults.privacy.redactions;
            source_embedding =
              Option.value source_embedding
                ~default:defaults.privacy.source_embedding;
          };
      }

let format_diagnostics ~file ~locations diagnostics =
  diagnostics
  |> List.map (fun diagnostic ->
      let location = Location_index.find locations diagnostic.path in
      Printf.sprintf "%s:%d:%d: %s: %s" file location.line location.column
        (String.concat "." diagnostic.path)
        diagnostic.message)
  |> String.concat "\n"

type origin = Defaults | Version_1 | Version_2
type loaded = { config : t; origin : origin; warnings : string list }

let parse_with_metadata ~file contents =
  let locations = Location_index.create contents in
  try
    let root = Otoml.Parser.from_string contents in
    match decode_version root with
    | Error errors -> Error (format_diagnostics ~file ~locations errors)
    | Ok version -> (
        let unknown = unknown_keys ~version root in
        match decode root with
        | Ok config when unknown = [] ->
            let origin, warnings =
              if version = 1 then
                ( Version_1,
                  [
                    Printf.sprintf
                      "%s uses deprecated config version 1; run `ocaml-mutants \
                       config migrate --write`"
                      file;
                  ] )
              else (Version_2, [])
            in
            Ok { config; origin; warnings }
        | Ok _ -> Error (format_diagnostics ~file ~locations unknown)
        | Error errors ->
            Error (format_diagnostics ~file ~locations (unknown @ errors)))
  with
  | Otoml.Parse_error (location, message) ->
      let line, column = Option.value location ~default:(1, 1) in
      Error (Printf.sprintf "%s:%d:%d: %s" file line column message)
  | Otoml.Duplicate_key key ->
      Error (Printf.sprintf "%s:1:1: duplicate key %s" file key)
  | Otoml.Type_error message | Otoml.Key_error message ->
      Error (Printf.sprintf "%s:1:1: %s" file message)

let parse ~file contents =
  Result.map (fun loaded -> loaded.config) (parse_with_metadata ~file contents)

let load_with_metadata root =
  let path = Filename.concat root ".ocaml-mutants.toml" in
  if not (Sys.file_exists path) then
    Ok { config = defaults; origin = Defaults; warnings = [] }
  else
    match Util.read_file path with
    | Error message -> Error (Printf.sprintf "%s: %s" path message)
    | Ok contents -> parse_with_metadata ~file:path contents

let load root =
  Result.map (fun loaded -> loaded.config) (load_with_metadata root)

let apply config overrides =
  {
    config with
    version = 2;
    mutation =
      {
        include_ =
          Option.value overrides.include_ ~default:config.mutation.include_;
        exclude =
          Option.value overrides.exclude ~default:config.mutation.exclude;
        operators =
          Option.value overrides.operators ~default:config.mutation.operators;
        profile =
          Option.value overrides.profile ~default:config.mutation.profile;
        expectations = config.mutation.expectations;
      };
    test =
      {
        config.test with
        command = Option.value overrides.command ~default:config.test.command;
        stages =
          (match overrides.command with
          | Some command -> [ { name = "test"; command } ]
          | None -> config.test.stages);
        timeout = Option.value overrides.timeout ~default:config.test.timeout;
      };
    execution =
      {
        config.execution with
        jobs = Option.value overrides.jobs ~default:config.execution.jobs;
      };
    cache =
      {
        config.cache with
        mode = Option.value overrides.cache_mode ~default:config.cache.mode;
        historical_reuse =
          (match overrides.cache_mode with
          | Some Off -> Reuse_off
          | Some On -> Reuse_exact
          | Some Auto | None -> config.cache.historical_reuse);
      };
  }

let cache_enabled mode ~command =
  match mode with
  | Off -> false
  | On -> true
  | Auto ->
      ignore command;
      false

let historical_reuse_enabled = function
  | Reuse_off -> false
  | Reuse_exact | Reuse_estimated -> true

let historical_reuse_name = function
  | Reuse_off -> "off"
  | Reuse_exact -> "exact"
  | Reuse_estimated -> "estimated"

let execution_mode_name = function Strict -> "strict" | Fast -> "fast"
let cache_mode_name = function Auto -> "auto" | On -> "on" | Off -> "off"

let driver_name = function
  | Auto_driver -> "auto"
  | Dune_driver -> "dune"
  | Command_driver -> "command"

let source_embedding_name = function
  | Context -> "context"
  | All_source -> "all"
  | No_source -> "none"

let json_strings values = `List (List.map (fun value -> `String value) values)
let json_command command = Core.Nonempty_argv.to_list command |> json_strings
let json_option_string = function None -> `Null | Some value -> `String value
let json_option_float = function None -> `Null | Some value -> `Float value

let to_yojson config =
  `Assoc
    [
      ("version", `Int 2);
      ( "mutation",
        `Assoc
          [
            ( "profile",
              `String (Core.Operator.Profile.name config.mutation.profile) );
            ("include", json_strings config.mutation.include_);
            ("exclude", json_strings config.mutation.exclude);
            ( "operators",
              config.mutation.operators
              |> List.map Core.Operator.Family.name
              |> json_strings );
            ( "expectations",
              `List
                (List.map
                   (fun expectation ->
                     `Assoc
                       [
                         ("id", `String expectation.id);
                         ("reason", `String expectation.reason);
                       ])
                   config.mutation.expectations) );
          ] );
      ( "test",
        `Assoc
          [
            ("driver", `String (driver_name config.test.driver));
            ("command", json_command config.test.command);
            ( "stages",
              `List
                (List.map
                   (fun stage ->
                     `Assoc
                       [
                         ("name", `String stage.name);
                         ("command", json_command stage.command);
                       ])
                   config.test.stages) );
            ( "timeout_seconds",
              match config.test.timeout with
              | None -> `Null
              | Some value -> `Float (Core.Duration.to_seconds value) );
            ( "baseline_runs",
              `Int (Core.Positive_int.to_int config.test.baseline_runs) );
            ("parallel_safe", `Bool config.test.parallel_safe);
            ("external_inputs", json_strings config.test.external_inputs);
            ("reproducible", `Bool config.test.reproducible);
          ] );
      ( "execution",
        `Assoc
          [
            ("mode", `String (execution_mode_name config.execution.mode));
            ("jobs", `Int (Core.Positive_int.to_int config.execution.jobs));
          ] );
      ( "cache",
        `Assoc
          [
            ("mode", `String (cache_mode_name config.cache.mode));
            ("directory", json_option_string config.cache.directory);
            ( "historical_reuse",
              `String (historical_reuse_name config.cache.historical_reuse) );
          ] );
      ( "policy",
        `Assoc
          [
            ("require_complete", `Bool config.policy.require_complete);
            ( "max_unexpected_survivors",
              `Int config.policy.max_unexpected_survivors );
            ("minimum_score", json_option_float config.policy.minimum_score);
            ( "maximum_score_drop",
              json_option_float config.policy.maximum_score_drop );
            ("allow_estimated", `Bool config.policy.allow_estimated);
          ] );
      ( "report",
        `Assoc
          [
            ( "formats",
              config.report.formats
              |> List.map artifact_format_name
              |> json_strings );
            ("directory", json_option_string config.report.directory);
          ] );
      ( "privacy",
        `Assoc
          [
            ("stdout_limit_bytes", `Int config.privacy.stdout_limit_bytes);
            ("stderr_limit_bytes", `Int config.privacy.stderr_limit_bytes);
            ("redactions", json_strings config.privacy.redactions);
            ( "source_embedding",
              `String (source_embedding_name config.privacy.source_embedding) );
          ] );
    ]

let report_redaction_placeholder = "<redacted>"

let to_report_yojson config =
  to_yojson
    {
      config with
      privacy =
        {
          config.privacy with
          redactions =
            List.map
              (Fun.const report_redaction_placeholder)
              config.privacy.redactions;
        };
    }

let toml_string value = Printf.sprintf "%S" value

let toml_strings values =
  "[" ^ String.concat ", " (List.map toml_string values) ^ "]"

let command_toml command = Core.Nonempty_argv.to_list command |> toml_strings

let option_float value =
  match value with None -> None | Some value -> Some (string_of_float value)

let to_toml config =
  let buffer = Buffer.create 2048 in
  let line format =
    Printf.ksprintf
      (fun value -> Buffer.add_string buffer (value ^ "\n"))
      format
  in
  line "# ocaml-mutants configuration v2";
  line "version = 2";
  line "";
  line "[mutation]";
  line "profile = %s"
    (toml_string (Core.Operator.Profile.name config.mutation.profile));
  line "include = %s" (toml_strings config.mutation.include_);
  line "exclude = %s" (toml_strings config.mutation.exclude);
  line "operators = %s"
    (config.mutation.operators
    |> List.map Core.Operator.Family.name
    |> toml_strings);
  List.iter
    (fun expectation ->
      line "";
      line "[[mutation.expect]]";
      line "id = %s" (toml_string expectation.id);
      line "reason = %s" (toml_string expectation.reason))
    config.mutation.expectations;
  line "";
  line "[test]";
  line "driver = %s" (toml_string (driver_name config.test.driver));
  (match config.test.stages with
  | [ stage ] -> line "command = %s" (command_toml stage.command)
  | _ -> ());
  line "timeout = %s"
    (match config.test.timeout with
    | None -> toml_string "auto"
    | Some duration -> string_of_float (Core.Duration.to_seconds duration));
  line "baseline_runs = %d" (Core.Positive_int.to_int config.test.baseline_runs);
  line "parallel_safe = %b" config.test.parallel_safe;
  line "external_inputs = %s" (toml_strings config.test.external_inputs);
  line "reproducible = %b" config.test.reproducible;
  (match config.test.stages with
  | [] | [ _ ] -> ()
  | stages ->
      List.iter
        (fun stage ->
          line "";
          line "[[test.stages]]";
          line "name = %s" (toml_string stage.name);
          line "command = %s" (command_toml stage.command))
        stages);
  line "";
  line "[execution]";
  line "mode = %s" (toml_string (execution_mode_name config.execution.mode));
  line "jobs = %d" (Core.Positive_int.to_int config.execution.jobs);
  line "";
  line "[cache]";
  line "mode = %s" (toml_string (cache_mode_name config.cache.mode));
  line "historical_reuse = %s"
    (toml_string (historical_reuse_name config.cache.historical_reuse));
  Option.iter
    (fun directory -> line "directory = %s" (toml_string directory))
    config.cache.directory;
  line "";
  line "[policy]";
  line "require_complete = %b" config.policy.require_complete;
  line "max_unexpected_survivors = %d" config.policy.max_unexpected_survivors;
  Option.iter
    (fun value -> line "minimum_score = %s" value)
    (option_float config.policy.minimum_score);
  Option.iter
    (fun value -> line "maximum_score_drop = %s" value)
    (option_float config.policy.maximum_score_drop);
  line "allow_estimated = %b" config.policy.allow_estimated;
  line "";
  line "[report]";
  line "formats = %s"
    (config.report.formats |> List.map artifact_format_name |> toml_strings);
  Option.iter
    (fun directory -> line "directory = %s" (toml_string directory))
    config.report.directory;
  line "";
  line "[privacy]";
  line "stdout_limit_bytes = %d" config.privacy.stdout_limit_bytes;
  line "stderr_limit_bytes = %d" config.privacy.stderr_limit_bytes;
  line "redactions = %s" (toml_strings config.privacy.redactions);
  line "source_embedding = %s"
    (toml_string (source_embedding_name config.privacy.source_embedding));
  Buffer.contents buffer

let example =
  {|# ocaml-mutants configuration v2
version = 2

[mutation]
profile = "balanced"
include = ["**/*.ml"]
exclude = ["**/test/**", "**/tests/**", "**/_build/**"]
# Every operator family is enabled while `operators` stays omitted.
# Uncomment to narrow the catalog:
# operators = [
#   "boolean-literal", "condition-negation", "boolean-connective",
#   "comparison", "integer-arithmetic", "float-arithmetic",
#   "if-branch", "sequence-deletion", "return-replacement",
# ]

[test]
driver = "auto"
command = ["dune", "runtest", "--force"]
timeout = "auto"
baseline_runs = 3
parallel_safe = false
external_inputs = []
reproducible = true

[execution]
mode = "strict"
jobs = "auto"

[cache]
historical_reuse = "off"
# Relative paths resolve below the OS cache root, never the workspace.
# directory = "team-cache"

[policy]
require_complete = true
max_unexpected_survivors = 0
allow_estimated = false

[report]
formats = ["terminal", "json"]

[privacy]
stdout_limit_bytes = 65536
stderr_limit_bytes = 65536
redactions = []
source_embedding = "context"
|}
