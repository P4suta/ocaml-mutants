module Core = Ocaml_mutants_core

type cache_mode = Auto | On | Off
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
  command : Core.Nonempty_argv.t;
  stages : stage list;
  timeout : Core.Duration.t option;
  baseline_runs : Core.Positive_int.t;
  parallel_safe : bool;
}

type execution = { jobs : Core.Positive_int.t }
type cache = { mode : cache_mode; directory : string option }

type t = {
  mutation : mutation;
  test : test;
  execution : execution;
  cache : cache;
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
        command = default_command;
        stages = [ { name = "full"; command = default_command } ];
        timeout = None;
        baseline_runs = get_ok (Core.Positive_int.of_int 3);
        parallel_safe = false;
      };
    execution =
      {
        jobs =
          get_ok
            (Core.Positive_int.of_int
               (max 1 (Domain.recommended_domain_count () - 1)));
      };
    cache = { mode = Auto; directory = None };
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

let positive_int path value =
  match integer path value with
  | Error _ as error -> error
  | Ok value -> (
      match Core.Positive_int.of_int value with
      | Ok value -> valid value
      | Error message -> invalid path message)

let duration path value =
  match number path value with
  | Error _ as error -> error
  | Ok value -> (
      match Core.Duration.of_seconds value with
      | Ok value when Core.Duration.to_seconds value > 0. -> valid value
      | Ok _ -> invalid path "duration must be positive"
      | Error message -> invalid path message)

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

let profile path value =
  match string path value with
  | Error _ as error -> error
  | Ok value -> (
      match Core.Operator.Profile.of_string value with
      | Ok profile -> valid profile
      | Error message -> invalid path message)

let known_keys =
  [
    ([], [ "version"; "mutation"; "test"; "execution"; "cache" ]);
    ([ "mutation" ], [ "include"; "exclude"; "operators"; "profile"; "expect" ]);
    ( [ "test" ],
      [ "command"; "stages"; "timeout"; "baseline_runs"; "parallel_safe" ] );
    ([ "execution" ], [ "jobs" ]);
    ([ "cache" ], [ "mode"; "directory" ]);
  ]

let unknown_keys root =
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
    known_keys;
  List.rev !errors

let decode root =
  let version =
    match lookup root [ "version" ] with
    | None ->
        invalid [ "version" ] "missing required key (expected version = 1)"
    | Some value -> (
        match integer [ "version" ] value with
        | Ok 1 -> valid ()
        | Ok value ->
            invalid [ "version" ]
              (Printf.sprintf "unsupported config version %d" value)
        | Error _ as error -> error)
  in
  let+ () = version
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
  and+ timeout = optional root [ "test"; "timeout" ] duration
  and+ baseline_runs = optional root [ "test"; "baseline_runs" ] positive_int
  and+ parallel_safe = optional root [ "test"; "parallel_safe" ] boolean
  and+ jobs = optional root [ "execution"; "jobs" ] positive_int
  and+ mode = optional root [ "cache"; "mode" ] cache_mode
  and+ directory = optional root [ "cache"; "directory" ] string in
  {
    mutation =
      {
        include_ = Option.value include_ ~default:defaults.mutation.include_;
        exclude = Option.value exclude ~default:defaults.mutation.exclude;
        operators = Option.value operators ~default:defaults.mutation.operators;
        profile = Option.value profile ~default:defaults.mutation.profile;
        expectations =
          Option.value expectations ~default:defaults.mutation.expectations;
      };
    test =
      {
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
          | Some value -> Some value);
        baseline_runs =
          Option.value baseline_runs ~default:defaults.test.baseline_runs;
        parallel_safe =
          Option.value parallel_safe ~default:defaults.test.parallel_safe;
      };
    execution = { jobs = Option.value jobs ~default:defaults.execution.jobs };
    cache =
      {
        mode = Option.value mode ~default:defaults.cache.mode;
        directory =
          (match directory with
          | None -> defaults.cache.directory
          | Some value -> Some value);
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

let parse ~file contents =
  let locations = Location_index.create contents in
  try
    let root = Otoml.Parser.from_string contents in
    let unknown = unknown_keys root in
    match decode root with
    | Ok config when unknown = [] -> Ok config
    | Ok _ -> Error (format_diagnostics ~file ~locations unknown)
    | Error errors ->
        Error (format_diagnostics ~file ~locations (unknown @ errors))
  with
  | Otoml.Parse_error (location, message) ->
      let line, column = Option.value location ~default:(1, 1) in
      Error (Printf.sprintf "%s:%d:%d: %s" file line column message)
  | Otoml.Duplicate_key key ->
      Error (Printf.sprintf "%s:1:1: duplicate key %s" file key)
  | Otoml.Type_error message | Otoml.Key_error message ->
      Error (Printf.sprintf "%s:1:1: %s" file message)

let load root =
  let path = Filename.concat root ".ocaml-mutants.toml" in
  if not (Sys.file_exists path) then Ok defaults
  else
    match Util.read_file path with
    | Error message -> Error (Printf.sprintf "%s: %s" path message)
    | Ok contents -> parse ~file:path contents

let apply config overrides =
  {
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
        command = Option.value overrides.command ~default:config.test.command;
        stages =
          (match overrides.command with
          | Some command -> [ { name = "test"; command } ]
          | None -> config.test.stages);
        timeout = Option.value overrides.timeout ~default:config.test.timeout;
        baseline_runs = config.test.baseline_runs;
        parallel_safe = config.test.parallel_safe;
      };
    execution =
      { jobs = Option.value overrides.jobs ~default:config.execution.jobs };
    cache =
      {
        config.cache with
        mode = Option.value overrides.cache_mode ~default:config.cache.mode;
      };
  }

let cache_enabled mode ~command =
  match mode with
  | Off -> false
  | On -> true
  | Auto ->
      ignore command;
      false

let example =
  {|# ocaml-mutants configuration v1
version = 1

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
command = ["dune", "runtest", "--force"]
# timeout = 30.0
baseline_runs = 3
parallel_safe = false

[execution]
jobs = 1

[cache]
mode = "auto"
# Relative paths resolve below the OS cache root, never the workspace.
# directory = "team-cache"
|}
