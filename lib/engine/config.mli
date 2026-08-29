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

val defaults : t
val empty_overrides : overrides

type origin = Defaults | Version_1 | Version_2
type loaded = { config : t; origin : origin; warnings : string list }

val load : string -> (t, string) result
val load_with_metadata : string -> (loaded, string) result
val parse : file:string -> string -> (t, string) result
val parse_with_metadata : file:string -> string -> (loaded, string) result
val apply : t -> overrides -> t
val cache_enabled : cache_mode -> command:Core.Nonempty_argv.t -> bool
val historical_reuse_enabled : historical_reuse -> bool
val execution_mode_name : execution_mode -> string
val historical_reuse_name : historical_reuse -> string
val artifact_format_name : artifact_format -> string
val to_yojson : t -> Yojson.Safe.t
val report_redaction_placeholder : string
val to_report_yojson : t -> Yojson.Safe.t
val to_toml : t -> string
val example : string
