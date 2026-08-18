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

val defaults : t
val empty_overrides : overrides
val load : string -> (t, string) result
val parse : file:string -> string -> (t, string) result
val apply : t -> overrides -> t
val cache_enabled : cache_mode -> command:Core.Nonempty_argv.t -> bool
val example : string
