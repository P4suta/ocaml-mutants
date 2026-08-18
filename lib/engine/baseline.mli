module Core = Ocaml_mutants_core

type process_disposition = Succeeded | Failed | Cancelled

module type PROCESS = sig
  type result

  val run :
    cancel:Cancel.t ->
    cwd:string ->
    env:(string * string option) list ->
    string list ->
    result

  val disposition : result -> process_disposition
  val duration_seconds : result -> float
  val stdout : result -> string
  val stderr : result -> string
  val status : result -> string
end

type evidence
type stop_kind = Command_failure | Cancellation

type incomplete = private {
  evidence : evidence;
  error : Error.t;
  kind : stop_kind;
}

type complete = private { evidence : evidence; slowest : Core.Duration.t }
type outcome = Completed of complete | Incomplete of incomplete

val completed_stages : evidence -> Run_store.baseline_stage list
val partial_stage : evidence -> Run_store.baseline_stage option

val stages : evidence -> Run_store.baseline_stage list
(** All measured stages in execution order. A partially measured final stage is
    included only when at least one repetition completed successfully. *)

val slowest : evidence -> Core.Duration.t option
(** The slowest successful repetition across [stages], or [None] when no
    repetition completed. *)

val was_interrupted : incomplete -> bool

module Make (Process : PROCESS) : sig
  val run :
    cancel:Cancel.t ->
    root:string ->
    config:Config.t ->
    build_dir:string ->
    outcome
end

val run :
  cancel:Cancel.t ->
  root:string ->
  config:Config.t ->
  build_dir:string ->
  outcome
