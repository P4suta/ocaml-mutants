type target_kind = Library | Executable | Unknown

type target = {
  kind : target_kind;
  name : string option;
  source_files : string list;
}

type workspace = {
  source_files : string list;
  cmt_targets : string list;
  targets : target list;
}

type described_test = { name : string; source_dir : string; target : string }
type source_role = Production | Test | Tool | Generated

module type PROCESS = sig
  type result

  val run :
    cancel:Cancel.t ->
    cwd:string ->
    env:(string * string option) list ->
    string list ->
    result

  val cancelled : result -> bool
  val succeeded : result -> bool
  val stdout : result -> string
  val stderr : result -> string
  val status : result -> string
end

module Make (Process : PROCESS) : sig
  val describe : cancel:Cancel.t -> root:string -> (workspace, Error.t) result

  val describe_tests :
    cancel:Cancel.t -> root:string -> (described_test list, Error.t) result

  val build_analysis :
    cancel:Cancel.t ->
    root:string ->
    build_dir:string ->
    cmt_targets:string list ->
    (Process.result, Error.t) result
end

module System : sig
  val describe : cancel:Cancel.t -> root:string -> (workspace, Error.t) result

  val describe_tests :
    cancel:Cancel.t -> root:string -> (described_test list, Error.t) result

  val build_analysis :
    cancel:Cancel.t ->
    root:string ->
    build_dir:string ->
    cmt_targets:string list ->
    (Process_supervisor.result, Error.t) result
end

val describe : cancel:Cancel.t -> root:string -> (workspace, Error.t) result
val parse : root:string -> string -> (workspace, string) result
val parse_tests : string -> (described_test list, string) result

val describe_tests :
  cancel:Cancel.t -> root:string -> (described_test list, Error.t) result

val source_role :
  workspace:workspace -> tests:described_test list -> string -> source_role

val build_analysis :
  cancel:Cancel.t ->
  root:string ->
  build_dir:string ->
  cmt_targets:string list ->
  (Process_supervisor.result, Error.t) result

val cmt_files : root:string -> build_dir:string -> string list
