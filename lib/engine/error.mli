type phase =
  | Cli
  | Snapshot
  | Dune
  | Analysis
  | Baseline_proof
  | Instrumentation
  | Ready_proof
  | Execution
  | Cache
  | Reporting
  | Cleanup

type cause =
  | Invalid_input
  | Workspace_violation
  | Decode_failure
  | Process_failure
  | Baseline_failure
  | Invariant_violation
  | Resource_busy
  | Io_failure
  | Corrupt_cache
  | Interrupted_by_user

type kind = Usage | Baseline | Tool | Process | Interrupted
type t

val create :
  phase:phase ->
  cause:cause ->
  ?context:(string * string) list ->
  ('a, Format.formatter, unit, t) format4 ->
  'a

val make : kind -> ('a, Format.formatter, unit, t) format4 -> 'a
val with_context : string -> string -> t -> t
val suppress : t -> t -> t

val restore :
  phase:phase ->
  cause:cause ->
  message:string ->
  context:(string * string) list ->
  suppressed:t list ->
  t

val phase : t -> phase
val cause : t -> cause
val message : t -> string
val context : t -> (string * string) list
val suppressed : t -> t list
val phase_name : phase -> string
val cause_name : cause -> string
val phase_of_name : string -> (phase, string) result
val cause_of_name : string -> (cause, string) result
val exit_code : t -> int
val pp : Format.formatter -> t -> unit
