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

type t = {
  phase : phase;
  cause : cause;
  message : string;
  context : (string * string) list;
  suppressed : t list;
}

let create ~phase ~cause ?(context = []) format =
  Format.kasprintf
    (fun message -> { phase; cause; message; context; suppressed = [] })
    format

let make kind format =
  let phase, cause =
    match kind with
    | Usage -> (Cli, Invalid_input)
    | Baseline -> (Baseline_proof, Baseline_failure)
    | Tool -> (Analysis, Io_failure)
    | Process -> (Execution, Process_failure)
    | Interrupted -> (Execution, Interrupted_by_user)
  in
  create ~phase ~cause format

let with_context key value error =
  { error with context = error.context @ [ (key, value) ] }

let suppress primary cleanup =
  { primary with suppressed = primary.suppressed @ [ cleanup ] }

let restore ~phase ~cause ~message ~context ~suppressed =
  { phase; cause; message; context; suppressed }

let phase error = error.phase
let cause error = error.cause
let message error = error.message
let context error = error.context
let suppressed error = error.suppressed

let phase_name = function
  | Cli -> "cli"
  | Snapshot -> "snapshot"
  | Dune -> "dune"
  | Analysis -> "analysis"
  | Baseline_proof -> "baseline-proof"
  | Instrumentation -> "instrumentation"
  | Ready_proof -> "ready-proof"
  | Execution -> "execution"
  | Cache -> "cache"
  | Reporting -> "reporting"
  | Cleanup -> "cleanup"

let cause_name = function
  | Invalid_input -> "invalid-input"
  | Workspace_violation -> "workspace-violation"
  | Decode_failure -> "decode-failure"
  | Process_failure -> "process-failure"
  | Baseline_failure -> "baseline-failure"
  | Invariant_violation -> "invariant-violation"
  | Resource_busy -> "resource-busy"
  | Io_failure -> "io-failure"
  | Corrupt_cache -> "corrupt-cache"
  | Interrupted_by_user -> "interrupted"

let phase_of_name = function
  | "cli" -> Ok Cli
  | "snapshot" -> Ok Snapshot
  | "dune" -> Ok Dune
  | "analysis" -> Ok Analysis
  | "baseline-proof" -> Ok Baseline_proof
  | "instrumentation" -> Ok Instrumentation
  | "ready-proof" -> Ok Ready_proof
  | "execution" -> Ok Execution
  | "cache" -> Ok Cache
  | "reporting" -> Ok Reporting
  | "cleanup" -> Ok Cleanup
  | value -> Error (Printf.sprintf "unknown failure phase %S" value)

let cause_of_name = function
  | "invalid-input" -> Ok Invalid_input
  | "workspace-violation" -> Ok Workspace_violation
  | "decode-failure" -> Ok Decode_failure
  | "process-failure" -> Ok Process_failure
  | "baseline-failure" -> Ok Baseline_failure
  | "invariant-violation" -> Ok Invariant_violation
  | "resource-busy" -> Ok Resource_busy
  | "io-failure" -> Ok Io_failure
  | "corrupt-cache" -> Ok Corrupt_cache
  | "interrupted" -> Ok Interrupted_by_user
  | value -> Error (Printf.sprintf "unknown failure cause %S" value)

let exit_code error =
  match error.cause with Interrupted_by_user -> 130 | _ -> 2

let rec pp formatter error =
  Format.fprintf formatter "[%s/%s] %s" (phase_name error.phase)
    (cause_name error.cause) error.message;
  List.iter
    (fun (key, value) -> Format.fprintf formatter "@,  %s: %s" key value)
    error.context;
  List.iter
    (fun suppressed ->
      Format.fprintf formatter "@,  suppressed cleanup error: %a" pp suppressed)
    error.suppressed
