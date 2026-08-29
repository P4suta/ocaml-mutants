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
  code : string;
  remediation : string;
  message : string;
  context : (string * string) list;
  suppressed : t list;
}

let phase_code = function
  | Cli -> "CLI"
  | Snapshot -> "SNAPSHOT"
  | Dune -> "DUNE"
  | Analysis -> "ANALYSIS"
  | Baseline_proof -> "BASELINE"
  | Instrumentation -> "INSTRUMENTATION"
  | Ready_proof -> "READINESS"
  | Execution -> "EXECUTION"
  | Cache -> "CACHE"
  | Reporting -> "REPORTING"
  | Cleanup -> "CLEANUP"

let cause_code = function
  | Invalid_input -> "INVALID_INPUT"
  | Workspace_violation -> "WORKSPACE_VIOLATION"
  | Decode_failure -> "DECODE_FAILURE"
  | Process_failure -> "PROCESS_FAILURE"
  | Baseline_failure -> "BASELINE_FAILURE"
  | Invariant_violation -> "INVARIANT_VIOLATION"
  | Resource_busy -> "RESOURCE_BUSY"
  | Io_failure -> "IO_FAILURE"
  | Corrupt_cache -> "CORRUPT_CACHE"
  | Interrupted_by_user -> "INTERRUPTED"

let default_code phase cause =
  Printf.sprintf "OM_%s_%s" (phase_code phase) (cause_code cause)

let default_remediation = function
  | Invalid_input ->
      "Run the command with --help and correct the invalid input."
  | Workspace_violation ->
      "Restore the workspace path or permissions, then retry without changing \
       the snapshot."
  | Decode_failure ->
      "Validate the referenced config or report against its versioned schema."
  | Process_failure ->
      "Re-run with --verbose and inspect the captured process output."
  | Baseline_failure ->
      "Make the configured baseline test stages pass, then retry."
  | Invariant_violation ->
      "Preserve the report and owner-private log, then report this as a tool \
       defect."
  | Resource_busy ->
      "Wait for the active run or maintenance operation to finish."
  | Io_failure ->
      "Check the reported path, free space, and permissions, then retry."
  | Corrupt_cache ->
      "Run `ocaml-mutants cache gc`; use clean only if corruption remains."
  | Interrupted_by_user ->
      "Re-run the command to resume from its last checkpoint."

let create ~phase ~cause ?code ?remediation ?(context = []) format =
  Format.kasprintf
    (fun message ->
      {
        phase;
        cause;
        code = Option.value code ~default:(default_code phase cause);
        remediation =
          Option.value remediation ~default:(default_remediation cause);
        message;
        context;
        suppressed = [];
      })
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

let restore_with_details ~phase ~cause ~code ~remediation ~message ~context
    ~suppressed =
  { phase; cause; code; remediation; message; context; suppressed }

let restore ~phase ~cause ~message ~context ~suppressed =
  restore_with_details ~phase ~cause ~code:(default_code phase cause)
    ~remediation:(default_remediation cause)
    ~message ~context ~suppressed

let phase error = error.phase
let cause error = error.cause
let code error = error.code
let remediation error = error.remediation
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
  Format.fprintf formatter "[%s %s/%s] %s" error.code (phase_name error.phase)
    (cause_name error.cause) error.message;
  List.iter
    (fun (key, value) -> Format.fprintf formatter "@,  %s: %s" key value)
    error.context;
  if error.remediation <> "" then
    Format.fprintf formatter "@,  next: %s" error.remediation;
  List.iter
    (fun suppressed ->
      Format.fprintf formatter "@,  suppressed cleanup error: %a" pp suppressed)
    error.suppressed
