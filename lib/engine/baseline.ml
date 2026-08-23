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

type evidence = {
  completed_stages : Run_store.baseline_stage list;
  partial_stage : Run_store.baseline_stage option;
  slowest : Core.Duration.t option;
}

type stop_kind = Command_failure | Cancellation
type incomplete = { evidence : evidence; error : Error.t; kind : stop_kind }
type complete = { evidence : evidence; slowest : Core.Duration.t }
type outcome = Completed of complete | Incomplete of incomplete

let completed_stages evidence = evidence.completed_stages
let partial_stage evidence = evidence.partial_stage

let stages evidence =
  match evidence.partial_stage with
  | None -> evidence.completed_stages
  | Some stage -> evidence.completed_stages @ [ stage ]

let slowest (evidence : evidence) = evidence.slowest
let was_interrupted incomplete = incomplete.kind = Cancellation

let test_command = Test_command.resolve

let slowest_duration = function
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun slowest duration ->
             if Core.Duration.compare duration slowest > 0 then duration
             else slowest)
           first rest)

let measured_stage (stage : Config.stage) runs =
  match slowest_duration runs with
  | None -> None
  | Some slowest ->
      Some
        { Run_store.name = stage.name; command = stage.command; runs; slowest }

let evidence ~completed_stages ~partial_stage =
  let measured =
    match partial_stage with
    | None -> completed_stages
    | Some stage -> completed_stages @ [ stage ]
  in
  let durations =
    List.concat_map
      (fun (stage : Run_store.baseline_stage) -> stage.runs)
      measured
  in
  { completed_stages; partial_stage; slowest = slowest_duration durations }

module Make (Process : PROCESS) = struct
  let interrupted () =
    Error.create ~phase:Error.Baseline_proof ~cause:Error.Interrupted_by_user
      "baseline proof was interrupted"

  let failed (stage : Config.stage) result =
    Error.create ~phase:Error.Baseline_proof ~cause:Error.Baseline_failure
      ~context:[ ("stage", stage.name); ("status", Process.status result) ]
      "baseline stage %S failed:\n%s%s" stage.name (Process.stdout result)
      (Process.stderr result)

  let invalid_duration (stage : Config.stage) result message =
    Error.create ~phase:Error.Baseline_proof ~cause:Error.Invariant_violation
      ~context:
        [
          ("stage", stage.name);
          ("status", Process.status result);
          ("duration_error", message);
        ]
      "baseline stage %S returned an invalid duration" stage.name

  let run ~cancel ~root ~config ~build_dir =
    let repetitions =
      Core.Positive_int.to_int config.Config.test.baseline_runs
    in
    let stop ~completed_stages ~stage ~runs ~kind error =
      let partial_stage = measured_stage stage (List.rev runs) in
      Incomplete
        { evidence = evidence ~completed_stages ~partial_stage; error; kind }
    in
    let rec run_repetitions ~completed_stages ~stage_index stage remaining runs
        =
      if remaining = 0 then
        match measured_stage stage (List.rev runs) with
        | Some measured -> `Stage_completed measured
        | None ->
            let error =
              Error.create ~phase:Error.Baseline_proof
                ~cause:Error.Invariant_violation
                ~context:[ ("stage", stage.Config.name) ]
                "baseline stage completed without measurements"
            in
            `Stopped
              (stop ~completed_stages ~stage ~runs ~kind:Command_failure error)
      else
        let result =
          Process.run ~cancel ~cwd:root
            ~env:
              [
                ("OCAML_MUTANTS_ACTIVE", None); ("DUNE_CACHE", Some "disabled");
              ]
            (test_command stage.Config.command
               (Printf.sprintf "%s-stage-%d" build_dir stage_index))
        in
        match Process.disposition result with
        | Cancelled ->
            `Stopped
              (stop ~completed_stages ~stage ~runs ~kind:Cancellation
                 (interrupted ()))
        | Failed ->
            `Stopped
              (stop ~completed_stages ~stage ~runs ~kind:Command_failure
                 (failed stage result))
        | Succeeded -> (
            match
              Core.Duration.of_seconds (Process.duration_seconds result)
            with
            | Error message ->
                `Stopped
                  (stop ~completed_stages ~stage ~runs ~kind:Command_failure
                     (invalid_duration stage result message))
            | Ok duration ->
                run_repetitions ~completed_stages ~stage_index stage
                  (remaining - 1) (duration :: runs))
    in
    let rec run_stages stage_index completed_stages = function
      | [] -> (
          let evidence =
            evidence
              ~completed_stages:(List.rev completed_stages)
              ~partial_stage:None
          in
          match evidence.slowest with
          | Some slowest -> Completed { evidence; slowest }
          | None ->
              Incomplete
                {
                  evidence;
                  error =
                    Error.create ~phase:Error.Baseline_proof
                      ~cause:Error.Invariant_violation
                      "completed baseline proof contained no measurements";
                  kind = Command_failure;
                })
      | stage :: rest -> (
          let completed = List.rev completed_stages in
          match
            run_repetitions ~completed_stages:completed ~stage_index stage
              repetitions []
          with
          | `Stopped outcome -> outcome
          | `Stage_completed measured ->
              run_stages (stage_index + 1) (measured :: completed_stages) rest)
    in
    run_stages 0 [] config.Config.test.stages
end

module System_process = struct
  type result = Process_supervisor.result

  let run ~cancel ~cwd ~env command =
    Process_supervisor.run ~cancel ~cwd ~env command

  let disposition result =
    match result.Process_supervisor.status with
    | Process_supervisor.Cancelled -> Cancelled
    | _ when Process_supervisor.succeeded result -> Succeeded
    | _ -> Failed

  let duration_seconds result = result.Process_supervisor.duration
  let stdout result = result.Process_supervisor.stdout
  let stderr result = result.Process_supervisor.stderr

  let status result =
    Process_supervisor.status_string result.Process_supervisor.status
end

module System = Make (System_process)

let run = System.run
