open Util

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
end

module Make (Process : PROCESS) = struct
  let command ~cancel root argv = Process.run ~cancel ~cwd:root ~env:[] argv

  let output_lines result =
    split_lines (Process.stdout result)
    |> List.map String.trim
    |> List.filter (fun line -> line <> "")

  let interrupted result =
    if Process.cancelled result then
      Some
        (Error.create ~phase:Error.Analysis ~cause:Error.Interrupted_by_user
           "Git selection was interrupted")
    else None

  let files ~cancel ~root ~from =
    let* base =
      match from with
      | Some revision -> Ok revision
      | None -> (
          let upstream =
            command ~cancel root
              [
                "git";
                "rev-parse";
                "--abbrev-ref";
                "--symbolic-full-name";
                "@{upstream}";
              ]
          in
          match interrupted upstream with
          | Some error -> Error error
          | None when not (Process.succeeded upstream) ->
              Error
                (Error.make Error.Usage
                   "--changed could not find an upstream branch; use \
                    --changed-from REV")
          | None -> (
              let upstream_name = String.trim (Process.stdout upstream) in
              let merge_base =
                command ~cancel root
                  [ "git"; "merge-base"; "HEAD"; upstream_name ]
              in
              match interrupted merge_base with
              | Some error -> Error error
              | None when Process.succeeded merge_base ->
                  Ok (String.trim (Process.stdout merge_base))
              | None ->
                  Error
                    (Error.make Error.Tool "git merge-base failed: %s"
                       (Process.stderr merge_base))))
    in
    let changed =
      command ~cancel root
        [ "git"; "diff"; "--name-only"; "--diff-filter=ACMR"; base; "--" ]
    in
    match interrupted changed with
    | Some error -> Error error
    | None when not (Process.succeeded changed) ->
        Error
          (Error.make Error.Tool "git diff failed: %s" (Process.stderr changed))
    | None -> (
        let untracked =
          command ~cancel root
            [ "git"; "ls-files"; "--others"; "--exclude-standard" ]
        in
        match interrupted untracked with
        | Some error -> Error error
        | None when not (Process.succeeded untracked) ->
            Error
              (Error.make Error.Tool "git ls-files failed: %s"
                 (Process.stderr untracked))
        | None ->
            Ok
              (output_lines changed @ output_lines untracked
              |> List.map Ocaml_mutants_core.Mutant.normalize_path
              |> List.sort_uniq String.compare))
end

module System_process = struct
  type result = Process_supervisor.result

  let run ~cancel ~cwd ~env argv = Process_supervisor.run ~cancel ~cwd ~env argv

  let cancelled result =
    match result.Process_supervisor.status with
    | Process_supervisor.Cancelled -> true
    | _ -> false

  let succeeded = Process_supervisor.succeeded
  let stdout result = result.Process_supervisor.stdout
  let stderr result = result.Process_supervisor.stderr
end

module System = Make (System_process)

let files = System.files
