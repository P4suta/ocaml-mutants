open Util
module Core = Ocaml_mutants_core

let workflow_error ?(cause = Error.Workspace_violation) ?(context = [])
    ?remediation message =
  Error.create ~phase:Error.Cli ~cause ~context ?remediation "%s" message

let find run prefix =
  if not (Core.Mutant.Id.is_valid_prefix prefix) then
    Error
      (workflow_error ~cause:Error.Invalid_input
         "mutant ID must be a lowercase hexadecimal full ID or prefix")
  else
    let matches =
      List.filter
        (fun result ->
          string_starts_with ~prefix
            (Core.Mutant.Id.full (Core.Mutant.id result.Run_store.mutant)))
        run.Run_store.results
    in
    match matches with
    | [ result ] -> Ok result
    | [] ->
        Error
          (workflow_error ~cause:Error.Invalid_input
             (Printf.sprintf "no mutant in run %s matches %s"
                (Core.Run_id.to_string run.metadata.id)
                prefix))
    | _ ->
        Error
          (workflow_error ~cause:Error.Invalid_input
             (Printf.sprintf "mutant prefix %s is ambiguous (%d matches)" prefix
                (List.length matches)))

let patch mutant =
  Format.asprintf "--- a/%s\n+++ b/%s\n@@ %a @@\n-%s\n+%s\n"
    (Core.Mutant.path mutant) (Core.Mutant.path mutant) Core.Source_range.pp
    (Core.Mutant.range mutant)
    (Core.Mutant.original mutant)
    (Core.Mutant.replacement mutant)

let show (result : Run_store.mutant_result) =
  let mutant = result.mutant in
  let stages =
    result.stages
    |> List.map (fun (stage : Run_store.stage_result) ->
        Printf.sprintf "  %s: %s (%.3fs)" stage.Run_store.name stage.status
          (Core.Duration.to_seconds stage.duration))
    |> String.concat "\n"
  in
  Printf.sprintf
    "id: %s\n\
     lineage: %s\n\
     outcome: %s\n\
     evidence: %s\n\
     location: %s:%d:%d\n\
     rule: %s\n\
     %s\n\
     stages:\n\
     %s\n\
     stdout:\n\
     %s\n\
     stderr:\n\
     %s\n"
    (Core.Mutant.Id.full (Core.Mutant.id mutant))
    (Core.Mutant.lineage_id mutant)
    (Core.Outcome.name result.outcome)
    (Run_store.result_evidence_level result |> Run_store.evidence_level_name)
    (Core.Mutant.path mutant)
    (Core.Source_range.start_line (Core.Mutant.range mutant))
    (Core.Source_range.start_column (Core.Mutant.range mutant))
    (Core.Operator.Rule.stable_name (Core.Mutant.rule mutant))
    (patch mutant) stages result.stdout.contents result.stderr.contents

let canonical path =
  try Unix.realpath path with Unix.Unix_error _ | Sys_error _ -> path

let normalized_for_compare path =
  let value = canonical path in
  if Sys.win32 then String.lowercase_ascii value else value

let within ~root path =
  let root = normalized_for_compare root in
  let path = normalized_for_compare path in
  String.equal root path
  || string_starts_with ~prefix:(root ^ Filename.dir_sep) path

let target_path ~root mutant =
  let relative = Core.Mutant.path mutant in
  if not (Filename.is_relative relative) then
    Error (workflow_error "mutant source path is absolute")
  else
    let target = Filename.concat root relative in
    let parent = Filename.dirname target in
    if not (within ~root parent) then
      Error (workflow_error "mutant source path escapes the workspace")
    else
      try
        let stat = Unix.lstat (windows_extended_path target) in
        if stat.st_kind <> Unix.S_REG then
          Error
            (workflow_error
               ~context:[ ("path", target) ]
               "refusing to mutate a non-regular source file")
        else Ok target
      with Unix.Unix_error (error, operation, argument) ->
        Error
          (workflow_error ~cause:Error.Io_failure
             ~context:[ ("path", target); ("operation", operation) ]
             (Printf.sprintf "%s: %s" argument (Unix.error_message error)))

let digest contents = Core.Source.(digest (of_string contents))

let replacement_contents mutant contents =
  let range = Core.Mutant.range mutant in
  let start_byte = Core.Source_range.start_byte range in
  let end_byte = Core.Source_range.end_byte range in
  if
    start_byte < 0
    || end_byte > String.length contents
    || start_byte >= end_byte
  then Error (workflow_error "mutant byte range is outside the current source")
  else
    let actual = String.sub contents start_byte (end_byte - start_byte) in
    if not (String.equal actual (Core.Mutant.original mutant)) then
      Error
        (workflow_error
           ~context:
             [
               ("expected_fragment", Core.Mutant.original mutant);
               ("actual_fragment", actual);
             ]
           ~remediation:"Regenerate the run; the source range has changed."
           "mutant source range no longer contains its recorded original")
    else
      Ok
        (String.sub contents 0 start_byte
        ^ Core.Mutant.replacement mutant
        ^ String.sub contents end_byte (String.length contents - end_byte))

type undo = {
  id : string;
  path : string;
  original_source : string;
  original_digest : string;
  applied_digest : string;
  start_byte : int;
  end_byte : int;
  original_fragment : string;
  replacement : string;
  status : string;
}

let undo_body undo =
  `Assoc
    [
      ("id", `String undo.id);
      ("path", `String undo.path);
      ("original_source", `String undo.original_source);
      ("original_digest", `String undo.original_digest);
      ("applied_digest", `String undo.applied_digest);
      ("start_byte", `Int undo.start_byte);
      ("end_byte", `Int undo.end_byte);
      ("original_fragment", `String undo.original_fragment);
      ("replacement", `String undo.replacement);
      ("status", `String undo.status);
    ]

let undo_string undo =
  let body = undo_body undo in
  `Assoc
    [
      ("document_type", `String "ocaml-mutants.undo-v1");
      ("schema_version", `Int 1);
      ("body", body);
      ("checksum_sha256", `String (sha256 (Yojson.Safe.to_string body)));
    ]
  |> Yojson.Safe.pretty_to_string ~std:true
  |> fun value -> value ^ "\n"

let decode_undo contents =
  try
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_string contents in
    if
      json |> member "document_type" |> to_string <> "ocaml-mutants.undo-v1"
      || json |> member "schema_version" |> to_int <> 1
    then Error "unsupported undo record"
    else
      let body = member "body" json in
      if
        json |> member "checksum_sha256" |> to_string
        <> sha256 (Yojson.Safe.to_string body)
      then Error "undo checksum mismatch"
      else
        Ok
          {
            id = body |> member "id" |> to_string;
            path = body |> member "path" |> to_string;
            original_source = body |> member "original_source" |> to_string;
            original_digest = body |> member "original_digest" |> to_string;
            applied_digest = body |> member "applied_digest" |> to_string;
            start_byte = body |> member "start_byte" |> to_int;
            end_byte = body |> member "end_byte" |> to_int;
            original_fragment = body |> member "original_fragment" |> to_string;
            replacement = body |> member "replacement" |> to_string;
            status = body |> member "status" |> to_string;
          }
  with
  | Yojson.Json_error message
  | Yojson.Safe.Util.Type_error (message, _)
  | Invalid_argument message
  ->
    Error message

let ensure_private_directory path =
  let rec ensure path =
    if Sys.file_exists path then
      if Sys.is_directory path then Ok ()
      else Error (Printf.sprintf "%s is not a directory" path)
    else
      let parent = Filename.dirname path in
      let* () = if parent = path then Ok () else ensure parent in
      try
        Unix.mkdir path 0o700;
        Ok ()
      with Unix.Unix_error (error, operation, argument) ->
        Error
          (Printf.sprintf "%s(%s): %s" operation argument
             (Unix.error_message error))
  in
  let* () = ensure path in
  try
    Unix.chmod path 0o700;
    Ok ()
  with Unix.Unix_error (error, operation, argument) ->
    Error
      (Printf.sprintf "%s(%s): %s" operation argument (Unix.error_message error))

let undo_path ~root id =
  Filename.concat
    (Filename.concat (Filename.concat root ".ocaml-mutants") "undo")
    (id ^ ".json")

let write_undo path undo =
  let directory = Filename.dirname path in
  let* () = ensure_private_directory directory in
  let* () = atomic_write path (undo_string undo) in
  try
    Unix.chmod path 0o600;
    Ok ()
  with Unix.Unix_error (error, operation, argument) ->
    Error
      (Printf.sprintf "%s(%s): %s" operation argument (Unix.error_message error))

let map_io operation path = function
  | Ok value -> Ok value
  | Error message ->
      Error
        (workflow_error ~cause:Error.Io_failure
           ~context:[ ("path", path); ("operation", operation) ]
           message)

let apply ~root mutant =
  let root = canonical root in
  let* target = target_path ~root mutant in
  let* before = read_file target |> map_io "read" target in
  let actual_digest = digest before in
  if not (String.equal actual_digest (Core.Mutant.source_digest mutant)) then
    Error
      (workflow_error
         ~context:
           [
             ("path", target);
             ("expected_digest", Core.Mutant.source_digest mutant);
             ("actual_digest", actual_digest);
           ]
         ~remediation:"Regenerate the run; no source file was changed."
         "source digest changed since mutant discovery")
  else
    let* after = replacement_contents mutant before in
    let id = Core.Mutant.Id.full (Core.Mutant.id mutant) in
    let record =
      let range = Core.Mutant.range mutant in
      {
        id;
        path = Core.Mutant.path mutant;
        original_source = before;
        original_digest = actual_digest;
        applied_digest = digest after;
        start_byte = Core.Source_range.start_byte range;
        end_byte = Core.Source_range.end_byte range;
        original_fragment = Core.Mutant.original mutant;
        replacement = Core.Mutant.replacement mutant;
        status = "applied";
      }
    in
    let record_path = undo_path ~root id in
    let* () =
      write_undo record_path record |> map_io "write-undo" record_path
    in
    let* current = read_file target |> map_io "revalidate" target in
    if not (String.equal (digest current) actual_digest) then
      Error
        (workflow_error
           ~remediation:"Review the displayed patch and regenerate the run."
           "source changed during apply; the mutation was not written")
    else atomic_write target after |> map_io "atomic-apply" target

let resolve_undo_path ~root prefix =
  if not (Core.Mutant.Id.is_valid_prefix prefix) then
    Error (workflow_error ~cause:Error.Invalid_input "invalid mutant ID prefix")
  else
    let directory = Filename.dirname (undo_path ~root "placeholder") in
    if not (Sys.file_exists directory && Sys.is_directory directory) then
      Error (workflow_error ~cause:Error.Invalid_input "no undo records exist")
    else
      let matches =
        Sys.readdir directory |> Array.to_list
        |> List.filter (fun name ->
            Filename.check_suffix name ".json"
            && string_starts_with ~prefix (Filename.chop_suffix name ".json"))
      in
      match matches with
      | [ name ] -> Ok (Filename.concat directory name)
      | [] ->
          Error
            (workflow_error ~cause:Error.Invalid_input "undo record not found")
      | _ ->
          Error
            (workflow_error ~cause:Error.Invalid_input
               "undo ID prefix is ambiguous")

let revert ~root ~id =
  let root = canonical root in
  let* record_path = resolve_undo_path ~root id in
  let* encoded = read_file record_path |> map_io "read-undo" record_path in
  let* record =
    decode_undo encoded
    |> Result.map_error (fun message ->
        workflow_error ~cause:Error.Decode_failure
          ~context:[ ("path", record_path) ]
          message)
  in
  if record.status <> "applied" then
    Error
      (workflow_error ~cause:Error.Invalid_input "mutant is already reverted")
  else
    let target = Filename.concat root record.path in
    let* () =
      if within ~root:(canonical root) (Filename.dirname target) then Ok ()
      else Error (workflow_error "undo source path escapes the workspace")
    in
    let* () =
      try
        let metadata = Unix.lstat (windows_extended_path target) in
        if metadata.st_kind = Unix.S_REG then Ok ()
        else
          Error
            (workflow_error
               ~context:[ ("path", target) ]
               "refusing to revert a non-regular source file")
      with Unix.Unix_error (error, operation, argument) ->
        Error
          (workflow_error ~cause:Error.Io_failure
             ~context:[ ("path", target); ("operation", operation) ]
             (Printf.sprintf "%s: %s" argument (Unix.error_message error)))
    in
    let* current = read_file target |> map_io "read" target in
    if not (String.equal (digest current) record.applied_digest) then
      Error
        (workflow_error
           ~context:
             [
               ("path", target);
               ("expected_digest", record.applied_digest);
               ("actual_digest", digest current);
             ]
           ~remediation:
             "Resolve the conflict manually; the undo record remains private \
              and unchanged."
           "current source does not match the applied mutant")
    else
      let* () =
        atomic_write target record.original_source
        |> map_io "atomic-revert" target
      in
      write_undo record_path { record with status = "reverted" }
      |> map_io "close-undo" record_path

let revert_patch ~root ~id =
  let root = canonical root in
  let* record_path = resolve_undo_path ~root id in
  let* encoded = read_file record_path |> map_io "read-undo" record_path in
  let* record =
    decode_undo encoded
    |> Result.map_error (fun message ->
        workflow_error ~cause:Error.Decode_failure
          ~context:[ ("path", record_path) ]
          message)
  in
  Ok
    (Printf.sprintf "--- b/%s\n+++ a/%s\n@@ bytes %d..%d @@\n-%s\n+%s\n"
       record.path record.path record.start_byte
       (record.start_byte + String.length record.replacement)
       record.replacement record.original_fragment)

type expectation_edit = { path : string; before : string; after : string }

let toml_string value = Yojson.Safe.to_string (`String value)

let prepare_expectation ~root ~(config : Config.loaded) ~id ~reason =
  if String.length id <> 64 || not (Core.Mutant.Id.is_valid_prefix id) then
    Error
      (workflow_error ~cause:Error.Invalid_input
         "expect requires the complete 64-character mutant ID")
  else if String.trim reason = "" then
    Error
      (workflow_error ~cause:Error.Invalid_input
         "expect requires a non-empty reason")
  else if config.origin = Config.Version_1 then
    Error
      (workflow_error ~cause:Error.Invalid_input
         ~remediation:"Run `ocaml-mutants config migrate --write` first."
         "expectation edits require a version 2 config")
  else if
    List.exists
      (fun expectation -> String.equal expectation.Config.id id)
      config.config.mutation.expectations
  then
    Error
      (workflow_error ~cause:Error.Invalid_input
         "that mutant already has an expectation")
  else
    let path = Filename.concat root ".ocaml-mutants.toml" in
    let* before =
      if Sys.file_exists path then read_file path |> map_io "read-config" path
      else Ok (Config.to_toml config.config)
    in
    let separator =
      if before = "" || string_ends_with ~suffix:"\n" before then "" else "\n"
    in
    let after =
      before ^ separator ^ "\n[[mutation.expect]]\nid = " ^ toml_string id
      ^ "\nreason = " ^ toml_string reason ^ "\n"
    in
    let* _ =
      Config.parse_with_metadata ~file:path after
      |> Result.map_error (fun message ->
          workflow_error ~cause:Error.Decode_failure
            ("generated expectation edit is invalid: " ^ message))
    in
    Ok { path; before; after }

let commit_expectation edit =
  let* current =
    if Sys.file_exists edit.path then
      read_file edit.path |> map_io "revalidate-config" edit.path
    else Ok edit.before
  in
  if not (String.equal current edit.before) then
    Error
      (workflow_error
         ~remediation:"Review a fresh diff; the config was not overwritten."
         "configuration changed during expectation confirmation")
  else
    atomic_write edit.path edit.after |> map_io "atomic-config-write" edit.path
