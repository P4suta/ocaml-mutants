open Util

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

let normalize_cmt_target value =
  let normalized = Ocaml_mutants_core.Mutant.normalize_path value in
  let marker = "_build/default/" in
  let rec find index =
    if index + String.length marker > String.length normalized then None
    else if String.sub normalized index (String.length marker) = marker then
      Some (index + String.length marker)
    else find (index + 1)
  in
  match find 0 with
  | Some start ->
      Some (String.sub normalized start (String.length normalized - start))
  | None when Filename.is_relative value -> Some normalized
  | None -> None

let normalize_source ~root value =
  let normalized = Ocaml_mutants_core.Mutant.normalize_path value in
  let marker = "_build/default/" in
  let rec find index =
    if index + String.length marker > String.length normalized then None
    else if String.sub normalized index (String.length marker) = marker then
      Some (index + String.length marker)
    else find (index + 1)
  in
  let candidate =
    match find 0 with
    | Some start ->
        Filename.concat root
          (String.sub normalized start (String.length normalized - start))
    | None when Filename.is_relative value -> Filename.concat root value
    | None -> value
  in
  if Sys.file_exists candidate && not (Sys.is_directory candidate) then
    try Some (normalize_relative ~root candidate)
    with Unix.Unix_error _ -> None
  else None

let record = function
  | Sexp.List fields ->
      List.fold_left
        (fun result -> function
          | Sexp.List [ Sexp.Atom key; value ] ->
              Result.map (fun fields -> (key, value) :: fields) result
          | _ -> Error "expected a record field (name value)")
        (Ok []) fields
      |> Result.map List.rev
  | _ -> Error "expected a record"

let field fields name =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error (Printf.sprintf "missing required field %s" name)

let atom = function
  | Sexp.Atom value -> Ok value
  | _ -> Error "expected an atom"

let option_atom = function
  | Sexp.List [] -> Ok None
  | Sexp.List [ Sexp.Atom value ] -> Ok (Some value)
  | _ -> Error "expected an optional atom"

let list decoder = function
  | Sexp.List values ->
      List.fold_left
        (fun result value ->
          match (result, decoder value) with
          | Ok values, Ok value -> Ok (value :: values)
          | (Error _ as error), _ -> error
          | Ok _, Error error -> Error error)
        (Ok []) values
      |> Result.map List.rev
  | _ -> Error "expected a list"

type described_module = {
  impl : string option;
  intf : string option;
  cmt : string option;
  cmti : string option;
}

let decode_module value =
  let* fields = record value in
  let* impl_value = field fields "impl" in
  let* impl = option_atom impl_value in
  let* intf_value = field fields "intf" in
  let* intf = option_atom intf_value in
  let* cmt_value = field fields "cmt" in
  let* cmt = option_atom cmt_value in
  let* cmti_value = field fields "cmti" in
  let* cmti = option_atom cmti_value in
  Ok { impl; intf; cmt; cmti }

let decode_modules fields =
  let* value = field fields "modules" in
  list decode_module value

let paths_of_modules ~root modules =
  modules
  |> List.concat_map (fun module_ ->
      List.filter_map Fun.id [ module_.impl; module_.intf ])
  |> List.filter_map (normalize_source ~root)
  |> List.sort_uniq String.compare

let cmts_of_modules modules =
  modules
  |> List.concat_map (fun module_ ->
      List.filter_map Fun.id [ module_.cmt; module_.cmti ])
  |> List.filter_map normalize_cmt_target
  |> List.sort_uniq String.compare

let decode_library ~root payload =
  let* fields = record payload in
  let* name_value = field fields "name" in
  let* name = atom name_value in
  let* local_value = field fields "local" in
  let* local = atom local_value in
  if local <> "true" then Ok None
  else
    let* modules = decode_modules fields in
    let source_files = paths_of_modules ~root modules in
    Ok
      (Some
         ( { kind = Library; name = Some name; source_files },
           cmts_of_modules modules ))

let decode_executables ~root payload =
  let* fields = record payload in
  let* names_value = field fields "names" in
  let* names = list atom names_value in
  let* modules = decode_modules fields in
  let source_files = paths_of_modules ~root modules in
  Ok
    (Some
       ( {
           kind = Executable;
           name = (match names with [] -> None | name :: _ -> Some name);
           source_files;
         },
         cmts_of_modules modules ))

let decode_item ~root = function
  | Sexp.List [ Sexp.Atom "library"; payload ] -> decode_library ~root payload
  | Sexp.List [ Sexp.Atom "executables"; payload ] ->
      decode_executables ~root payload
  | Sexp.List [ Sexp.Atom ("root" | "build_context"); Sexp.Atom _ ] -> Ok None
  | Sexp.List (Sexp.Atom _unknown :: _) -> Ok None
  | _ -> Error "malformed dune workspace item"

let parse ~root contents =
  let* sexp = Sexp.parse contents in
  let* items = list (decode_item ~root) sexp in
  let decoded = List.filter_map Fun.id items in
  let targets = List.map fst decoded in
  let source_files =
    targets
    |> List.concat_map (fun (target : target) -> target.source_files)
    |> List.sort_uniq String.compare
  in
  let cmt_targets =
    decoded |> List.concat_map snd |> List.sort_uniq String.compare
  in
  Ok { source_files; cmt_targets; targets }

let decode_test value =
  let* fields = record value in
  let* name_value = field fields "name" in
  let* name = atom name_value in
  let* source_dir_value = field fields "source_dir" in
  let* source_dir = atom source_dir_value in
  let* target_value = field fields "target" in
  let* target = atom target_value in
  let* enabled_value = field fields "enabled" in
  let* enabled = atom enabled_value in
  if enabled = "true" then
    Ok
      (Some
         {
           name;
           source_dir = Ocaml_mutants_core.Mutant.normalize_path source_dir;
           target = Ocaml_mutants_core.Mutant.normalize_path target;
         })
  else if enabled = "false" then Ok None
  else Error "test enabled field must be true or false"

let parse_tests contents =
  let* sexp = Sexp.parse contents in
  let* tests = list decode_test sexp in
  Ok (List.filter_map Fun.id tests)

let source_role ~workspace ~tests path =
  let belongs_to_test target =
    match (target.kind, target.name) with
    | Executable, Some name ->
        List.exists (fun test -> String.equal name test.name) tests
    | (Library | Executable | Unknown), _ -> false
  in
  match
    List.find_opt
      (fun target ->
        belongs_to_test target && List.mem path target.source_files)
      workspace.targets
  with
  | Some _ -> Test
  | None -> Production

let with_analysis_lock ~cancel action =
  let path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      "ocaml-mutants-analysis-v1.lock"
  in
  try
    let descriptor = Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT ] 0o600 in
    Fun.protect
      ~finally:(fun () ->
        (try Unix.lockf descriptor Unix.F_ULOCK 0 with Unix.Unix_error _ -> ());
        Unix.close descriptor)
      (fun () ->
        let rec acquire () =
          if Cancel.is_requested cancel then
            Error
              (Error.create ~phase:Error.Analysis
                 ~cause:Error.Interrupted_by_user
                 "analysis lock wait was interrupted")
          else
            try
              Unix.lockf descriptor Unix.F_TLOCK 0;
              Ok ()
            with Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
              Unix.sleepf 0.02;
              acquire ()
        in
        match acquire () with
        | Error _ as error -> error
        | Ok () -> Ok (action ()))
  with Unix.Unix_error (error, operation, _) ->
    Error
      (Error.create ~phase:Error.Analysis ~cause:Error.Io_failure
         ~context:[ ("lock", path); ("operation", operation) ]
         "cannot coordinate analysis builds: %s" (Unix.error_message error))

module Make (Process : PROCESS) = struct
  let describe ~cancel ~root =
    let result =
      Process.run ~cancel ~cwd:root ~env:[]
        [
          "dune"; "describe"; "workspace"; "--format"; "csexp"; "--lang"; "0.1";
        ]
    in
    if Process.cancelled result then
      Error
        (Error.create ~phase:Error.Dune ~cause:Error.Interrupted_by_user
           "dune describe was interrupted")
    else if not (Process.succeeded result) then
      Error
        (Error.create ~phase:Error.Dune ~cause:Error.Process_failure
           ~context:[ ("status", Process.status result) ]
           "dune describe workspace failed:\n%s" (Process.stderr result))
    else
      match parse ~root (Process.stdout result) with
      | Ok workspace -> Ok workspace
      | Error message ->
          Error
            (Error.create ~phase:Error.Dune ~cause:Error.Decode_failure
               "invalid dune describe workspace v0.1 output: %s" message)

  let describe_tests ~cancel ~root =
    let result =
      Process.run ~cancel ~cwd:root ~env:[]
        [ "dune"; "describe"; "tests"; "--format"; "csexp" ]
    in
    if Process.cancelled result then
      Error
        (Error.create ~phase:Error.Dune ~cause:Error.Interrupted_by_user
           "dune test description was interrupted")
    else if not (Process.succeeded result) then
      Error
        (Error.create ~phase:Error.Dune ~cause:Error.Process_failure
           ~context:[ ("status", Process.status result) ]
           "dune describe tests failed:\n%s" (Process.stderr result))
    else
      match parse_tests (Process.stdout result) with
      | Ok tests -> Ok tests
      | Error message ->
          Error
            (Error.create ~phase:Error.Dune ~cause:Error.Decode_failure
               "invalid experimental dune describe tests output: %s" message)

  let build_analysis ~cancel ~root ~build_dir ~cmt_targets =
    let locked =
      with_analysis_lock ~cancel (fun () ->
          Process.run ~cancel ~cwd:root
            ~env:(Test_command.dune_cache_environment ~root)
            ([ "dune"; "build"; "--build-dir"; build_dir; "@all" ] @ cmt_targets))
    in
    let* result = locked in
    if Process.cancelled result then
      Error
        (Error.create ~phase:Error.Analysis ~cause:Error.Interrupted_by_user
           "analysis build was interrupted")
    else if Process.succeeded result then Ok result
    else
      Error
        (Error.create ~phase:Error.Analysis ~cause:Error.Process_failure
           ~context:[ ("status", Process.status result) ]
           "analysis build failed:\n%s" (Process.stderr result))
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

  let status (result : Process_supervisor.result) =
    Process_supervisor.status_string result.status
end

module System = Make (System_process)

let describe = System.describe
let describe_tests = System.describe_tests
let build_analysis = System.build_analysis

let cmt_files ~root ~build_dir =
  let directory = Filename.concat root build_dir in
  files_recursive directory
  |> List.filter (string_ends_with ~suffix:".cmt")
  |> List.map (Filename.concat directory)
