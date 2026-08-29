open Util
module Core = Ocaml_mutants_core

type assignment = {
  index : int;
  mutant_ids : string list;
  estimated_duration_seconds : float;
}

type t = {
  plan_id : string;
  input_fingerprint : string;
  workspace_digest : string;
  toolchain : string;
  catalog_ids : string list;
  assignments : assignment list;
}

let framed values =
  values
  |> List.map (fun value -> Printf.sprintf "%d:%s" (String.length value) value)
  |> String.concat ""

let fingerprint_ids ~workspace_digest ~toolchain ~config ~catalog_ids =
  sha256
    (framed
       ([
          "ocaml-mutants-shard-input-v1";
          workspace_digest;
          toolchain;
          Config.to_toml config;
        ]
       @ catalog_ids))

let input_fingerprint ~workspace_digest ~toolchain ~config ~catalog =
  let catalog_ids =
    Core.Catalog.to_list catalog
    |> List.map (fun mutant -> Core.Mutant.Id.full (Core.Mutant.id mutant))
    |> List.sort String.compare
  in
  fingerprint_ids ~workspace_digest ~toolchain ~config ~catalog_ids

let stable_bucket id shard_count =
  String.fold_left
    (fun value character ->
      ((value * 33) + Char.code character) mod shard_count)
    0 id

let assignment_body assignments =
  assignments
  |> List.concat_map (fun assignment ->
      string_of_int assignment.index
      :: Printf.sprintf "%.17g" assignment.estimated_duration_seconds
      :: assignment.mutant_ids)

let calculate_plan_id input_fingerprint assignments =
  sha256
    (framed
       ("ocaml-mutants-shard-plan-v1" :: input_fingerprint
       :: assignment_body assignments))

let create ~workspace_digest ~toolchain ~config ~catalog ~shard_count ~durations
    =
  if shard_count < 1 then Error "shard count must be at least one"
  else
    let catalog_ids =
      Core.Catalog.to_list catalog
      |> List.map (fun mutant -> Core.Mutant.Id.full (Core.Mutant.id mutant))
      |> List.sort String.compare
    in
    let input_fingerprint =
      fingerprint_ids ~workspace_digest ~toolchain ~config ~catalog_ids
    in
    let known = Hashtbl.create (List.length durations) in
    List.iter
      (fun (id, duration) ->
        if Float.is_finite duration && duration >= 0. then
          Hashtbl.replace known id duration)
      durations;
    let buckets = Array.make shard_count [] in
    let loads = Array.make shard_count 0. in
    let () =
      if Hashtbl.length known = 0 then
        List.iter
          (fun id ->
            let index = stable_bucket id shard_count in
            buckets.(index) <- id :: buckets.(index))
          catalog_ids
      else
        let weighted =
          catalog_ids
          |> List.map (fun id ->
              (id, Option.value (Hashtbl.find_opt known id) ~default:1.))
          |> List.sort (fun (left_id, left) (right_id, right) ->
              let by_weight = Float.compare right left in
              if by_weight <> 0 then by_weight
              else String.compare left_id right_id)
        in
        List.iter
          (fun (id, weight) ->
            let target = ref 0 in
            for index = 1 to shard_count - 1 do
              if loads.(index) < loads.(!target) then target := index
            done;
            buckets.(!target) <- id :: buckets.(!target);
            loads.(!target) <- loads.(!target) +. weight)
          weighted
    in
    let assignments =
      Array.to_list
        (Array.mapi
           (fun index mutant_ids ->
             {
               index;
               mutant_ids = List.sort String.compare mutant_ids;
               estimated_duration_seconds = loads.(index);
             })
           buckets)
    in
    let plan_id = calculate_plan_id input_fingerprint assignments in
    Ok
      {
        plan_id;
        input_fingerprint;
        workspace_digest;
        toolchain;
        catalog_ids;
        assignments;
      }

let assignment plan index =
  match
    List.find_opt (fun assignment -> assignment.index = index) plan.assignments
  with
  | Some assignment -> Ok assignment
  | None ->
      Error
        (Printf.sprintf "shard index %d is outside 0..%d" index
           (List.length plan.assignments - 1))

let selection_tag plan ~index =
  Printf.sprintf "shard:%s:%s:%d:%d" plan.plan_id plan.input_fingerprint index
    (List.length plan.assignments)

let assignment_to_json assignment =
  `Assoc
    [
      ("index", `Int assignment.index);
      ( "mutant_ids",
        `List (List.map (fun id -> `String id) assignment.mutant_ids) );
      ( "estimated_duration_seconds",
        `Float assignment.estimated_duration_seconds );
    ]

let to_yojson plan =
  `Assoc
    [
      ("document_type", `String "ocaml-mutants.shard-plan-v1");
      ("schema_version", `Int 1);
      ("plan_id", `String plan.plan_id);
      ("input_fingerprint", `String plan.input_fingerprint);
      ("workspace_digest", `String plan.workspace_digest);
      ("toolchain", `String plan.toolchain);
      ("catalog_ids", `List (List.map (fun id -> `String id) plan.catalog_ids));
      ("shard_count", `Int (List.length plan.assignments));
      ("assignments", `List (List.map assignment_to_json plan.assignments));
    ]

let to_string plan =
  Yojson.Safe.pretty_to_string ~std:true (to_yojson plan) ^ "\n"

let has_only_fields allowed = function
  | `Assoc fields ->
      List.for_all (fun (name, _) -> List.mem name allowed) fields
  | _ -> false

let of_yojson json =
  try
    let open Yojson.Safe.Util in
    if
      not
        (has_only_fields
           [
             "document_type";
             "schema_version";
             "plan_id";
             "input_fingerprint";
             "workspace_digest";
             "toolchain";
             "catalog_ids";
             "shard_count";
             "assignments";
           ]
           json)
    then Error "shard plan contains unknown fields"
    else if
      json |> member "document_type" |> to_string
      <> "ocaml-mutants.shard-plan-v1"
    then Error "unsupported shard plan document"
    else if json |> member "schema_version" |> to_int <> 1 then
      Error "unsupported shard plan schema version"
    else
      let decode_assignment value =
        if
          not
            (has_only_fields
               [ "index"; "mutant_ids"; "estimated_duration_seconds" ]
               value)
        then Error "shard assignment contains unknown fields"
        else
          let duration =
            value |> member "estimated_duration_seconds" |> to_float
          in
          if (not (Float.is_finite duration)) || duration < 0. then
            Error "shard duration must be finite and non-negative"
          else
            Ok
              {
                index = value |> member "index" |> to_int;
                mutant_ids =
                  value |> member "mutant_ids" |> to_list |> List.map to_string;
                estimated_duration_seconds = duration;
              }
      in
      let rec decode decoded = function
        | [] -> Ok (List.rev decoded)
        | value :: rest ->
            let* assignment = decode_assignment value in
            decode (assignment :: decoded) rest
      in
      let* assignments = decode [] (json |> member "assignments" |> to_list) in
      let plan =
        {
          plan_id = json |> member "plan_id" |> to_string;
          input_fingerprint = json |> member "input_fingerprint" |> to_string;
          workspace_digest = json |> member "workspace_digest" |> to_string;
          toolchain = json |> member "toolchain" |> to_string;
          catalog_ids =
            json |> member "catalog_ids" |> to_list |> List.map to_string;
          assignments;
        }
      in
      let shard_count = json |> member "shard_count" |> to_int in
      let indexes = List.map (fun assignment -> assignment.index) assignments in
      let assigned =
        List.concat_map (fun assignment -> assignment.mutant_ids) assignments
      in
      if shard_count < 1 || shard_count <> List.length assignments then
        Error "shard_count contradicts assignments"
      else if indexes <> List.init shard_count Fun.id then
        Error "shard assignment indexes are not canonical"
      else if List.sort_uniq String.compare assigned <> plan.catalog_ids then
        Error "shard assignments do not partition catalog_ids exactly"
      else if plan.catalog_ids <> List.sort_uniq String.compare plan.catalog_ids
      then Error "catalog_ids are not unique and canonically ordered"
      else if
        calculate_plan_id plan.input_fingerprint assignments <> plan.plan_id
      then Error "plan_id does not match the canonical plan body"
      else Ok plan
  with Yojson.Safe.Util.Type_error (message, _) | Invalid_argument message ->
    Error message

let of_string contents =
  try of_yojson (Yojson.Safe.from_string contents)
  with Yojson.Json_error message -> Error message

let parse_selection selection =
  match String.split_on_char ':' selection with
  | [ "shard"; plan_id; fingerprint; index; count ] -> (
      match (int_of_string_opt index, int_of_string_opt count) with
      | Some index, Some count -> Ok (plan_id, fingerprint, index, count)
      | _ -> Error "shard selection has invalid numeric fields")
  | _ -> Error "run was not produced by a shard plan"

let merge_expectations runs =
  let rank = function
    | Run_store.Expectation_not_evaluated -> 0
    | Expectation_stale -> 1
    | Expectation_fulfilled | Expectation_unfulfilled_killed
    | Expectation_unfulfilled_confirmed_timeout | Expectation_inconclusive _
    | Expectation_error _ ->
        2
  in
  let table : (string, Run_store.expectation_evaluation) Hashtbl.t =
    Hashtbl.create 16
  in
  List.iter
    (fun run ->
      List.iter
        (fun (evaluation : Run_store.expectation_evaluation) ->
          match Hashtbl.find_opt table evaluation.Run_store.mutant_id with
          | Some (existing : Run_store.expectation_evaluation)
            when rank existing.Run_store.status >= rank evaluation.status ->
              ()
          | _ -> Hashtbl.replace table evaluation.mutant_id evaluation)
        run.Run_store.expectations)
    runs;
  Hashtbl.to_seq_values table
  |> List.of_seq
  |> List.sort (fun left right ->
      String.compare left.Run_store.mutant_id right.mutant_id)

let merge ~plan ~id ~finished_at runs =
  match runs with
  | [] -> Error "merge requires at least one shard report"
  | first :: _ ->
      let count = List.length plan.assignments in
      let by_index = Hashtbl.create count in
      let rec validate = function
        | [] -> Ok ()
        | run :: rest ->
            let* plan_id, fingerprint, index, encoded_count =
              parse_selection run.Run_store.metadata.selection
            in
            if plan_id <> plan.plan_id || fingerprint <> plan.input_fingerprint
            then Error "shard report belongs to a different plan or fingerprint"
            else if encoded_count <> count then
              Error "shard count differs from plan"
            else if
              run.metadata.workspace_digest <> plan.workspace_digest
              || run.metadata.toolchain <> plan.toolchain
            then Error "shard report workspace or toolchain differs from plan"
            else if Hashtbl.mem by_index index then
              Error (Printf.sprintf "duplicate shard report for index %d" index)
            else
              let* expected = assignment plan index in
              let actual =
                run.results
                |> List.map (fun result ->
                    Core.Mutant.Id.full (Core.Mutant.id result.Run_store.mutant))
                |> List.sort String.compare
              in
              if run.status <> Run_store.Completed then
                Error (Printf.sprintf "shard %d did not complete" index)
              else if run.completeness <> Run_store.Complete then
                Error
                  (Printf.sprintf "shard %d contains incomplete evidence" index)
              else if actual <> expected.mutant_ids then
                Error
                  (Printf.sprintf "shard %d results differ from its assignment"
                     index)
              else (
                Hashtbl.add by_index index run;
                validate rest)
      in
      let* () = validate runs in
      if Hashtbl.length by_index <> count then
        Error
          (Printf.sprintf "merge is missing %d shard report(s)"
             (count - Hashtbl.length by_index))
      else
        let results_by_id = Hashtbl.create (List.length plan.catalog_ids) in
        List.iter
          (fun run ->
            List.iter
              (fun result ->
                let id =
                  Core.Mutant.Id.full (Core.Mutant.id result.Run_store.mutant)
                in
                Hashtbl.replace results_by_id id result)
              run.Run_store.results)
          runs;
        let results = List.map (Hashtbl.find results_by_id) plan.catalog_ids in
        Ok
          {
            Run_store.metadata =
              {
                first.metadata with
                id;
                finished_at;
                selection = "shard-merge:" ^ plan.plan_id;
                cache_key = plan.input_fingerprint;
                input_fingerprint = plan.input_fingerprint;
              };
            status = Run_store.Completed;
            results;
            completeness = Run_store.Complete;
            expectations = merge_expectations runs;
            skipped = first.skipped;
            warnings =
              List.concat_map (fun run -> run.Run_store.warnings) runs
              @ [
                  {
                    Run_store.code = "shard-merge";
                    message = "merged deterministic shard plan " ^ plan.plan_id;
                  };
                ];
          }
