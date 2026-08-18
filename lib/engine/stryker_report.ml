module Core = Ocaml_mutants_core

type thresholds = { high : int; low : int }

let thresholds ~high ~low =
  if high < 0 || high > 100 then
    Error "high threshold must be between 0 and 100"
  else if low < 0 || low > 100 then
    Error "low threshold must be between 0 and 100"
  else if low > high then Error "low threshold must not exceed high threshold"
  else Ok { high; low }

type endpoint = Start | End

type error =
  | Source_unavailable of { path : string; reason : string }
  | Source_digest_mismatch of {
      path : string;
      expected : string;
      actual : string;
    }
  | Source_span_invalid of { path : string; mutant_id : string }
  | Source_location_mismatch of {
      path : string;
      mutant_id : string;
      endpoint : endpoint;
      recorded_line : int;
      recorded_column : int;
      derived_line : int;
      derived_column : int;
    }
  | Source_original_mismatch of {
      path : string;
      mutant_id : string;
      expected : string;
      actual : string;
    }
  | Duplicate_mutant_id of {
      mutant_id : string;
      first_path : string;
      duplicate_path : string;
    }

let endpoint_name = function Start -> "start" | End -> "end"

let pp_error formatter = function
  | Source_unavailable { path; reason } ->
      Format.fprintf formatter "cannot read source %s: %s" path reason
  | Source_digest_mismatch { path; expected; actual } ->
      Format.fprintf formatter
        "source digest for %s does not match the native report: expected %s, \
         got %s"
        path expected actual
  | Source_span_invalid { path; mutant_id } ->
      Format.fprintf formatter "mutant %s has an invalid source span in %s"
        mutant_id path
  | Source_location_mismatch
      {
        path;
        mutant_id;
        endpoint;
        recorded_line;
        recorded_column;
        derived_line;
        derived_column;
      } ->
      Format.fprintf formatter
        "mutant %s has a %s location mismatch in %s: recorded %d:%d, derived \
         %d:%d from its byte offset"
        mutant_id (endpoint_name endpoint) path recorded_line recorded_column
        derived_line derived_column
  | Source_original_mismatch { path; mutant_id; expected; actual } ->
      Format.fprintf formatter
        "mutant %s source in %s does not match the native report: expected %S, \
         got %S"
        mutant_id path expected actual
  | Duplicate_mutant_id { mutant_id; first_path; duplicate_path } ->
      Format.fprintf formatter
        "duplicate mutant ID %s first appears in %s and appears again in %s"
        mutant_id first_path duplicate_path

type entry = Executed of Run_store.mutant_result | Not_run of Core.Mutant.t

let mutant_of_entry = function
  | Executed result -> result.Run_store.mutant
  | Not_run mutant -> mutant

let full_id mutant = Core.Mutant.Id.full (Core.Mutant.id mutant)

let compare_entry left right =
  let left = mutant_of_entry left in
  let right = mutant_of_entry right in
  match String.compare (Core.Mutant.path left) (Core.Mutant.path right) with
  | 0 -> String.compare (full_id left) (full_id right)
  | value -> value

let entries run =
  List.map (fun result -> Executed result) run.Run_store.results
  @ List.map (fun mutant -> Not_run mutant) (Run_store.not_run run)
  |> List.sort compare_entry

let mutator_name mutant =
  match Core.Mutant.family mutant with
  | Core.Operator.Boolean_literal -> "BooleanLiteral"
  | Core.Operator.Condition_negation -> "ConditionalExpression"
  | Core.Operator.Boolean_connective -> "LogicalOperator"
  | Core.Operator.Comparison -> "EqualityOperator"
  | Core.Operator.Integer_arithmetic -> "ArithmeticOperator"
  | Core.Operator.Float_arithmetic -> "ArithmeticOperator"
  | Core.Operator.If_branch -> "ConditionalExpression"
  | Core.Operator.Sequence_deletion -> "BlockStatement"
  | Core.Operator.Return_replacement -> "ReturnValue"

let expected_reason result message =
  match result.Run_store.expected_reason with
  | None -> message
  | Some reason ->
      let expectation = "Expected survivor was unfulfilled: " ^ reason in
      Some
        (match message with
        | None -> expectation
        | Some message -> message ^ "; " ^ expectation)

let status_and_reason (result : Run_store.mutant_result) =
  match result.outcome with
  | Core.Outcome.Killed -> ("Killed", expected_reason result None)
  | Core.Outcome.Survived -> (
      match result.expected_reason with
      | None -> ("Survived", None)
      | Some reason ->
          ("Ignored", Some ("Expected survivor fulfilled: " ^ reason)))
  | Core.Outcome.Timeout when result.timeout_confirmed ->
      ("Timeout", expected_reason result None)
  | Core.Outcome.Timeout ->
      ( "RuntimeError",
        expected_reason result (Some "Inconclusive: timeout was not confirmed")
      )
  | Core.Outcome.Inconclusive reason ->
      ("RuntimeError", expected_reason result (Some ("Inconclusive: " ^ reason)))
  | Core.Outcome.Error reason ->
      let reason =
        if String.trim reason = "" then "ocaml-mutants reported an error"
        else reason
      in
      ("RuntimeError", expected_reason result (Some reason))

let location_json mutant =
  let range = Core.Mutant.range mutant in
  `Assoc
    [
      ( "start",
        `Assoc
          [
            ("line", `Int (Core.Source_range.start_line range));
            ("column", `Int (Core.Source_range.start_column range + 1));
          ] );
      ( "end",
        `Assoc
          [
            ("line", `Int (Core.Source_range.end_line range));
            ("column", `Int (Core.Source_range.end_column range + 1));
          ] );
    ]

let mutant_json entry =
  let mutant = mutant_of_entry entry in
  let status, reason =
    match entry with
    | Executed result -> status_and_reason result
    | Not_run _ -> ("Pending", Some "Not run by ocaml-mutants")
  in
  let fields =
    [
      ("id", `String (full_id mutant));
      ("mutatorName", `String (mutator_name mutant));
      ("replacement", `String (Core.Mutant.replacement mutant));
      ("location", location_json mutant);
      ("status", `String status);
    ]
  in
  let fields =
    match entry with
    | Not_run _ -> fields
    | Executed result ->
        fields
        @ [
            ( "duration",
              `Float
                (Core.Duration.to_seconds result.Run_store.duration *. 1000.) );
          ]
  in
  `Assoc
    (match reason with
    | None -> fields
    | Some reason -> fields @ [ ("statusReason", `String reason) ])

let validate_source ~path source entries =
  let source = Core.Source.of_string source in
  let actual_digest = Core.Source.digest source in
  let location_error ~mutant_id ~endpoint ~recorded derived =
    Source_location_mismatch
      {
        path;
        mutant_id;
        endpoint;
        recorded_line = Core.Location.line recorded;
        recorded_column = Core.Location.column recorded;
        derived_line = Core.Location.line derived;
        derived_column = Core.Location.column derived;
      }
  in
  let validate_location ~mutant_id ~endpoint ~byte ~recorded =
    match Core.Source.location_at_byte source ~byte with
    | None -> Error (Source_span_invalid { path; mutant_id })
    | Some derived when Core.Location.compare recorded derived = 0 -> Ok ()
    | Some derived ->
        Error (location_error ~mutant_id ~endpoint ~recorded derived)
  in
  let rec validate = function
    | [] -> Ok source
    | entry :: rest -> (
        let mutant = mutant_of_entry entry in
        let mutant_id = full_id mutant in
        let digest = Core.Mutant.source_digest mutant in
        if not (String.equal digest actual_digest) then
          Error
            (Source_digest_mismatch
               { path; expected = digest; actual = actual_digest })
        else
          let range = Core.Mutant.range mutant in
          match Core.Source.slice source range with
          | Error _ -> Error (Source_span_invalid { path; mutant_id })
          | Ok actual
            when not (String.equal actual (Core.Mutant.original mutant)) ->
              Error
                (Source_original_mismatch
                   {
                     path;
                     mutant_id;
                     expected = Core.Mutant.original mutant;
                     actual;
                   })
          | Ok _ -> (
              match
                validate_location ~mutant_id ~endpoint:Start
                  ~byte:(Core.Source_range.start_byte range)
                  ~recorded:(Core.Source_range.start_location range)
              with
              | Error _ as error -> error
              | Ok () -> (
                  match
                    validate_location ~mutant_id ~endpoint:End
                      ~byte:(Core.Source_range.end_byte range)
                      ~recorded:(Core.Source_range.end_location range)
                  with
                  | Error _ as error -> error
                  | Ok () -> validate rest)))
  in
  validate entries

let grouped_entries entries =
  let rec group groups current_path current = function
    | [] -> (
        match current_path with
        | None -> List.rev groups
        | Some path -> List.rev ((path, List.rev current) :: groups))
    | entry :: rest ->
        let path = Core.Mutant.path (mutant_of_entry entry) in
        if current_path = Some path then
          group groups current_path (entry :: current) rest
        else
          let groups =
            match current_path with
            | None -> groups
            | Some previous -> (previous, List.rev current) :: groups
          in
          group groups (Some path) [ entry ] rest
  in
  group [] None [] entries

module String_map = Map.Make (String)

let reject_duplicate_identities identities =
  let identities =
    List.sort
      (fun (left_path, left_id) (right_path, right_id) ->
        match String.compare left_path right_path with
        | 0 -> String.compare left_id right_id
        | value -> value)
      identities
  in
  let rec check seen = function
    | [] -> Ok ()
    | (path, mutant_id) :: rest -> (
        match String_map.find_opt mutant_id seen with
        | Some first_path ->
            Error
              (Duplicate_mutant_id
                 { mutant_id; first_path; duplicate_path = path })
        | None -> check (String_map.add mutant_id path seen) rest)
  in
  check String_map.empty identities

let reject_duplicate_ids entries =
  entries
  |> List.map (fun entry ->
      let mutant = mutant_of_entry entry in
      (Core.Mutant.path mutant, full_id mutant))
  |> reject_duplicate_identities

module For_testing = struct
  let reject_duplicate_identities = reject_duplicate_identities
end

let to_yojson ~thresholds ~read_source run =
  let entries = entries run in
  match reject_duplicate_ids entries with
  | Error _ as error -> error
  | Ok () ->
      let rec project files = function
        | [] ->
            Ok
              (`Assoc
                 [
                   ("schemaVersion", `String "2");
                   ( "thresholds",
                     `Assoc
                       [
                         ("high", `Int thresholds.high);
                         ("low", `Int thresholds.low);
                       ] );
                   ("files", `Assoc (List.rev files));
                 ])
        | (path, entries) :: rest -> (
            match read_source ~path with
            | Error reason -> Error (Source_unavailable { path; reason })
            | Ok contents -> (
                match validate_source ~path contents entries with
                | Error _ as error -> error
                | Ok _ ->
                    let file =
                      `Assoc
                        [
                          ("language", `String "ocaml");
                          ("source", `String contents);
                          ("mutants", `List (List.map mutant_json entries));
                        ]
                    in
                    project ((path, file) :: files) rest))
      in
      project [] (grouped_entries entries)

let to_string ~thresholds ~read_source run =
  Result.map
    (fun json -> Yojson.Safe.pretty_to_string ~std:true json ^ "\n")
    (to_yojson ~thresholds ~read_source run)
