open Util
module Core = Ocaml_mutants_core

type skip_reason =
  | Ghost_location
  | Generated_source
  | Test_source
  | Imprecise_mapping
  | Unsupported_expression
  | Duplicate

type skip_summary = {
  reason : skip_reason;
  count : int;
  examples : string list;
}

type discovery = { catalog : Core.Catalog.t; skipped : skip_summary list }

exception Operator_shadow_parity_failure of Error.t

module Skip_reason_map = Map.Make (struct
  type t = skip_reason

  let compare = Stdlib.compare
end)

module String_set = Set.Make (String)

let skip_reason_name = function
  | Ghost_location -> "ghost/generated location"
  | Generated_source -> "generated source"
  | Test_source -> "test or excluded source"
  | Imprecise_mapping -> "source mapping is not byte-exact"
  | Unsupported_expression -> "expression is not supported by enabled operators"
  | Duplicate -> "duplicate source transformation"

let unchecked_range_of_location location =
  Core.Source_range.make ~start_byte:location.Location.loc_start.Lexing.pos_cnum
    ~end_byte:location.loc_end.pos_cnum ~start_line:location.loc_start.pos_lnum
    ~start_column:(location.loc_start.pos_cnum - location.loc_start.pos_bol)
    ~end_line:location.loc_end.pos_lnum
    ~end_column:(location.loc_end.pos_cnum - location.loc_end.pos_bol)

let range_example path range =
  Format.asprintf "%s:%a" path Core.Source_range.pp range

let location_example path location =
  match unchecked_range_of_location location with
  | Ok range -> range_example path range
  | Error _ -> path

let summarize_skips skipped =
  let add summaries (reason, example) =
    let count, examples =
      Option.value
        (Skip_reason_map.find_opt reason summaries)
        ~default:(0, String_set.empty)
    in
    Skip_reason_map.add reason
      (count + 1, String_set.add example examples)
      summaries
  in
  List.fold_left add Skip_reason_map.empty skipped
  |> Skip_reason_map.bindings
  |> List.map (fun (reason, (count, examples)) ->
      { reason; count; examples = String_set.elements examples })
  |> List.sort (fun left right ->
      String.compare
        (skip_reason_name left.reason)
        (skip_reason_name right.reason))

let canonical_source_name ~root path =
  let absolute =
    if Filename.is_relative path then Filename.concat root path else path
  in
  let canonical =
    try Unix.realpath absolute
    with Unix.Unix_error _ -> Fpath.(v absolute |> normalize |> to_string)
  in
  Core.Mutant.normalize_path canonical

let source_file_matches ~root expected actual =
  let expected = canonical_source_name ~root expected in
  let actual = canonical_source_name ~root actual in
  if Sys.win32 then
    String.equal
      (String.lowercase_ascii expected)
      (String.lowercase_ascii actual)
  else String.equal expected actual

let exact_range_of_location ~root ~sourcefile ~source location =
  let* range = unchecked_range_of_location location in
  let start_matches =
    Core.Source.location_at_byte source
      ~byte:(Core.Source_range.start_byte range)
    = Some (Core.Source_range.start_location range)
  in
  let end_matches =
    Core.Source.location_at_byte source ~byte:(Core.Source_range.end_byte range)
    = Some (Core.Source_range.end_location range)
  in
  if
    start_matches && end_matches
    && source_file_matches ~root sourcefile
         location.Location.loc_start.pos_fname
    && source_file_matches ~root sourcefile location.loc_end.pos_fname
  then Ok range
  else Error "compiler location does not exactly describe the source bytes"

let substring source range =
  if
    Core.Source_range.start_byte range < 0
    || Core.Source_range.end_byte range > String.length source
    || Core.Source_range.start_byte range >= Core.Source_range.end_byte range
  then None
  else
    Some
      (String.sub source
         (Core.Source_range.start_byte range)
         (Core.Source_range.byte_length range))

let stable_range range =
  Printf.sprintf "%d:%d:%d:%d:%d:%d"
    (Core.Source_range.start_byte range)
    (Core.Source_range.end_byte range)
    (Core.Source_range.start_line range)
    (Core.Source_range.start_column range)
    (Core.Source_range.end_line range)
    (Core.Source_range.end_column range)

let shadow_fields ~raw_original mutant =
  [
    ("rule", Core.Operator.Rule.stable_name (Core.Mutant.rule mutant));
    ("path", Core.Mutant.path mutant);
    ("range", stable_range (Core.Mutant.range mutant));
    ("original", raw_original);
    ("replacement", Core.Mutant.replacement mutant);
    ("source_digest", Core.Mutant.source_digest mutant);
    ("full_id", Core.Mutant.Id.full (Core.Mutant.id mutant));
  ]

let render_shadow_fields fields =
  fields
  |> List.map (fun (name, value) -> Printf.sprintf "%s=%S" name value)
  |> String.concat "; "

let shadow_error ~kind ~path ~range ?decision ?legacy ?shadow message =
  let context =
    [ ("path", path); ("range", stable_range range) ]
    @ Option.fold ~none:[] ~some:(fun value -> [ ("decision", value) ]) decision
    @ Option.fold ~none:[] ~some:(fun value -> [ ("legacy", value) ]) legacy
    @ Option.fold ~none:[] ~some:(fun value -> [ ("spec", value) ]) shadow
  in
  Error.create ~phase:Error.Analysis ~cause:Error.Invariant_violation ~context
    "%s legacy/Spec shadow parity failed: %s" kind message

let boolean_shadow_error = shadow_error ~kind:"Boolean"
let connective_shadow_error = shadow_error ~kind:"Boolean connective"
let binary_shadow_error = shadow_error ~kind:"Binary operator"
let condition_shadow_error = shadow_error ~kind:"Condition negation"
let if_branch_shadow_error = shadow_error ~kind:"If branch"
let sequence_shadow_error = shadow_error ~kind:"Sequence deletion"
let return_shadow_error = shadow_error ~kind:"Return replacement"

let describe_shadow_decision = function
  | Core.Operator.Spec.Candidate { rule; _ } ->
      "candidate:" ^ Core.Operator.Rule.stable_name rule
  | Core.Operator.Spec.Rejection { rule; reason } ->
      Printf.sprintf "rejection:%s:%s"
        (Core.Operator.Rule.stable_name rule)
        (Core.Operator.Spec.rejection_name reason)

let shadow_decision_rule = function
  | Core.Operator.Spec.Candidate { rule; _ }
  | Core.Operator.Spec.Rejection { rule; _ } ->
      rule

let verify_shadow ~kind ~evaluate ~source ~path ~range
    ~(expression : Typedtree.expression)
    ?(target_expression : Typedtree.expression = expression) ~legacy () =
  let parity_error = shadow_error ~kind in
  let target_range = unchecked_range_of_location target_expression.exp_loc in
  match target_range with
  | Error message ->
      Error
        (parity_error ~path ~range
           ("typed expression has no valid source range: " ^ message))
  | Ok target_range when Core.Source_range.compare target_range range <> 0 ->
      Error
        (parity_error ~path ~range
           ~decision:
             (Printf.sprintf "typed-range:%s" (stable_range target_range))
           "legacy range is not owned by the typed expression witness")
  | Ok _ -> (
      match Core.Source.slice source range with
      | Error error ->
          Error
            (parity_error ~path ~range
               (Format.asprintf "cannot own the exact source slice: %a"
                  Core.Source.pp_error error))
      | Ok source_bytes -> (
          let decisions =
            evaluate ~source_bytes expression
            |> List.filter (fun decision ->
                Core.Operator.Rule.equal
                  (shadow_decision_rule decision)
                  (Core.Mutant.rule legacy))
          in
          match decisions with
          | [ (Core.Operator.Spec.Candidate { rule; plan } as decision) ] -> (
              let replacement =
                Core.Operator.Spec.Replacement_plan.replacement_bytes plan
              in
              match Core.Mutant.unchecked ~path ~range ~rule ~replacement with
              | Error error ->
                  Error
                    (parity_error ~path ~range
                       ~decision:(describe_shadow_decision decision)
                       (Format.asprintf
                          "Spec candidate is not constructible: %a"
                          Core.Mutant.pp_validation_error error))
              | Ok unchecked -> (
                  match Core.Mutant.validate ~source unchecked with
                  | Error error ->
                      Error
                        (parity_error ~path ~range
                           ~decision:(describe_shadow_decision decision)
                           (Format.asprintf
                              "Spec candidate does not validate against the \
                               source: %a"
                              Core.Mutant.pp_validation_error error))
                  | Ok shadow ->
                      let legacy_fields =
                        shadow_fields
                          ~raw_original:(Core.Mutant.original legacy)
                          legacy
                      in
                      let shadow_fields =
                        shadow_fields
                          ~raw_original:
                            (Core.Operator.Spec.Replacement_plan.source_bytes
                               plan)
                          shadow
                      in
                      if legacy_fields = shadow_fields then Ok ()
                      else
                        Error
                          (parity_error ~path ~range
                             ~decision:(describe_shadow_decision decision)
                             ~legacy:(render_shadow_fields legacy_fields)
                             ~shadow:(render_shadow_fields shadow_fields)
                             "ordered candidate fields differ")))
          | [] ->
              Error
                (parity_error ~path ~range ~decision:"not-applicable"
                   "legacy emitted a candidate but Spec emitted no event")
          | decisions ->
              Error
                (parity_error ~path ~range
                   ~decision:
                     (String.concat ","
                        (List.map describe_shadow_decision decisions))
                   "Spec did not emit exactly one candidate")))

let render_shadow_candidate_set fields =
  match fields with
  | [] -> "<none>"
  | fields -> String.concat " | " (List.map render_shadow_fields fields)

let field_value name fields = List.assoc_opt name fields

let duplicate_full_id fields =
  let rec find seen = function
    | [] -> None
    | candidate :: rest -> (
        match field_value "full_id" candidate with
        | Some full_id when List.mem full_id seen -> Some full_id
        | Some full_id -> find (full_id :: seen) rest
        | None -> find seen rest)
  in
  find [] fields

let candidate_ids fields = List.filter_map (field_value "full_id") fields

let ordered_difference_kind ~legacy_fields ~shadow_fields =
  let legacy_ids = candidate_ids legacy_fields in
  let shadow_ids = candidate_ids shadow_fields in
  let only_in left right =
    List.filter (fun value -> not (List.mem value right)) left
  in
  let legacy_only = only_in legacy_ids shadow_ids in
  let shadow_only = only_in shadow_ids legacy_ids in
  if legacy_only = [] && shadow_only <> [] then "spec-only"
  else if legacy_only <> [] && shadow_only = [] then "legacy-only"
  else if
    List.sort String.compare legacy_ids = List.sort String.compare shadow_ids
  then "order-mismatch"
  else if legacy_ids = shadow_ids then "field-mismatch"
  else "candidate-set-mismatch"

let materialize_shadow_event ~kind ~evaluate ~source ~path ~range
    ~(expression : Typedtree.expression)
    ?(target_expression : Typedtree.expression = expression) ~legacy () =
  let parity_error = shadow_error ~kind in
  let target_range = unchecked_range_of_location target_expression.exp_loc in
  match target_range with
  | Error message ->
      Error
        (parity_error ~path ~range
           ("typed expression has no valid source range: " ^ message))
  | Ok target_range when Core.Source_range.compare target_range range <> 0 ->
      Error
        (parity_error ~path ~range
           ~decision:
             (Printf.sprintf "typed-range:%s" (stable_range target_range))
           "legacy event range is not owned by the typed expression witness")
  | Ok _ -> (
      match Core.Source.slice source range with
      | Error error ->
          Error
            (parity_error ~path ~range
               (Format.asprintf "cannot own the exact source slice: %a"
                  Core.Source.pp_error error))
      | Ok source_bytes -> (
          let rec materialize reversed = function
            | [] -> Ok (List.rev reversed)
            | (Core.Operator.Spec.Rejection _ as decision) :: _ ->
                Error
                  (parity_error ~path ~range
                     ~decision:(describe_shadow_decision decision)
                     "Spec rejected a legacy typed visit event")
            | (Core.Operator.Spec.Candidate { rule; plan } as decision) :: rest
              -> (
                let replacement =
                  Core.Operator.Spec.Replacement_plan.replacement_bytes plan
                in
                match Core.Mutant.unchecked ~path ~range ~rule ~replacement with
                | Error error ->
                    Error
                      (parity_error ~path ~range
                         ~decision:(describe_shadow_decision decision)
                         (Format.asprintf
                            "Spec candidate is not constructible: %a"
                            Core.Mutant.pp_validation_error error))
                | Ok unchecked -> (
                    match Core.Mutant.validate ~source unchecked with
                    | Error error ->
                        Error
                          (parity_error ~path ~range
                             ~decision:(describe_shadow_decision decision)
                             (Format.asprintf
                                "Spec candidate does not validate against the \
                                 source: %a"
                                Core.Mutant.pp_validation_error error))
                    | Ok shadow ->
                        materialize
                          (( shadow,
                             shadow_fields
                               ~raw_original:
                                 (Core.Operator.Spec.Replacement_plan
                                  .source_bytes plan)
                               shadow )
                          :: reversed)
                          rest))
          in
          let legacy_fields =
            List.map
              (fun mutant ->
                shadow_fields ~raw_original:(Core.Mutant.original mutant) mutant)
              legacy
          in
          let decisions = evaluate ~source_bytes expression in
          match materialize [] decisions with
          | Error _ as error -> error
          | Ok materialized -> (
              let shadow_mutants, shadow_fields = List.split materialized in
              match duplicate_full_id legacy_fields with
              | Some full_id ->
                  Error
                    (parity_error ~path ~range ~decision:"duplicate-legacy"
                       ~legacy:(render_shadow_candidate_set legacy_fields)
                       ~shadow:(render_shadow_candidate_set shadow_fields)
                       (Printf.sprintf
                          "legacy event emitted duplicate full ID %s" full_id))
              | None -> (
                  match duplicate_full_id shadow_fields with
                  | Some full_id ->
                      Error
                        (parity_error ~path ~range ~decision:"duplicate-spec"
                           ~legacy:(render_shadow_candidate_set legacy_fields)
                           ~shadow:(render_shadow_candidate_set shadow_fields)
                           (Printf.sprintf
                              "Spec event emitted duplicate full ID %s" full_id))
                  | None ->
                      if legacy_fields = shadow_fields then Ok shadow_mutants
                      else
                        Error
                          (parity_error ~path ~range
                             ~decision:
                               (ordered_difference_kind ~legacy_fields
                                  ~shadow_fields)
                             ~legacy:(render_shadow_candidate_set legacy_fields)
                             ~shadow:(render_shadow_candidate_set shadow_fields)
                             "ordered candidate sets differ")))))

let verify_expression_shadow ~kind ~evaluate ~source ~path ~range ~expression
    ~legacy =
  verify_shadow ~kind ~evaluate ~source ~path ~range ~expression ~legacy ()

let verify_boolean_shadow =
  verify_expression_shadow ~kind:"Boolean"
    ~evaluate:Core.Operator.Spec.evaluate_boolean_literal

let verify_boolean_connective_shadow =
  verify_expression_shadow ~kind:"Boolean connective"
    ~evaluate:Core.Operator.Spec.evaluate_boolean_connective

let verify_binary_shadow =
  verify_expression_shadow ~kind:"Binary operator"
    ~evaluate:Core.Operator.Spec.evaluate_binary_operator

let verify_condition_shadow =
  verify_expression_shadow ~kind:"Condition negation"
    ~evaluate:Core.Operator.Spec.evaluate_condition_negation

let if_target_expression conditional target =
  match (conditional.Typedtree.exp_desc, target) with
  | ( Typedtree.Texp_ifthenelse (_, then_branch, Some _),
      Core.Operator.Spec.Then_branch ) ->
      Some then_branch
  | ( Typedtree.Texp_ifthenelse (_, _, Some else_branch),
      Core.Operator.Spec.Else_branch ) ->
      Some else_branch
  | _ -> None

let verify_if_branch_shadow ~source ~path ~range ~conditional ~target ~legacy =
  match if_target_expression conditional target with
  | None ->
      Error
        (if_branch_shadow_error ~path ~range
           "typed witness is not a full if expression with the requested target")
  | Some target_expression ->
      let evaluate ~source_bytes:_ conditional =
        Core.Operator.Spec.evaluate_if_branch
          ~source_bytes:(Core.Source.to_string source)
          ~target conditional
      in
      verify_shadow ~kind:"If branch" ~evaluate ~source ~path ~range
        ~expression:conditional ~target_expression ~legacy ()

let verify_sequence_shadow =
  verify_expression_shadow ~kind:"Sequence deletion"
    ~evaluate:Core.Operator.Spec.evaluate_sequence_deletion

let verify_return_shadow =
  verify_expression_shadow ~kind:"Return replacement"
    ~evaluate:Core.Operator.Spec.evaluate_return_replacement

let materialize_expression_shadow_event ~kind ~evaluate ~source ~path ~range
    ~expression ~legacy =
  materialize_shadow_event ~kind ~evaluate ~source ~path ~range ~expression
    ~legacy ()

let verify_materialized = Result.map (fun _ -> ())

let verify_expression_shadow_event ~kind ~evaluate ~source ~path ~range
    ~expression ~legacy =
  materialize_expression_shadow_event ~kind ~evaluate ~source ~path ~range
    ~expression ~legacy
  |> verify_materialized

let materialize_boolean_shadow_event =
  materialize_expression_shadow_event ~kind:"Boolean"
    ~evaluate:Core.Operator.Spec.evaluate_boolean_literal

let verify_boolean_shadow_event =
  verify_expression_shadow_event ~kind:"Boolean"
    ~evaluate:Core.Operator.Spec.evaluate_boolean_literal

let materialize_boolean_connective_shadow_event =
  materialize_expression_shadow_event ~kind:"Boolean connective"
    ~evaluate:Core.Operator.Spec.evaluate_boolean_connective

let verify_boolean_connective_shadow_event =
  verify_expression_shadow_event ~kind:"Boolean connective"
    ~evaluate:Core.Operator.Spec.evaluate_boolean_connective

let materialize_binary_shadow_event =
  materialize_expression_shadow_event ~kind:"Binary operator"
    ~evaluate:Core.Operator.Spec.evaluate_binary_operator

let verify_binary_shadow_event =
  verify_expression_shadow_event ~kind:"Binary operator"
    ~evaluate:Core.Operator.Spec.evaluate_binary_operator

let materialize_condition_shadow_event =
  materialize_expression_shadow_event ~kind:"Condition negation"
    ~evaluate:Core.Operator.Spec.evaluate_condition_negation

let verify_condition_shadow_event =
  verify_expression_shadow_event ~kind:"Condition negation"
    ~evaluate:Core.Operator.Spec.evaluate_condition_negation

let materialize_if_branch_shadow_event ~source ~path ~range ~conditional ~target
    ~legacy =
  match if_target_expression conditional target with
  | None ->
      Error
        (if_branch_shadow_error ~path ~range
           "typed witness is not a full if expression with the requested target")
  | Some target_expression ->
      let evaluate ~source_bytes:_ conditional =
        Core.Operator.Spec.evaluate_if_branch
          ~source_bytes:(Core.Source.to_string source)
          ~target conditional
      in
      materialize_shadow_event ~kind:"If branch" ~evaluate ~source ~path ~range
        ~expression:conditional ~target_expression ~legacy ()

let verify_if_branch_shadow_event ~source ~path ~range ~conditional ~target
    ~legacy =
  materialize_if_branch_shadow_event ~source ~path ~range ~conditional ~target
    ~legacy
  |> verify_materialized

let materialize_sequence_shadow_event =
  materialize_expression_shadow_event ~kind:"Sequence deletion"
    ~evaluate:Core.Operator.Spec.evaluate_sequence_deletion

let verify_sequence_shadow_event =
  verify_expression_shadow_event ~kind:"Sequence deletion"
    ~evaluate:Core.Operator.Spec.evaluate_sequence_deletion

let materialize_return_shadow_event =
  materialize_expression_shadow_event ~kind:"Return replacement"
    ~evaluate:Core.Operator.Spec.evaluate_return_replacement

let verify_return_shadow_event =
  verify_expression_shadow_event ~kind:"Return replacement"
    ~evaluate:Core.Operator.Spec.evaluate_return_replacement

let commit_return_shadow_event ~source ~path ~range ~expression ~legacy ~commit
    =
  match
    materialize_return_shadow_event ~source ~path ~range ~expression ~legacy
  with
  | Error _ as error -> error
  | Ok shadow ->
      commit shadow;
      Ok ()

module For_testing = struct
  let verify_boolean_shadow = verify_boolean_shadow
  let verify_boolean_connective_shadow = verify_boolean_connective_shadow
  let verify_binary_shadow = verify_binary_shadow
  let verify_condition_shadow = verify_condition_shadow
  let verify_if_branch_shadow = verify_if_branch_shadow
  let verify_sequence_shadow = verify_sequence_shadow
  let verify_return_shadow = verify_return_shadow
  let verify_boolean_shadow_event = verify_boolean_shadow_event

  let verify_boolean_connective_shadow_event =
    verify_boolean_connective_shadow_event

  let verify_binary_shadow_event = verify_binary_shadow_event
  let verify_condition_shadow_event = verify_condition_shadow_event
  let verify_if_branch_shadow_event = verify_if_branch_shadow_event
  let verify_sequence_shadow_event = verify_sequence_shadow_event
  let verify_return_shadow_event = verify_return_shadow_event
  let commit_return_shadow_event = commit_return_shadow_event
end

let operator_enabled operators operator = List.mem operator operators

let is_false_literal (expression : Typedtree.expression) =
  match expression.exp_desc with
  | Typedtree.Texp_construct (_, constructor, []) ->
      String.equal constructor.cstr_name "false"
      && Typedtree_compat.primitive ~environment:expression.exp_env
           expression.exp_type
         = Typedtree_compat.Bool
  | _ -> false

let direct_assert_false_child (expression : Typedtree.expression) =
  match expression.exp_desc with
  | Typedtree.Texp_assert (assertion, _) when is_false_literal assertion ->
      Some assertion
  | _ -> None

let return_replacements expression =
  Typedtree_compat.neutral_replacements_in
    ~environment:expression.Typedtree.exp_env expression.Typedtree.exp_type

let resolve_source ~root sourcefile =
  let candidate =
    if Filename.is_relative sourcefile then Filename.concat root sourcefile
    else sourcefile
  in
  if Sys.file_exists candidate && not (Sys.is_directory candidate) then
    Some candidate
  else None

let find_substring ~needle value =
  let rec search index =
    if index + String.length needle > String.length value then None
    else if String.sub value index (String.length needle) = needle then
      Some index
    else search (index + 1)
  in
  search 0

let exact_preprocessed_source ~root ~cmt_file sourcefile =
  let normalized = Core.Mutant.normalize_path sourcefile in
  let suffix = ".pp.ml" in
  if not (string_ends_with ~suffix normalized) then None
  else
    let original_relative =
      String.sub normalized 0 (String.length normalized - String.length suffix)
      ^ ".ml"
    in
    let original = Filename.concat root original_relative in
    let cmt = Core.Mutant.normalize_path cmt_file in
    match find_substring ~needle:"/default/" cmt with
    | None -> None
    | Some marker -> (
        let context_root =
          String.sub cmt 0 (marker + String.length "/default")
        in
        let generated = Filename.concat context_root normalized in
        match (read_file original, read_file generated) with
        | Ok original_bytes, Ok generated_bytes
          when String.equal original_bytes generated_bytes ->
            Some original
        | _ -> None)

let resolve_compiled_source ~root ~cmt_file sourcefile =
  match exact_preprocessed_source ~root ~cmt_file sourcefile with
  | Some _ as source -> source
  | None -> resolve_source ~root sourcefile

let artifact_example ~root cmt_file = normalize_relative ~root cmt_file

let discover_cmt_unchecked ~root ~selected_source ~operators cmt_file =
  let cmt_example = artifact_example ~root cmt_file in
  let _, cmt = Cmt_format.read cmt_file in
  match cmt with
  | None -> ([], [ (Generated_source, cmt_example) ])
  | Some information -> (
      match information.Cmt_format.cmt_annots with
      | Cmt_format.Implementation structure -> (
          match information.cmt_sourcefile with
          | None -> ([], [ (Generated_source, cmt_example) ])
          | Some sourcefile -> (
              match resolve_compiled_source ~root ~cmt_file sourcefile with
              | None -> ([], [ (Generated_source, cmt_example) ])
              | Some source_path -> (
                  let relative = normalize_relative ~root source_path in
                  if not (selected_source relative) then
                    ([], [ (Test_source, relative) ])
                  else
                    match read_file source_path with
                    | Error _ -> ([], [ (Generated_source, relative) ])
                    | Ok source ->
                        let source_value = Core.Source.of_string source in
                        let range_of_location =
                          exact_range_of_location ~root ~sourcefile
                            ~source:source_value
                        in
                        let mutants = ref [] in
                        let skipped = ref [] in
                        let skip ?location reason =
                          let example =
                            match location with
                            | None -> relative
                            | Some location ->
                                location_example relative location
                          in
                          skipped := (reason, example) :: !skipped
                        in
                        let add ?rule ~collect location operator replacement =
                          if not (operator_enabled operators operator) then ()
                          else if location.Location.loc_ghost then
                            skip ~location Ghost_location
                          else
                            match range_of_location location with
                            | Error _ -> skip ~location Imprecise_mapping
                            | Ok range -> (
                                match substring source range with
                                | None -> skip ~location Imprecise_mapping
                                | Some original -> (
                                    if String.trim original = "" then
                                      skip ~location Imprecise_mapping
                                    else
                                      let rule =
                                        match rule with
                                        | Some rule -> Ok rule
                                        | None ->
                                            Core.Operator.rule_for_replacement
                                              operator ~original ~replacement
                                      in
                                      match rule with
                                      | Error _ ->
                                          skip ~location Unsupported_expression
                                      | Ok rule -> (
                                          match
                                            Core.Mutant.unchecked ~path:relative
                                              ~range ~rule ~replacement
                                          with
                                          | Error _ ->
                                              skip ~location Imprecise_mapping
                                          | Ok unchecked -> (
                                              match
                                                Core.Mutant.validate
                                                  ~source:source_value unchecked
                                              with
                                              | Ok mutant -> collect mutant
                                              | Error _ ->
                                                  skip ~location
                                                    Imprecise_mapping))))
                        in
                        let add_event ?rule legacy location operator replacement
                            =
                          add ?rule
                            ~collect:(fun mutant -> legacy := mutant :: !legacy)
                            location operator replacement
                        in
                        let verify_event location legacy verify =
                          if location.Location.loc_ghost then ()
                          else
                            match range_of_location location with
                            | Error _ -> ()
                            | Ok range -> (
                                match substring source range with
                                | None -> ()
                                | Some original when String.trim original = ""
                                  ->
                                    ()
                                | Some _ -> (
                                    match verify range (List.rev !legacy) with
                                    | Ok shadow ->
                                        mutants :=
                                          List.rev_append shadow !mutants
                                    | Error error ->
                                        raise
                                          (Operator_shadow_parity_failure error)
                                    ))
                        in
                        let negate_condition condition =
                          if
                            operator_enabled operators
                              Core.Operator.Condition_negation
                          then
                            match
                              range_of_location condition.Typedtree.exp_loc
                            with
                            | Error _ ->
                                skip ~location:condition.exp_loc
                                  Imprecise_mapping
                            | Ok condition_range -> (
                                match substring source condition_range with
                                | Some original ->
                                    let legacy = ref [] in
                                    add_event legacy condition.exp_loc
                                      Core.Operator.Condition_negation
                                      ("Stdlib.not (" ^ original ^ ")");
                                    verify_event condition.exp_loc legacy
                                      (fun range legacy ->
                                        materialize_condition_shadow_event
                                          ~source:source_value ~path:relative
                                          ~range ~expression:condition ~legacy)
                                | None ->
                                    skip ~location:condition.exp_loc
                                      Imprecise_mapping)
                        in
                        let negate_guards cases =
                          List.iter
                            (fun case ->
                              Option.iter negate_condition
                                case.Typedtree.c_guard)
                            cases
                        in
                        let rec visit_expression iterator expression =
                          let location = expression.Typedtree.exp_loc in
                          (match expression.exp_desc with
                          | Typedtree.Texp_function (_, body) -> (
                              if
                                operator_enabled operators
                                  Core.Operator.Return_replacement
                              then
                                let add_body body_expression =
                                  match body_expression.Typedtree.exp_desc with
                                  | Typedtree.Texp_unreachable -> ()
                                  | _ ->
                                      let legacy = ref [] in
                                      let original =
                                        match
                                          range_of_location
                                            body_expression.Typedtree.exp_loc
                                        with
                                        | Ok range -> substring source range
                                        | Error _ -> None
                                      in
                                      return_replacements body_expression
                                      |> List.iter (fun replacement ->
                                          if original <> Some replacement then
                                            add_event legacy
                                              body_expression.exp_loc
                                              Core.Operator.Return_replacement
                                              replacement);
                                      verify_event body_expression.exp_loc
                                        legacy (fun range legacy ->
                                          materialize_return_shadow_event
                                            ~source:source_value ~path:relative
                                            ~range ~expression:body_expression
                                            ~legacy)
                                in
                                match body with
                                | Typedtree.Tfunction_body body_expression ->
                                    add_body body_expression
                                | Typedtree.Tfunction_cases { cases; _ } ->
                                    List.iter
                                      (fun case ->
                                        add_body case.Typedtree.c_rhs)
                                      cases)
                          | Typedtree.Texp_construct (_, constructor, []) ->
                              if
                                operator_enabled operators
                                  Core.Operator.Boolean_literal
                                && (constructor.cstr_name = "true"
                                   || constructor.cstr_name = "false")
                              then (
                                let legacy = ref [] in
                                if constructor.cstr_name = "true" then
                                  add_event legacy location
                                    Core.Operator.Boolean_literal "false"
                                else
                                  add_event legacy location
                                    Core.Operator.Boolean_literal "true";
                                verify_event location legacy
                                  (fun range legacy ->
                                    materialize_boolean_shadow_event
                                      ~source:source_value ~path:relative ~range
                                      ~expression ~legacy))
                          | Typedtree.Texp_ifthenelse
                              (condition, yes_branch, no_branch) -> (
                              negate_condition condition;
                              match no_branch with
                              | Some no_branch
                                when operator_enabled operators
                                       Core.Operator.If_branch -> (
                                  match
                                    ( range_of_location yes_branch.exp_loc,
                                      range_of_location no_branch.exp_loc )
                                  with
                                  | Ok yes_range, Ok no_range -> (
                                      (match substring source no_range with
                                      | Some replacement ->
                                          let legacy = ref [] in
                                          add_event
                                            ~rule:
                                              (Core.Operator.Spec.if_branch_rule
                                                 Core.Operator.Spec.Then_branch)
                                            legacy yes_branch.exp_loc
                                            Core.Operator.If_branch replacement;
                                          verify_event yes_branch.exp_loc legacy
                                            (fun range legacy ->
                                              materialize_if_branch_shadow_event
                                                ~source:source_value
                                                ~path:relative ~range
                                                ~conditional:expression
                                                ~target:
                                                  Core.Operator.Spec.Then_branch
                                                ~legacy)
                                      | None ->
                                          skip ~location:no_branch.exp_loc
                                            Imprecise_mapping);
                                      match substring source yes_range with
                                      | Some replacement ->
                                          let legacy = ref [] in
                                          add_event
                                            ~rule:
                                              (Core.Operator.Spec.if_branch_rule
                                                 Core.Operator.Spec.Else_branch)
                                            legacy no_branch.exp_loc
                                            Core.Operator.If_branch replacement;
                                          verify_event no_branch.exp_loc legacy
                                            (fun range legacy ->
                                              materialize_if_branch_shadow_event
                                                ~source:source_value
                                                ~path:relative ~range
                                                ~conditional:expression
                                                ~target:
                                                  Core.Operator.Spec.Else_branch
                                                ~legacy)
                                      | None ->
                                          skip ~location:yes_branch.exp_loc
                                            Imprecise_mapping)
                                  | _ -> skip ~location Imprecise_mapping)
                              | _ -> ())
                          | Typedtree.Texp_match
                              (_, computation_cases, effect_cases, _) ->
                              negate_guards computation_cases;
                              negate_guards effect_cases
                          | Typedtree.Texp_try (_, exception_cases, effect_cases)
                            ->
                              negate_guards exception_cases;
                              negate_guards effect_cases
                          | Typedtree.Texp_while (condition, _) ->
                              negate_condition condition
                          | Typedtree.Texp_sequence (_, right) -> (
                              if
                                operator_enabled operators
                                  Core.Operator.Sequence_deletion
                              then
                                match range_of_location right.exp_loc with
                                | Ok right_range -> (
                                    match substring source right_range with
                                    | Some replacement ->
                                        let legacy = ref [] in
                                        add_event legacy location
                                          Core.Operator.Sequence_deletion
                                          replacement;
                                        verify_event location legacy
                                          (fun range legacy ->
                                            materialize_sequence_shadow_event
                                              ~source:source_value
                                              ~path:relative ~range ~expression
                                              ~legacy)
                                    | None ->
                                        skip ~location:right.exp_loc
                                          Imprecise_mapping)
                                | Error _ ->
                                    skip ~location:right.exp_loc
                                      Imprecise_mapping)
                          | Typedtree.Texp_apply
                              ( {
                                  exp_desc =
                                    Typedtree.Texp_ident (path, _, description);
                                  exp_loc = operator_location;
                                  _;
                                },
                                arguments ) -> (
                              let legacy = ref [] in
                              let actual_arguments =
                                List.filter_map
                                  (fun (_, argument) ->
                                    match argument with
                                    | Typedtree.Arg expression ->
                                        Some expression
                                    | Typedtree.Omitted () -> None)
                                  arguments
                              in
                              if List.length actual_arguments = 2 then
                                match range_of_location operator_location with
                                | Error _ ->
                                    skip ~location:operator_location
                                      Imprecise_mapping
                                | Ok operator_range ->
                                    (match substring source operator_range with
                                    | None ->
                                        skip ~location:operator_location
                                          Imprecise_mapping
                                    | Some token -> (
                                        let token = String.trim token in
                                        match
                                          Core.Operator.Spec
                                          .find_binary_transition
                                            ~original:token
                                        with
                                        | None -> ()
                                        | Some transition -> (
                                            let rule =
                                              Core.Operator.Spec
                                              .binary_transition_rule transition
                                            in
                                            let operator =
                                              Core.Operator.Rule.family rule
                                            in
                                            if
                                              operator_enabled operators
                                                operator
                                              && Typedtree_compat
                                                 .is_resolved_stdlib_value ~path
                                                   ~description
                                                   ~environment:
                                                     expression.exp_env
                                                   ~name:token
                                              && Typedtree_compat
                                                 .operator_application_is_typed_in
                                                   ~environment:
                                                     expression.exp_env ~token
                                                   ~result_type:
                                                     expression.exp_type
                                                   ~argument_types:
                                                     (List.map
                                                        (fun argument ->
                                                          argument
                                                            .Typedtree.exp_type)
                                                        actual_arguments)
                                            then
                                              match
                                                range_of_location location
                                              with
                                              | Error _ ->
                                                  skip ~location
                                                    Imprecise_mapping
                                              | Ok _ -> (
                                                  match
                                                    List.map
                                                      (fun argument ->
                                                        match
                                                          range_of_location
                                                            argument
                                                              .Typedtree.exp_loc
                                                        with
                                                        | Ok range ->
                                                            substring source
                                                              range
                                                        | Error _ -> None)
                                                      actual_arguments
                                                  with
                                                  | [ Some left; Some right ] ->
                                                      let expression_replacement
                                                          =
                                                        Core.Operator.Spec
                                                        .render_binary_transition
                                                          transition ~left
                                                          ~right
                                                      in
                                                      add_event ~rule legacy
                                                        location operator
                                                        expression_replacement
                                                  | _ ->
                                                      skip ~location
                                                        Imprecise_mapping))));
                                    let evaluate ~source_bytes expression =
                                      Core.Operator.Spec
                                      .evaluate_boolean_connective ~source_bytes
                                        expression
                                      @ Core.Operator.Spec
                                        .evaluate_binary_operator ~source_bytes
                                          expression
                                      |> List.filter (fun decision ->
                                          operator_enabled operators
                                            (Core.Operator.Rule.family
                                               (shadow_decision_rule decision)))
                                    in
                                    verify_event location legacy
                                      (fun range legacy ->
                                        materialize_shadow_event
                                          ~kind:"Binary application" ~evaluate
                                          ~source:source_value ~path:relative
                                          ~range ~expression ~legacy ()))
                          | _ -> ());
                          match direct_assert_false_child expression with
                          | None ->
                              Tast_iterator.default_iterator.expr iterator
                                expression
                          | Some direct_false ->
                              let iterator =
                                {
                                  iterator with
                                  expr =
                                    (fun nested_iterator nested_expression ->
                                      if nested_expression == direct_false then
                                        Tast_iterator.default_iterator.expr
                                          nested_iterator nested_expression
                                      else
                                        visit_expression nested_iterator
                                          nested_expression);
                                }
                              in
                              Tast_iterator.default_iterator.expr iterator
                                expression
                        in
                        let iterator =
                          {
                            Tast_iterator.default_iterator with
                            expr = visit_expression;
                          }
                        in
                        iterator.structure iterator structure;
                        (List.rev !mutants, List.rev !skipped))))
      | _ -> ([], [ (Generated_source, cmt_example) ]))

let discover_cmt ~root ~selected_source ~operators cmt_file =
  try
    Ok (discover_cmt_unchecked ~root ~selected_source ~operators cmt_file)
  with
  | Operator_shadow_parity_failure error ->
      Error (Error.with_context "cmt" cmt_file error)
  | exn ->
      Error
        (Error.create ~phase:Error.Analysis ~cause:Error.Decode_failure
           ~context:[ ("cmt", cmt_file) ]
           "cannot decode compiler artifact: %s" (Printexc.to_string exn))

let compare_transformation left right =
  let compare next left_value right_value =
    if next <> 0 then next else String.compare left_value right_value
  in
  let compared_path =
    String.compare (Core.Mutant.path left) (Core.Mutant.path right)
  in
  let compared_range =
    if compared_path <> 0 then compared_path
    else
      Core.Source_range.compare (Core.Mutant.range left)
        (Core.Mutant.range right)
  in
  let compared_replacement =
    compare compared_range
      (Core.Mutant.replacement left)
      (Core.Mutant.replacement right)
  in
  compare compared_replacement
    (Core.Mutant.source_digest left)
    (Core.Mutant.source_digest right)

let deduplicate_transformations mutants =
  let compare_candidate left right =
    match compare_transformation left right with
    | 0 -> (
        match
          Core.Operator.Spec.compare_deduplication_precedence
            (Core.Mutant.family left) (Core.Mutant.family right)
        with
        | 0 -> Core.Mutant.compare left right
        | compared -> compared)
    | compared -> compared
  in
  let sorted = List.sort compare_candidate mutants in
  let rec collect previous unique duplicates = function
    | [] -> (List.rev unique, List.rev duplicates)
    | candidate :: rest -> (
        match previous with
        | Some previous when compare_transformation previous candidate = 0 ->
            collect (Some previous) unique (candidate :: duplicates) rest
        | _ -> collect (Some candidate) (candidate :: unique) duplicates rest)
  in
  collect None [] [] sorted

let discover ~root ~cmt_files ~selected_source ~operators =
  Compmisc.init_path ();
  let discovered =
    List.fold_left
      (fun accumulated cmt_file ->
        match accumulated with
        | Error _ as error -> error
        | Ok (all_mutants, all_skipped) -> (
            match discover_cmt ~root ~selected_source ~operators cmt_file with
            | Error error -> Error error
            | Ok (mutants, skipped) ->
                Ok
                  ( List.rev_append mutants all_mutants,
                    List.rev_append skipped all_skipped )))
      (Ok ([], []))
      cmt_files
  in
  let* mutants, skipped = discovered in
  let mutants, semantic_duplicates = deduplicate_transformations mutants in
  let skipped =
    List.fold_left
      (fun skipped mutant ->
        ( Duplicate,
          range_example (Core.Mutant.path mutant) (Core.Mutant.range mutant) )
        :: skipped)
      skipped semantic_duplicates
  in
  let catalog = Core.Catalog.of_list mutants in
  match catalog with
  | Error collision ->
      Error
        (Error.create ~phase:Error.Analysis ~cause:Error.Invariant_violation
           "cannot build mutant catalog: %a" Core.Catalog.pp_error collision)
  | Ok catalog ->
      let duplicates = Core.Catalog.exact_duplicates catalog in
      if duplicates > 0 then
        Error
          (Error.create ~phase:Error.Analysis ~cause:Error.Invariant_violation
             "transformation deduplication left %d exact catalog duplicates"
             duplicates)
      else Ok { catalog; skipped = summarize_skips skipped }

let instrument_files ~root catalog =
  let by_path = Hashtbl.create 32 in
  Core.Catalog.to_list catalog
  |> List.iter (fun mutant ->
      let previous =
        Option.value
          (Hashtbl.find_opt by_path (Core.Mutant.path mutant))
          ~default:[]
      in
      Hashtbl.replace by_path (Core.Mutant.path mutant) (mutant :: previous));
  let paths = Hashtbl.fold (fun path _ paths -> path :: paths) by_path [] in
  let rec instrument completed = function
    | [] -> Ok (List.rev completed)
    | path :: rest ->
        let absolute = Filename.concat root path in
        let* source =
          match read_file absolute with
          | Ok value -> Ok value
          | Error message ->
              Error (Error.make Error.Tool "cannot read %s: %s" path message)
        in
        let mutants = Hashtbl.find by_path path in
        let* instrumented =
          match
            Core.Instrumentation.instrument
              ~source:(Core.Source.of_string source)
              mutants
          with
          | Ok value -> Ok value
          | Error error ->
              Error
                (Error.make Error.Tool "cannot instrument %s: %a" path
                   Core.Instrumentation.pp_error error)
        in
        let* () =
          match write_file absolute instrumented with
          | Ok () -> Ok ()
          | Error message ->
              Error (Error.make Error.Tool "cannot write %s: %s" path message)
        in
        instrument (path :: completed) rest
  in
  instrument [] paths
