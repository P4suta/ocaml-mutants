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

exception Frontend_invariant_failure of Error.t

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

let operator_enabled operators operator = List.mem operator operators

let decision_rule = function
  | Core.Operator.Spec.Candidate { rule; _ }
  | Core.Operator.Spec.Rejection { rule; _ } ->
      rule

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
    (* Only the separators are normalized here: [Core.Mutant.normalize_path]
       drops empty components and would strip the leading slash from an absolute
       POSIX cmt path, breaking the context-root derivation. *)
    let cmt =
      String.map (function '\\' -> '/' | character -> character) cmt_file
    in
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
                        (* Single production path: evaluate the Spec registry at
                           a typed visit site, then validate and commit every
                           candidate. The gate order (ghost, range, slice,
                           whitespace) mirrors the historical writer so the skip
                           ledger stays stable; [attempts] preserves the
                           per-replacement skip multiplicity that return sites
                           report on gated locations. *)
                        let invariant ~range message error_pp error =
                          Error.create ~phase:Error.Analysis
                            ~cause:Error.Invariant_violation
                            ~context:
                              [
                                ("path", relative); ("range", stable_range range);
                              ]
                            "%s: %a" message error_pp error
                        in
                        let commit_event ?(attempts = fun () -> 1) location
                            evaluate =
                          let skip_gated reason =
                            for _ = 1 to attempts () do
                              skip ~location reason
                            done
                          in
                          if location.Location.loc_ghost then
                            skip_gated Ghost_location
                          else
                            match range_of_location location with
                            | Error _ -> skip_gated Imprecise_mapping
                            | Ok range -> (
                                match substring source range with
                                | None -> skip_gated Imprecise_mapping
                                | Some original when String.trim original = ""
                                  ->
                                    skip_gated Imprecise_mapping
                                | Some _
                                  when let start =
                                         Core.Source_range.start_byte range
                                       in
                                       start > 0
                                       && (source.[start - 1] = '~'
                                          || source.[start - 1] = '?') ->
                                    (* A punned labeled or optional argument
                                       (~name / ?name) admits no expression on
                                       the name's own byte range: replacing it
                                       in place would render the invalid
                                       ~(dispatch) form, so the site is reported
                                       as unsupported instead. *)
                                    skip_gated Unsupported_expression
                                | Some _ -> (
                                    match
                                      Core.Source.slice source_value range
                                    with
                                    | Error error ->
                                        raise
                                          (Frontend_invariant_failure
                                             (invariant ~range
                                                "typed visit site does not own \
                                                 its source slice"
                                                Core.Source.pp_error error))
                                    | Ok source_bytes ->
                                        List.iter
                                          (fun decision ->
                                            match decision with
                                            | Core.Operator.Spec.Rejection _ ->
                                                skip ~location Imprecise_mapping
                                            | Core.Operator.Spec.Candidate
                                                { rule; plan } -> (
                                                let replacement =
                                                  Core.Operator.Spec
                                                  .Replacement_plan
                                                  .replacement_bytes plan
                                                in
                                                match
                                                  Core.Mutant.unchecked
                                                    ~path:relative ~range ~rule
                                                    ~replacement
                                                with
                                                | Error error ->
                                                    raise
                                                      (Frontend_invariant_failure
                                                         (invariant ~range
                                                            "Spec candidate is \
                                                             not constructible"
                                                            Core.Mutant
                                                            .pp_validation_error
                                                            error))
                                                | Ok unchecked -> (
                                                    match
                                                      Core.Mutant.validate
                                                        ~source:source_value
                                                        unchecked
                                                    with
                                                    | Error error ->
                                                        raise
                                                          (Frontend_invariant_failure
                                                             (invariant ~range
                                                                "Spec \
                                                                 candidate \
                                                                 does not \
                                                                 validate \
                                                                 against the \
                                                                 source"
                                                                Core.Mutant
                                                                .pp_validation_error
                                                                error))
                                                    | Ok mutant ->
                                                        mutants :=
                                                          mutant :: !mutants)))
                                          (evaluate ~source_bytes)))
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
                                | Some _ ->
                                    commit_event condition.exp_loc
                                      (fun ~source_bytes ->
                                        Core.Operator.Spec
                                        .evaluate_condition_negation
                                          ~source_bytes condition)
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
                        let mutate_arms cases =
                          if operator_enabled operators Core.Operator.Match_arm
                          then
                            List.iter
                              (fun case ->
                                let arm = case.Typedtree.c_rhs in
                                match arm.Typedtree.exp_desc with
                                | Typedtree.Texp_unreachable -> ()
                                | _ ->
                                    commit_event arm.exp_loc
                                      (fun ~source_bytes ->
                                        Core.Operator.Spec.evaluate_match_arm
                                          ~source_bytes arm))
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
                                      let attempts () =
                                        let original =
                                          match
                                            range_of_location
                                              body_expression.Typedtree.exp_loc
                                          with
                                          | Ok range -> substring source range
                                          | Error _ -> None
                                        in
                                        return_replacements body_expression
                                        |> List.filter (fun replacement ->
                                            original <> Some replacement)
                                        |> List.length
                                      in
                                      commit_event ~attempts
                                        body_expression.exp_loc
                                        (fun ~source_bytes ->
                                          Core.Operator.Spec
                                          .evaluate_return_replacement
                                            ~source_bytes body_expression)
                                in
                                match body with
                                | Typedtree.Tfunction_body body_expression ->
                                    add_body body_expression
                                | Typedtree.Tfunction_cases { cases; _ } ->
                                    List.iter
                                      (fun case ->
                                        add_body case.Typedtree.c_rhs)
                                      cases)
                          | Typedtree.Texp_construct (_, constructor, arguments)
                            -> (
                              match arguments with
                              | [] ->
                                  if
                                    operator_enabled operators
                                      Core.Operator.Boolean_literal
                                    && (constructor.cstr_name = "true"
                                       || constructor.cstr_name = "false")
                                  then
                                    commit_event location (fun ~source_bytes ->
                                        Core.Operator.Spec
                                        .evaluate_boolean_literal ~source_bytes
                                          expression)
                              | _ :: _ ->
                                  if
                                    operator_enabled operators
                                      Core.Operator.Constructor_replacement
                                  then
                                    commit_event location (fun ~source_bytes ->
                                        Core.Operator.Spec
                                        .evaluate_constructor_replacement
                                          ~source_bytes expression))
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
                                      | Some _ ->
                                          commit_event yes_branch.exp_loc
                                            (fun ~source_bytes:_ ->
                                              Core.Operator.Spec
                                              .evaluate_if_branch
                                                ~source_bytes:source
                                                ~target:
                                                  Core.Operator.Spec.Then_branch
                                                expression)
                                      | None ->
                                          skip ~location:no_branch.exp_loc
                                            Imprecise_mapping);
                                      match substring source yes_range with
                                      | Some _ ->
                                          commit_event no_branch.exp_loc
                                            (fun ~source_bytes:_ ->
                                              Core.Operator.Spec
                                              .evaluate_if_branch
                                                ~source_bytes:source
                                                ~target:
                                                  Core.Operator.Spec.Else_branch
                                                expression)
                                      | None ->
                                          skip ~location:yes_branch.exp_loc
                                            Imprecise_mapping)
                                  | _ -> skip ~location Imprecise_mapping)
                              | _ -> ())
                          | Typedtree.Texp_match
                              (_, computation_cases, effect_cases, _) ->
                              negate_guards computation_cases;
                              negate_guards effect_cases;
                              mutate_arms computation_cases;
                              mutate_arms effect_cases
                          | Typedtree.Texp_try (_, exception_cases, effect_cases)
                            ->
                              negate_guards exception_cases;
                              negate_guards effect_cases;
                              mutate_arms exception_cases;
                              mutate_arms effect_cases
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
                                    | Some _ ->
                                        commit_event location
                                          (fun ~source_bytes ->
                                            Core.Operator.Spec
                                            .evaluate_sequence_deletion
                                              ~source_bytes expression)
                                    | None ->
                                        skip ~location:right.exp_loc
                                          Imprecise_mapping)
                                | Error _ ->
                                    skip ~location:right.exp_loc
                                      Imprecise_mapping)
                          | Typedtree.Texp_apply
                              ( {
                                  exp_desc = Typedtree.Texp_ident _;
                                  exp_loc = operator_location;
                                  _;
                                },
                                arguments ) -> (
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
                                    | Some _ -> ());
                                    commit_event location (fun ~source_bytes ->
                                        Core.Operator.Spec
                                        .evaluate_boolean_connective
                                          ~source_bytes expression
                                        @ Core.Operator.Spec
                                          .evaluate_binary_operator
                                            ~source_bytes expression
                                        |> List.filter (fun decision ->
                                            operator_enabled operators
                                              (Core.Operator.Rule.family
                                                 (decision_rule decision)))))
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
  | Frontend_invariant_failure error ->
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
