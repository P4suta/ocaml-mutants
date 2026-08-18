module Core = Ocaml_mutants_core
module Engine = Ocaml_mutants_engine

module Sexp_codec = Csexp.Make (struct
  type t = Engine.Sexp.t = Atom of string | List of t list
end)

let get_ok = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let range start_byte end_byte =
  get_ok
    (Core.Source_range.make ~start_byte ~end_byte ~start_line:1
       ~start_column:start_byte ~end_line:1 ~end_column:end_byte)

let rule name = get_ok (Core.Operator.Rule.of_stable_name name)

let mutant ?(path = "lib/example.ml") ?(rule_name = "true-to-false@1") ~source
    ~start_byte ~end_byte replacement =
  let source_value = Core.Source.of_string source in
  let unchecked =
    match
      Core.Mutant.unchecked ~path
        ~range:(range start_byte end_byte)
        ~rule:(rule rule_name) ~replacement
    with
    | Ok value -> value
    | Error error -> Alcotest.failf "%a" Core.Mutant.pp_validation_error error
  in
  match Core.Mutant.validate ~source:source_value unchecked with
  | Ok value -> value
  | Error error -> Alcotest.failf "%a" Core.Mutant.pp_validation_error error

let id mutant = Core.Mutant.Id.short (Core.Mutant.id mutant)

let test_refined_values () =
  Alcotest.(check bool)
    "NaN rejected" true
    (Result.is_error (Core.Duration.of_seconds Float.nan));
  Alcotest.(check bool)
    "infinity rejected" true
    (Result.is_error (Core.Duration.of_seconds Float.infinity));
  Alcotest.(check bool)
    "zero jobs rejected" true
    (Result.is_error (Core.Positive_int.of_int 0));
  Alcotest.(check bool)
    "empty argv rejected" true
    (Result.is_error (Core.Nonempty_argv.of_list []));
  Alcotest.(check bool)
    "negative byte span rejected" true
    (Result.is_error (Core.Byte_span.make ~start_byte:(-1) ~end_byte:1));
  Alcotest.(check bool)
    "zero-width byte span rejected" true
    (Result.is_error (Core.Byte_span.make ~start_byte:1 ~end_byte:1));
  Alcotest.(check bool)
    "zero line rejected" true
    (Result.is_error (Core.Location.make ~line:0 ~column:0));
  Alcotest.(check bool)
    "negative column rejected" true
    (Result.is_error (Core.Location.make ~line:1 ~column:(-1)));
  Alcotest.(check bool)
    "parent source path rejected" true
    (match
       Core.Mutant.unchecked ~path:"../escape.ml" ~range:(range 0 1)
         ~rule:(rule "return-zero@1") ~replacement:"0"
     with
    | Error (Core.Mutant.Invalid_path _) -> true
    | _ -> false);
  let source = Core.Source.of_string "α\r\nb\n" in
  let position byte =
    Core.Source.location_at_byte source ~byte
    |> Option.map (fun location ->
        (Core.Location.line location, Core.Location.column location))
  in
  Alcotest.(check (option (pair int int)))
    "UTF-8 columns are byte-based"
    (Some (1, 2))
    (position 2);
  Alcotest.(check (option (pair int int)))
    "CRLF advances once after LF"
    (Some (2, 0))
    (position 4);
  Alcotest.(check (option (pair int int)))
    "EOF has a source position"
    (Some (3, 0))
    (position 6);
  Alcotest.(check (option (pair int int)))
    "outside source has no position" None (position 7)

let test_stable_id () =
  let source = "true" in
  let left = mutant ~source ~start_byte:0 ~end_byte:4 "false" in
  let right =
    mutant ~path:"./lib\\example.ml" ~source ~start_byte:0 ~end_byte:4 "false"
  in
  Alcotest.(check string) "same normalized identity" (id left) (id right);
  Alcotest.(check int) "display prefix" 20 (String.length (id left));
  Alcotest.(check int)
    "full SHA-256" 64
    (String.length (Core.Mutant.Id.full (Core.Mutant.id left)))

let test_stable_id_changes () =
  let source = "true" in
  let left = mutant ~source ~start_byte:0 ~end_byte:4 "false" in
  let replacement = mutant ~source ~start_byte:0 ~end_byte:4 "not true" in
  let rule_changed =
    mutant ~rule_name:"negate-condition@1" ~source ~start_byte:0 ~end_byte:4
      "false"
  in
  Alcotest.(check bool)
    "replacement belongs to identity" true
    (id left <> id replacement);
  Alcotest.(check bool)
    "concrete rule belongs to identity" true
    (id left <> id rule_changed)

let test_user_value_operator_catalog () =
  let boundary =
    get_ok
      (Core.Operator.rule_for_replacement Core.Operator.Comparison ~original:"<"
         ~replacement:"<=")
  in
  Alcotest.(check string)
    "strict boundary relaxation" "lt-to-le@1"
    (Core.Operator.Rule.stable_name boundary);
  Alcotest.(check bool)
    "old complement rule removed" true
    (Result.is_error (Core.Operator.Rule.of_stable_name "lt-to-ge@1"));
  let multiplication =
    get_ok
      (Core.Operator.rule_for_replacement Core.Operator.Integer_arithmetic
         ~original:"*" ~replacement:"/")
  in
  Alcotest.(check string)
    "multiplicative replacement" "int-mul-to-div@1"
    (Core.Operator.Rule.stable_name multiplication);
  Alcotest.(check (list string))
    "option neutral return" [ "None" ]
    (Engine.Typedtree_compat.neutral_replacements
       (Predef.type_option Predef.type_int));
  Alcotest.(check (list string))
    "list neutral return" [ "[]" ]
    (Engine.Typedtree_compat.neutral_replacements
       (Predef.type_list Predef.type_int));
  Alcotest.(check bool)
    "integer plus type evidence" true
    (Engine.Typedtree_compat.operator_application_is_typed ~token:"+"
       ~result_type:Predef.type_int
       ~argument_types:[ Predef.type_int; Predef.type_int ]);
  Alcotest.(check bool)
    "float plus rejected as integer operator" false
    (Engine.Typedtree_compat.operator_application_is_typed ~token:"+"
       ~result_type:Predef.type_float
       ~argument_types:[ Predef.type_float; Predef.type_float ])

let operator_metadata rule =
  Printf.sprintf "%s|%s|%d|%s"
    (Core.Operator.Rule.stable_name rule)
    (Core.Operator.Family.name (Core.Operator.Rule.family rule))
    (Core.Operator.Rule.version rule)
    (Core.Operator.Profile.name (Core.Operator.Rule.profile rule))

let test_operator_registry_contract () =
  let rules = Core.Operator.Rule.all in
  let expected =
    [
      "true-to-false@1|boolean-literal|1|balanced";
      "false-to-true@1|boolean-literal|1|balanced";
      "negate-condition@1|condition-negation|1|balanced";
      "and-to-or@1|boolean-connective|1|balanced";
      "or-to-and@1|boolean-connective|1|balanced";
      "eq-to-neq@1|comparison|1|balanced";
      "neq-to-eq@1|comparison|1|balanced";
      "lt-to-le@1|comparison|1|balanced";
      "le-to-lt@1|comparison|1|balanced";
      "gt-to-ge@1|comparison|1|balanced";
      "ge-to-gt@1|comparison|1|balanced";
      "int-add-to-sub@1|integer-arithmetic|1|balanced";
      "int-sub-to-add@1|integer-arithmetic|1|balanced";
      "int-mul-to-div@1|integer-arithmetic|1|balanced";
      "int-div-to-mul@1|integer-arithmetic|1|balanced";
      "float-add-to-sub@1|float-arithmetic|1|balanced";
      "float-sub-to-add@1|float-arithmetic|1|balanced";
      "float-mul-to-div@1|float-arithmetic|1|balanced";
      "float-div-to-mul@1|float-arithmetic|1|balanced";
      "select-then-branch@1|if-branch|1|strong";
      "select-else-branch@1|if-branch|1|strong";
      "delete-left-sequence@1|sequence-deletion|1|all";
      "return-unit@1|return-replacement|1|balanced";
      "return-false@1|return-replacement|1|balanced";
      "return-true@1|return-replacement|1|balanced";
      "return-zero@1|return-replacement|1|balanced";
      "return-float-zero@1|return-replacement|1|balanced";
      "return-empty-string@1|return-replacement|1|balanced";
      "return-empty-list@1|return-replacement|1|balanced";
      "return-none@1|return-replacement|1|balanced";
      "match-arm-unit@1|match-arm|1|balanced";
      "match-arm-false@1|match-arm|1|balanced";
      "match-arm-true@1|match-arm|1|balanced";
      "match-arm-zero@1|match-arm|1|balanced";
      "match-arm-float-zero@1|match-arm|1|balanced";
      "match-arm-empty-string@1|match-arm|1|balanced";
      "match-arm-empty-list@1|match-arm|1|balanced";
      "match-arm-none@1|match-arm|1|balanced";
      "some-to-none@1|constructor-replacement|1|balanced";
      "cons-to-nil@1|constructor-replacement|1|balanced";
    ]
  in
  Alcotest.(check (list string))
    "frozen ordered rule metadata" expected
    (List.map operator_metadata rules);
  Alcotest.(check (list string))
    "frozen family order"
    [
      "boolean-literal";
      "condition-negation";
      "boolean-connective";
      "comparison";
      "integer-arithmetic";
      "float-arithmetic";
      "if-branch";
      "sequence-deletion";
      "return-replacement";
      "match-arm";
      "constructor-replacement";
    ]
    (List.map Core.Operator.Family.name Core.Operator.Family.all);
  Alcotest.(check (list string))
    "registry invariants" []
    (Core.Operator.Rule.For_testing.registry_errors ());
  let stable_names = List.map Core.Operator.Rule.stable_name rules in
  Alcotest.(check int)
    "stable names are unique" (List.length stable_names)
    (List.length (List.sort_uniq String.compare stable_names));
  Alcotest.(check bool)
    "versions are positive" true
    (List.for_all (fun rule -> Core.Operator.Rule.version rule > 0) rules);
  let compatibility_examples =
    Core.Operator.Rule.For_testing.compatibility_examples ()
  in
  Alcotest.(check (list string))
    "every rule has compatibility evidence" stable_names
    (List.map
       (fun (rule, _, _) -> Core.Operator.Rule.stable_name rule)
       compatibility_examples);
  List.iter
    (fun (expected_rule, original, replacement) ->
      match
        Core.Operator.rule_for_replacement
          (Core.Operator.Rule.family expected_rule)
          ~original ~replacement
      with
      | Ok actual_rule ->
          Alcotest.(check string)
            "compatibility resolution is unambiguous"
            (Core.Operator.Rule.stable_name expected_rule)
            (Core.Operator.Rule.stable_name actual_rule)
      | Error message -> Alcotest.fail message)
    compatibility_examples

let typed_expression_fixture environment source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf "operator-spec-fixture.ml";
  try Typecore.type_expression environment (Parse.expression lexbuf)
  with exn ->
    Alcotest.failf "cannot type fixed expression fixture %S: %s" source
      (Printexc.to_string exn)

let boolean_candidate_key rule source_bytes replacement_bytes =
  Printf.sprintf "candidate|%s|%S|%S"
    (Core.Operator.Rule.stable_name rule)
    source_bytes replacement_bytes

let boolean_rejection_key rule reason =
  Printf.sprintf "rejection|%s|%s" (Core.Operator.Rule.stable_name rule) reason

let legacy_boolean_literal_stream ~source_bytes expression =
  let transition =
    match expression.Typedtree.exp_desc with
    | Typedtree.Texp_construct (_, constructor, [])
      when String.equal constructor.cstr_name "true" ->
        Some ("true", "false")
    | Typedtree.Texp_construct (_, constructor, [])
      when String.equal constructor.cstr_name "false" ->
        Some ("false", "true")
    | _ -> None
  in
  match transition with
  | None -> []
  | Some (original, replacement) -> (
      let expected_rule =
        get_ok
          (Core.Operator.rule_for_replacement Core.Operator.Boolean_literal
             ~original ~replacement)
      in
      match
        Core.Operator.rule_for_replacement Core.Operator.Boolean_literal
          ~original:source_bytes ~replacement
      with
      | Ok rule -> [ boolean_candidate_key rule source_bytes replacement ]
      | Error _ ->
          [ boolean_rejection_key expected_rule "source-bytes-mismatch" ])

let spec_boolean_literal_stream ~definitions ~source_bytes expression =
  Core.Operator.Spec.For_testing.evaluate_expression ~definitions ~source_bytes
    expression
  |> List.filter_map (function
    | Core.Operator.Spec.Candidate { rule; plan } ->
        Some
          (boolean_candidate_key rule
             (Core.Operator.Spec.Replacement_plan.source_bytes plan)
             (Core.Operator.Spec.Replacement_plan.replacement_bytes plan))
    | Core.Operator.Spec.Rejection { rule; reason } ->
        Some
          (boolean_rejection_key rule
             (Core.Operator.Spec.rejection_name reason)))

let test_boolean_literal_spec_shadow () =
  Compmisc.init_path ();
  let environment = Compmisc.initial_env () in
  let typed source = typed_expression_fixture environment source in
  let typed_true = typed "true" in
  let typed_false = typed "false" in
  let true_to_false =
    get_ok
      (Core.Operator.rule_for_replacement Core.Operator.Boolean_literal
         ~original:"true" ~replacement:"false")
  in
  let definitions = Core.Operator.Spec.For_testing.boolean_literal_specs () in
  let spec_rules =
    List.map
      (fun definition ->
        Core.Operator.Spec.rule definition |> Core.Operator.Rule.stable_name)
      definitions
  in
  Alcotest.(check (list string))
    "only the two Boolean specs are shadowed"
    [ "true-to-false@1"; "false-to-true@1" ]
    spec_rules;
  Alcotest.(check bool)
    "shadow profiles remain Balanced" true
    (List.for_all
       (fun definition ->
         Core.Operator.Rule.profile (Core.Operator.Spec.rule definition)
         = Core.Operator.Profile.Balanced)
       definitions);
  let fixtures =
    [
      ("true", typed_true);
      ("false", typed_false);
      (" true ", typed_true);
      ("None", typed "None");
      ("Some true", typed "Some true");
      ("1", typed "1");
      ("false", typed_true);
      ("true", typed_false);
    ]
  in
  List.iteri
    (fun index (source_bytes, expression) ->
      Alcotest.(check (list string))
        (Printf.sprintf "legacy/spec stream parity fixture %d" index)
        (legacy_boolean_literal_stream ~source_bytes expression)
        (spec_boolean_literal_stream ~definitions ~source_bytes expression))
    fixtures;
  Alcotest.(check (list string))
    "source ownership accepts an exactly reparsed slice"
    [ boolean_candidate_key true_to_false " true " "false" ]
    (spec_boolean_literal_stream ~definitions ~source_bytes:" true " typed_true);
  Alcotest.(check (list string))
    "source ownership rejects a different parsed expression"
    [ boolean_rejection_key true_to_false "source-bytes-mismatch" ]
    (spec_boolean_literal_stream ~definitions ~source_bytes:"not true"
       typed_true);
  let candidate_plans expression source_bytes =
    Core.Operator.Spec.For_testing.evaluate_expression ~definitions
      ~source_bytes expression
    |> List.filter_map (function
      | Core.Operator.Spec.Candidate { plan; _ } -> Some plan
      | Core.Operator.Spec.Rejection _ -> None)
  in
  let plans =
    candidate_plans typed_true "true"
    @ candidate_plans typed_false "false"
    @ candidate_plans typed_true " true "
  in
  Alcotest.(check (list string))
    "semantic keys describe typed transformations"
    [
      "boolean-literal:true->false";
      "boolean-literal:false->true";
      "boolean-literal:true->false";
    ]
    (List.map Core.Operator.Spec.Replacement_plan.semantic_key plans);
  Alcotest.(check (list string))
    "validated plans own exact source bytes"
    [ "true"; "false"; " true " ]
    (List.map Core.Operator.Spec.Replacement_plan.source_bytes plans)

let test_decoded_mutant_revalidation () =
  let original = mutant ~source:"true" ~start_byte:0 ~end_byte:4 "false" in
  let decoded =
    match
      Core.Mutant.decoded
        ~path:(Core.Mutant.path original)
        ~range:(Core.Mutant.range original)
        ~rule:(Core.Mutant.rule original)
        ~original:(Core.Mutant.original original)
        ~replacement:"false"
        ~source_digest:(Core.Mutant.source_digest original)
        ~full_id:(Core.Mutant.Id.full (Core.Mutant.id original))
    with
    | Ok value -> value
    | Error error -> Alcotest.failf "%a" Core.Mutant.pp_validation_error error
  in
  Alcotest.(check bool)
    "same source validates" true
    (Result.is_ok
       (Core.Mutant.validate ~source:(Core.Source.of_string "true") decoded));
  Alcotest.(check bool)
    "changed source rejected" true
    (Result.is_error
       (Core.Mutant.validate ~source:(Core.Source.of_string "false") decoded))

let test_mutant_id_prefix_validation () =
  List.iter
    (fun prefix ->
      Alcotest.(check bool)
        ("valid prefix " ^ prefix) true
        (Core.Mutant.Id.is_valid_prefix prefix))
    [ "0"; "0123456789abcdef"; String.make 64 'f' ];
  List.iter
    (fun prefix ->
      Alcotest.(check bool)
        ("invalid prefix " ^ String.escaped prefix)
        false
        (Core.Mutant.Id.is_valid_prefix prefix))
    [ ""; "A"; "g"; "12-34"; String.make 65 'f' ]

let instrument source mutants =
  Core.Instrumentation.instrument ~source:(Core.Source.of_string source) mutants

let test_instrumentation_guard_shape () =
  let source = "true" in
  let candidate = mutant ~source ~start_byte:0 ~end_byte:4 "false" in
  match instrument source [ candidate ] with
  | Error error -> Alcotest.failf "%a" Core.Instrumentation.pp_error error
  | Ok rendered ->
      let expected =
        Printf.sprintf
          "module Ocaml_mutants_runtime_0 = struct\n\
          \  let active = Stdlib.Sys.getenv_opt \"OCAML_MUTANTS_ACTIVE\"\n\
           end\n\
           (match Ocaml_mutants_runtime_0.active with | None -> (true) | Some \
           %S -> (false) | Some _ -> (true))"
          (id candidate)
      in
      Alcotest.(check string)
        "original constrains replacements and unknown IDs fall back" expected
        rendered

let instrumentation_compiler () =
  let switch_prefix =
    Filename.dirname (Filename.dirname Config.standard_library)
  in
  Filename.concat
    (Filename.concat switch_prefix "bin")
    ("ocamlc" ^ Config.ext_exe)

let compile_instrumentation_fixture ~directory ~name source =
  let path = Filename.concat directory (name ^ ".ml") in
  (match Engine.Util.atomic_write path source with
  | Ok () -> ()
  | Error message -> Alcotest.fail message);
  Engine.Process_supervisor.run ~timeout:30. ~cwd:directory ~env:[]
    [ instrumentation_compiler (); "-c"; Filename.basename path ]

let test_instrumentation_original_first_type_inference () =
  let prefix =
    "type selected_record = { payload : int }\n\
     type decoy_record = { payload : int }\n\n\
     let selected : selected_record = { payload = 1 }\n\n\
     let choose () = "
  in
  let original = "selected" in
  let source = prefix ^ original ^ "\n" in
  let candidate =
    mutant ~rule_name:"return-zero@1" ~source ~start_byte:(String.length prefix)
      ~end_byte:(String.length prefix + String.length original)
      "{ payload = 2 }"
  in
  let rendered =
    match instrument source [ candidate ] with
    | Ok value -> value
    | Error error -> Alcotest.failf "%a" Core.Instrumentation.pp_error error
  in
  let old_replacement_first =
    Printf.sprintf
      "type selected_record = { payload : int }\n\
       type decoy_record = { payload : int }\n\n\
       let selected : selected_record = { payload = 1 }\n\n\
       let active : string option = None\n\n\
       let choose () =\n\
      \  match active with\n\
      \  | Some %S -> { payload = 2 }\n\
      \  | _ -> selected\n"
      (id candidate)
  in
  let directory = Filename.temp_dir "ocaml-mutants-instrumentation-" ".tmp" in
  Fun.protect
    ~finally:(fun () -> ignore (Engine.Util.remove_tree directory))
    (fun () ->
      let old_result =
        compile_instrumentation_fixture ~directory ~name:"replacement_first"
          old_replacement_first
      in
      (match old_result.status with
      | Engine.Process_supervisor.Exited 0 ->
          Alcotest.fail
            "replacement-first fixture unexpectedly resolved the selected \
             record type"
      | Engine.Process_supervisor.Exited _ -> ()
      | status ->
          Alcotest.failf "replacement-first compiler did not exit: %s\n%s"
            (Engine.Process_supervisor.status_string status)
            old_result.stderr);
      let new_result =
        compile_instrumentation_fixture ~directory ~name:"original_first"
          rendered
      in
      match new_result.status with
      | Engine.Process_supervisor.Exited 0 -> ()
      | status ->
          Alcotest.failf
            "original-first instrumentation did not type-check: %s\n%s"
            (Engine.Process_supervisor.status_string status)
            new_result.stderr)

let test_nested_instrumentation () =
  let source = "if true then false else true" in
  let outer =
    mutant ~rule_name:"negate-condition@1" ~source ~start_byte:3 ~end_byte:18
      "false"
  in
  let inner = mutant ~source ~start_byte:13 ~end_byte:18 "true" in
  match instrument source [ inner; outer ] with
  | Error error -> Alcotest.failf "%a" Core.Instrumentation.pp_error error
  | Ok result ->
      Alcotest.(check bool)
        "runtime read once" true
        (String.starts_with ~prefix:"module Ocaml_mutants_runtime_" result);
      Alcotest.(check bool)
        "preserves untouched body prefix" true
        (String.contains result 'i')

let test_same_range_and_permutation () =
  let source = "a + b" in
  let left =
    mutant ~rule_name:"int-add-to-sub@1" ~source ~start_byte:0 ~end_byte:5
      "a - b"
  in
  let right =
    mutant ~rule_name:"return-zero@1" ~source ~start_byte:0 ~end_byte:5 "0"
  in
  match
    (instrument source [ left; right ], instrument source [ right; left ])
  with
  | Ok left_render, Ok right_render ->
      Alcotest.(check string) "input order independent" left_render right_render
  | Error error, _ | _, Error error ->
      Alcotest.failf "%a" Core.Instrumentation.pp_error error

let test_runtime_module_freshness_uses_parsetree () =
  let prefix = "(* Ocaml_mutants_runtime_0 is only a comment. *)\n" in
  let source = prefix ^ "true" in
  let candidate =
    mutant ~source ~start_byte:(String.length prefix)
      ~end_byte:(String.length source) "false"
  in
  match instrument source [ candidate ] with
  | Error error -> Alcotest.failf "%a" Core.Instrumentation.pp_error error
  | Ok rendered ->
      Alcotest.(check bool)
        "comment does not consume module name" true
        (String.starts_with ~prefix:"module Ocaml_mutants_runtime_0 " rendered)

let test_crossing_rejected () =
  let source = "abcdefgh" in
  let left =
    mutant ~rule_name:"return-false@1" ~source ~start_byte:0 ~end_byte:5 "false"
  in
  let right =
    mutant ~rule_name:"return-true@1" ~source ~start_byte:3 ~end_byte:8 "true"
  in
  match instrument source [ left; right ] with
  | Error (Core.Interval_forest.Crossing_ranges _) -> ()
  | Error error ->
      Alcotest.failf "wrong error: %a" Core.Instrumentation.pp_error error
  | Ok _ -> Alcotest.fail "crossing ranges must be rejected"

let test_catalog_and_summary () =
  let first = mutant ~source:"true" ~start_byte:0 ~end_byte:4 "false" in
  let second =
    mutant ~rule_name:"false-to-true@1" ~source:"false" ~start_byte:0
      ~end_byte:5 "true"
  in
  let catalog =
    match Core.Catalog.of_list [ second; first; first ] with
    | Ok value -> value
    | Error error -> Alcotest.failf "%a" Core.Catalog.pp_error error
  in
  Alcotest.(check int) "unique" 2 (Core.Catalog.length catalog);
  Alcotest.(check int)
    "exact duplicates" 1
    (Core.Catalog.exact_duplicates catalog);
  let complete =
    match
      Core.Run_results.of_complete_list catalog
        [ (first, Core.Outcome.Killed); (second, Core.Outcome.Survived) ]
    with
    | Ok value -> value
    | Error error -> Alcotest.failf "%a" Core.Run_results.pp_error error
  in
  let summary = Core.Summary.of_results complete in
  Alcotest.(check int) "killed" 1 (Core.Summary.killed summary);
  Alcotest.(check int) "survived" 1 (Core.Summary.survived summary)

let test_catalog_distinguishes_collision_from_duplicate () =
  let first = mutant ~source:"true" ~start_byte:0 ~end_byte:4 "false" in
  let second =
    mutant ~rule_name:"negate-condition@1" ~source:"true" ~start_byte:0
      ~end_byte:4 "not true"
  in
  let same_short_id _ = "synthetic-collision" in
  let duplicates =
    Core.Catalog.For_testing.of_list_with_short_id same_short_id
      [ first; first ]
  in
  Alcotest.(check bool)
    "exact duplicate is accepted" true
    (match duplicates with
    | Ok catalog -> Core.Catalog.exact_duplicates catalog = 1
    | Error _ -> false);
  Alcotest.(check bool)
    "distinct identities collide" true
    (match
       Core.Catalog.For_testing.of_list_with_short_id same_short_id
         [ first; second ]
     with
    | Error (Core.Catalog.Hash_collision { id; _ }) ->
        String.equal id "synthetic-collision"
    | Ok _ -> false)

let test_config () =
  let text =
    {|version = 1
[mutation]
include = ["lib/**/*.ml"]
operators = ["comparison"]
[test]
command = ["dune", "runtest"]
timeout = 12.5
parallel_safe = true
[execution]
jobs = 3
[cache]
mode = "auto"
|}
  in
  match Engine.Config.parse ~file:"test.toml" text with
  | Error message -> Alcotest.fail message
  | Ok config ->
      Alcotest.(check int)
        "jobs" 3
        (Core.Positive_int.to_int config.execution.jobs);
      Alcotest.(check (float 0.001))
        "timeout" 12.5
        (Core.Duration.to_seconds (Option.get config.test.timeout));
      Alcotest.(check bool) "parallel safe" true config.test.parallel_safe;
      Alcotest.(check bool)
        "custom auto cache off" false
        (Engine.Config.cache_enabled config.cache.mode
           ~command:config.test.command)

let test_config_accumulates_errors () =
  let text =
    "version = 1\n[test]\ncommnad = []\ntimeout = -1.0\n[execution]\njobs = 0\n"
  in
  match Engine.Config.parse ~file:"test.toml" text with
  | Error message ->
      if not (String.contains message '3') then
        Alcotest.failf "unknown-key diagnostic lost line 3:\n%s" message;
      Alcotest.(check bool)
        "reports jobs too" true
        (String.ends_with ~suffix:"value must be positive" message)
  | Ok _ -> Alcotest.fail "all invalid fields must fail"

let test_config_stages_profiles_and_expectations () =
  let id = String.make 64 'a' in
  let text =
    Printf.sprintf
      {|version = 1
[mutation]
profile = "strong"

[[mutation.expect]]
id = %S
reason = "Equivalent because the fixture fixes this input."

[test]
parallel_safe = false

[[test.stages]]
name = "fast"
command = ["dune", "build", "@fast"]

[[test.stages]]
name = "full"
command = ["dune", "runtest", "--force"]
|}
      id
  in
  match Engine.Config.parse ~file:"stages.toml" text with
  | Error message -> Alcotest.fail message
  | Ok config ->
      Alcotest.(check string)
        "profile" "strong"
        (Core.Operator.Profile.name config.mutation.profile);
      Alcotest.(check int) "two stages" 2 (List.length config.test.stages);
      Alcotest.(check int)
        "three baseline runs by default" 3
        (Core.Positive_int.to_int config.test.baseline_runs);
      Alcotest.(check string)
        "full expectation ID" id (List.hd config.mutation.expectations).id;
      Alcotest.(check bool)
        "auto cache is proof-gated off" false
        (Engine.Config.cache_enabled Engine.Config.Auto
           ~command:config.test.command)

let test_csexp () =
  match Engine.Sexp.parse "(3:foo(3:bar0:))" with
  | Error message -> Alcotest.fail message
  | Ok parsed ->
      Alcotest.(check (list string))
        "atoms" [ "foo"; "bar"; "" ] (Engine.Sexp.atoms parsed)

let test_dune_workspace_decoder () =
  let open Engine.Sexp in
  let field name value = List [ Atom name; value ] in
  let module_ =
    List
      [
        field "name" (Atom "Fixture_math");
        field "impl" (List [ Atom "_build/default/lib/fixture_math.ml" ]);
        field "intf" (List []);
        field "cmt"
          (List
             [
               Atom
                 "_build/default/lib/.fixture_math.objs/byte/fixture_math.cmt";
             ]);
        field "cmti" (List []);
      ]
  in
  let payload =
    List
      [
        field "name" (Atom "fixture_math");
        field "uid" (Atom "digest");
        field "local" (Atom "true");
        field "requires" (List []);
        field "source_dir" (Atom "_build/default/lib");
        field "modules" (List [ module_ ]);
        field "include_dirs" (List []);
      ]
  in
  let encoded =
    Sexp_codec.to_string (List [ List [ Atom "library"; payload ] ])
  in
  let root = Unix.realpath "fixtures/basic" in
  match Engine.Dune_adapter.parse ~root encoded with
  | Error message -> Alcotest.fail message
  | Ok workspace ->
      Alcotest.(check (list string))
        "source path unmaps _build/default" [ "lib/fixture_math.ml" ]
        workspace.source_files;
      Alcotest.(check int) "cmt target" 1 (List.length workspace.cmt_targets)

let test_dune_tests_decoder_and_roles () =
  let open Engine.Sexp in
  let field name value = List [ Atom name; value ] in
  let encoded =
    Sexp_codec.to_string
      (List
         [
           List
             [
               field "name" (Atom "root_test");
               field "source_dir" (Atom ".");
               field "package" (List []);
               field "enabled" (Atom "true");
               field "location" (Atom "dune:1");
               field "target" (Atom "_build/default/root_test.exe");
             ];
         ])
  in
  let tests =
    match Engine.Dune_adapter.parse_tests encoded with
    | Ok tests -> tests
    | Error message -> Alcotest.fail message
  in
  Alcotest.(check int) "one enabled test" 1 (List.length tests);
  let workspace : Engine.Dune_adapter.workspace =
    {
      source_files = [ "subject.ml"; "root_test.ml" ];
      cmt_targets = [];
      targets =
        [
          {
            kind = Engine.Dune_adapter.Library;
            name = Some "subject";
            source_files = [ "subject.ml" ];
          };
          {
            kind = Engine.Dune_adapter.Executable;
            name = Some "root_test";
            source_files = [ "root_test.ml" ];
          };
        ];
    }
  in
  Alcotest.(check bool)
    "root test source classified exactly" true
    (Engine.Dune_adapter.source_role ~workspace ~tests "root_test.ml"
    = Engine.Dune_adapter.Test);
  Alcotest.(check bool)
    "root production source remains production" true
    (Engine.Dune_adapter.source_role ~workspace ~tests "subject.ml"
    = Engine.Dune_adapter.Production)

let test_report_codec () =
  let timeout_mutant =
    mutant ~source:"true" ~start_byte:0 ~end_byte:4 "false"
  in
  let run_id =
    get_ok (Core.Run_id.create ~started_at:"20260101T000000Z" ~nonce:"abc")
  in
  let command =
    get_ok (Core.Nonempty_argv.of_list [ "dune"; "runtest"; "--force" ])
  in
  let duration = get_ok (Core.Duration.of_seconds 1.) in
  let total_duration = get_ok (Core.Duration.of_seconds 2.) in
  let retry_attempt outcome output : Engine.Run_store.retry_attempt =
    {
      outcome;
      duration;
      stages = [];
      stdout = Engine.Run_store.captured output;
      stderr = Engine.Run_store.captured "";
    }
  in
  let fulfilled_mutant =
    mutant ~source:"true" ~start_byte:0 ~end_byte:4 "(false)"
  in
  let killed_mutant =
    mutant ~source:"true" ~start_byte:0 ~end_byte:4 "not true"
  in
  let inconclusive_mutant =
    mutant ~source:"true" ~start_byte:0 ~end_byte:4 "true && false"
  in
  let error_mutant =
    mutant ~source:"true" ~start_byte:0 ~end_byte:4
      "if true then false else true"
  in
  let ordinary_result mutant outcome expected_reason :
      Engine.Run_store.mutant_result =
    {
      mutant;
      outcome;
      duration;
      cached = false;
      stages = [];
      timeout_confirmed = false;
      timeout_retry = None;
      expected_reason = Some expected_reason;
      stdout = Engine.Run_store.captured "";
      stderr = Engine.Run_store.captured "";
    }
  in
  let run : Engine.Run_store.run =
    {
      metadata =
        {
          id = run_id;
          started_at = "2026-01-01T00:00:00Z";
          finished_at = "2026-01-01T00:00:01Z";
          workspace_digest = String.make 64 'a';
          toolchain = "ocaml=5.5; dune=3.24";
          profile = Core.Operator.Profile.All;
          selection = "all";
          test_command = command;
          baseline_duration = Some duration;
          baseline_stages =
            [
              {
                Engine.Run_store.name = "full";
                command;
                runs = [ duration ];
                slowest = duration;
              };
            ];
          timeout = Some duration;
          cache_mode = "auto";
          cache_key = String.make 64 'b';
        };
      status = Completed;
      results =
        [
          {
            mutant = timeout_mutant;
            outcome = Core.Outcome.Timeout;
            duration = total_duration;
            cached = false;
            stages = [];
            timeout_confirmed = true;
            timeout_retry =
              Some
                {
                  initial_timeout =
                    retry_attempt Core.Outcome.Timeout "initial timeout";
                  serial_retry =
                    retry_attempt Core.Outcome.Timeout "serial timeout";
                };
            expected_reason = Some "equivalent timeout";
            stdout = Engine.Run_store.captured "out";
            stderr = Engine.Run_store.captured "err";
          };
          ordinary_result fulfilled_mutant Core.Outcome.Survived "fulfilled";
          ordinary_result killed_mutant Core.Outcome.Killed "killed";
          ordinary_result inconclusive_mutant
            (Core.Outcome.Inconclusive "retry unavailable") "inconclusive";
          ordinary_result error_mutant (Core.Outcome.Error "spawn failed")
            "error";
        ];
      completeness = Engine.Run_store.Complete;
      expectations =
        [
          {
            Engine.Run_store.mutant_id =
              Core.Mutant.Id.full (Core.Mutant.id timeout_mutant);
            reason = "equivalent timeout";
            status = Engine.Run_store.Expectation_unfulfilled_confirmed_timeout;
          };
          {
            mutant_id = Core.Mutant.Id.full (Core.Mutant.id fulfilled_mutant);
            reason = "fulfilled";
            status = Engine.Run_store.Expectation_fulfilled;
          };
          {
            mutant_id = Core.Mutant.Id.full (Core.Mutant.id killed_mutant);
            reason = "killed";
            status = Engine.Run_store.Expectation_unfulfilled_killed;
          };
          {
            mutant_id = Core.Mutant.Id.full (Core.Mutant.id inconclusive_mutant);
            reason = "inconclusive";
            status =
              Engine.Run_store.Expectation_inconclusive "retry unavailable";
          };
          {
            mutant_id = Core.Mutant.Id.full (Core.Mutant.id error_mutant);
            reason = "error";
            status = Engine.Run_store.Expectation_error "spawn failed";
          };
          {
            mutant_id = String.make 64 'e';
            reason = "stale";
            status = Engine.Run_store.Expectation_stale;
          };
          {
            mutant_id = String.make 64 'f';
            reason = "partial selection";
            status = Engine.Run_store.Expectation_not_evaluated;
          };
        ];
      skipped =
        [
          {
            Engine.Run_store.reason = "source mapping is not byte-exact";
            count = 3;
            examples = [ "lib/a.ml:2:0-2:4"; "lib/b.ml" ];
          };
        ];
      warnings = [];
    }
  in
  let json = Engine.Run_store.run_to_yojson run in
  Alcotest.(check string)
    "document type" "ocaml-mutants.run-report-v1"
    Yojson.Safe.Util.(json |> member "document_type" |> to_string);
  Alcotest.(check string)
    "profile is independent metadata" "all"
    Yojson.Safe.Util.(json |> member "profile" |> to_string);
  (match Engine.Run_store.run_of_json json with
  | Error message -> Alcotest.fail message
  | Ok decoded ->
      Alcotest.(check string)
        "run ID round trip"
        (Core.Run_id.to_string run.metadata.id)
        (Core.Run_id.to_string decoded.metadata.id);
      Alcotest.(check string)
        "profile round trip"
        (Core.Operator.Profile.name run.metadata.profile)
        (Core.Operator.Profile.name decoded.metadata.profile);
      Alcotest.(check string)
        "successful report codec is lossless"
        (Yojson.Safe.to_string json)
        (Yojson.Safe.to_string (Engine.Run_store.run_to_yojson decoded)));
  let open Yojson.Safe.Util in
  let encoded_result = json |> member "mutants" |> index 0 in
  Alcotest.(check string)
    "initial timeout retained" "initial timeout"
    (encoded_result |> member "timeout_retry" |> member "initial_timeout"
   |> member "stdout" |> member "contents" |> to_string);
  Alcotest.(check string)
    "serial retry retained" "serial timeout"
    (encoded_result |> member "timeout_retry" |> member "serial_retry"
   |> member "stdout" |> member "contents" |> to_string);
  Alcotest.(check (list string))
    "all expectation states are explicit"
    [
      "unfulfilled-confirmed-timeout";
      "fulfilled";
      "unfulfilled-killed";
      "inconclusive";
      "error";
      "stale";
      "not-evaluated";
    ]
    (json |> member "expectations" |> to_list
    |> List.map (fun value -> value |> member "status" |> to_string));
  let encoded_skip = json |> member "skips" |> index 0 in
  Alcotest.(check int)
    "skip occurrence count retained" 3
    (encoded_skip |> member "count" |> to_int);
  Alcotest.(check (list string))
    "all distinct skip examples retained"
    [ "lib/a.ml:2:0-2:4"; "lib/b.ml" ]
    (encoded_skip |> member "examples" |> to_list |> List.map to_string);
  Alcotest.(check (list bool))
    "only fulfilled and partial-selection expectations are non-failures"
    [ true; false; true; true; true; true; false ]
    (List.map
       (fun (evaluation : Engine.Run_store.expectation_evaluation) ->
         Engine.Run_store.expectation_status_is_failure evaluation.status)
       run.expectations);
  let replace_member name replacement = function
    | `Assoc fields ->
        `Assoc
          (List.map
             (fun (key, value) ->
               if String.equal key name then (key, replacement) else (key, value))
             fields)
    | _ -> Alcotest.fail "expected a JSON object"
  in
  let mutate_first_result mutate = function
    | `Assoc fields ->
        `Assoc
          (List.map
             (function
               | "mutants", `List (first :: rest) ->
                   ("mutants", `List (mutate first :: rest))
               | field -> field)
             fields)
    | _ -> Alcotest.fail "expected a run report object"
  in
  let check_rejected label contradictory =
    match Engine.Run_store.run_of_json contradictory with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail label
  in
  check_rejected "unknown mutation profile was accepted"
    (replace_member "profile" (`String "custom") json);
  check_rejected "contradictory expected_survivor was accepted"
    (mutate_first_result (replace_member "expected_survivor" (`Bool true)) json);
  check_rejected "contradictory expectation status was accepted"
    (mutate_first_result
       (fun result ->
         let expectation = result |> member "expectation" in
         replace_member "expectation"
           (replace_member "status" (`String "fulfilled") expectation)
           result)
       json);
  check_rejected "contradictory expectation detail was accepted"
    (mutate_first_result
       (fun result ->
         let expectation = result |> member "expectation" in
         replace_member "expectation"
           (replace_member "detail" (`String "fabricated") expectation)
           result)
       json);
  let cleanup =
    Engine.Error.create ~phase:Engine.Error.Cleanup
      ~cause:Engine.Error.Io_failure
      ~context:[ ("path", "snapshot") ]
      "cleanup failed"
  in
  let primary =
    Engine.Error.create ~phase:Engine.Error.Analysis
      ~cause:Engine.Error.Decode_failure
      ~context:[ ("cmt", "x.cmt") ]
      "decode failed"
    |> fun error -> Engine.Error.suppress error cleanup
  in
  let failed =
    {
      run with
      metadata =
        {
          run.metadata with
          baseline_duration = None;
          baseline_stages = [];
          timeout = None;
        };
      status = Engine.Run_store.Failed primary;
      results = [];
      expectations = [];
    }
  in
  let encoded = Engine.Run_store.run_to_yojson failed in
  match Engine.Run_store.run_of_json encoded with
  | Error message -> Alcotest.fail message
  | Ok decoded ->
      Alcotest.(check string)
        "failure codec is lossless"
        (Yojson.Safe.to_string encoded)
        (Yojson.Safe.to_string (Engine.Run_store.run_to_yojson decoded))

let test_stryker_threshold_validation () =
  let check_invalid label expected ~high ~low =
    match Engine.Stryker_report.thresholds ~high ~low with
    | Error actual -> Alcotest.(check string) label expected actual
    | Ok _ -> Alcotest.failf "%s: invalid thresholds were accepted" label
  in
  check_invalid "negative high" "high threshold must be between 0 and 100"
    ~high:(-1) ~low:0;
  check_invalid "high above 100" "high threshold must be between 0 and 100"
    ~high:101 ~low:0;
  check_invalid "negative low" "low threshold must be between 0 and 100"
    ~high:100 ~low:(-1);
  check_invalid "low above 100" "low threshold must be between 0 and 100"
    ~high:100 ~low:101;
  check_invalid "low above high" "low threshold must not exceed high threshold"
    ~high:59 ~low:60;
  Alcotest.(check bool)
    "zero boundary accepted" true
    (Result.is_ok (Engine.Stryker_report.thresholds ~high:0 ~low:0));
  Alcotest.(check bool)
    "100 boundary accepted" true
    (Result.is_ok (Engine.Stryker_report.thresholds ~high:100 ~low:100))

let test_stryker_global_mutant_ids () =
  let mutant_id = String.make 64 'd' in
  let check identities =
    match
      Engine.Stryker_report.For_testing.reject_duplicate_identities identities
    with
    | Error
        (Engine.Stryker_report.Duplicate_mutant_id
           { mutant_id = actual; first_path; duplicate_path }) ->
        Alcotest.(check string) "full ID" mutant_id actual;
        Alcotest.(check string) "deterministic first path" "a.ml" first_path;
        Alcotest.(check string)
          "deterministic duplicate path" "z.ml" duplicate_path
    | Error error ->
        Alcotest.failf "unexpected uniqueness error: %a"
          Engine.Stryker_report.pp_error error
    | Ok () -> Alcotest.fail "the same full mutant ID was accepted across files"
  in
  check [ ("z.ml", mutant_id); ("a.ml", mutant_id) ];
  check [ ("a.ml", mutant_id); ("z.ml", mutant_id) ];
  Alcotest.(check bool)
    "different full IDs accepted" true
    (Result.is_ok
       (Engine.Stryker_report.For_testing.reject_duplicate_identities
          [ ("a.ml", String.make 64 'a'); ("z.ml", String.make 64 'b') ]))

let test_stryker_report_projection () =
  let source = "true" in
  let killed = mutant ~source ~start_byte:0 ~end_byte:4 "false" in
  let survived = mutant ~source ~start_byte:0 ~end_byte:4 "(true)" in
  let expected = mutant ~source ~start_byte:0 ~end_byte:4 "(true : bool)" in
  let timeout = mutant ~source ~start_byte:0 ~end_byte:4 "(not false)" in
  let unconfirmed =
    mutant ~source ~start_byte:0 ~end_byte:4 "(true || false)"
  in
  let inconclusive =
    mutant ~source ~start_byte:0 ~end_byte:4 "(true && true)"
  in
  let errored =
    mutant ~source ~start_byte:0 ~end_byte:4 "(if true then true else false)"
  in
  let not_run = mutant ~source ~start_byte:0 ~end_byte:4 "Stdlib.not false" in
  let duration = get_ok (Core.Duration.of_seconds 0.25) in
  let result ?expected_reason ?(timeout_confirmed = false) ?timeout_retry mutant
      outcome : Engine.Run_store.mutant_result =
    {
      mutant;
      outcome;
      duration;
      cached = false;
      stages = [];
      timeout_confirmed;
      timeout_retry;
      expected_reason;
      stdout = Engine.Run_store.captured "";
      stderr = Engine.Run_store.captured "";
    }
  in
  let results =
    let attempt outcome : Engine.Run_store.retry_attempt =
      {
        outcome;
        duration;
        stages = [];
        stdout = Engine.Run_store.captured "";
        stderr = Engine.Run_store.captured "";
      }
    in
    let confirmed_retry =
      {
        Engine.Run_store.initial_timeout = attempt Core.Outcome.Timeout;
        serial_retry = attempt Core.Outcome.Timeout;
      }
    in
    [
      result errored (Core.Outcome.Error "test process failed");
      result inconclusive (Core.Outcome.Inconclusive "confirmation failed");
      result ~timeout_confirmed:true ~timeout_retry:confirmed_retry timeout
        Core.Outcome.Timeout;
      result unconfirmed Core.Outcome.Timeout;
      result ~expected_reason:"equivalent by construction" expected
        Core.Outcome.Survived;
      result survived Core.Outcome.Survived;
      result killed Core.Outcome.Killed;
    ]
  in
  let run_id =
    get_ok (Core.Run_id.create ~started_at:"20260101T000000Z" ~nonce:"stryker")
  in
  let command = get_ok (Core.Nonempty_argv.of_list [ "dune"; "runtest" ]) in
  let run : Engine.Run_store.run =
    {
      metadata =
        {
          id = run_id;
          started_at = "2026-01-01T00:00:00Z";
          finished_at = "2026-01-01T00:00:01Z";
          workspace_digest = String.make 64 'a';
          toolchain = "ocaml=5.5; dune=3.24";
          profile = Core.Operator.Profile.All;
          selection = "all";
          test_command = command;
          baseline_duration = None;
          baseline_stages = [];
          timeout = Some duration;
          cache_mode = "off";
          cache_key = String.make 64 'b';
        };
      status = Engine.Run_store.Completed;
      results;
      completeness = Engine.Run_store.Partial [ not_run ];
      expectations =
        [
          {
            Engine.Run_store.mutant_id =
              Core.Mutant.Id.full (Core.Mutant.id expected);
            reason = "equivalent by construction";
            status = Engine.Run_store.Expectation_fulfilled;
          };
        ];
      skipped = [];
      warnings = [];
    }
  in
  let reads = ref 0 in
  let read_source ~path =
    incr reads;
    Alcotest.(check string) "normalized source path" "lib/example.ml" path;
    Ok source
  in
  let thresholds = get_ok (Engine.Stryker_report.thresholds ~high:80 ~low:60) in
  let projected =
    match Engine.Stryker_report.to_yojson ~thresholds ~read_source run with
    | Ok json -> json
    | Error error -> Alcotest.failf "%a" Engine.Stryker_report.pp_error error
  in
  Alcotest.(check int) "source read once per file" 1 !reads;
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "schema version" "2"
    (projected |> member "schemaVersion" |> to_string);
  Alcotest.(check int)
    "high threshold" 80
    (projected |> member "thresholds" |> member "high" |> to_int);
  Alcotest.(check int)
    "low threshold" 60
    (projected |> member "thresholds" |> member "low" |> to_int);
  let files = projected |> member "files" |> to_assoc in
  Alcotest.(check (list string))
    "deterministic file keys" [ "lib/example.ml" ] (List.map fst files);
  let projected_mutants =
    projected |> member "files" |> member "lib/example.ml" |> member "mutants"
    |> to_list
  in
  let projected_ids =
    List.map (fun json -> json |> member "id" |> to_string) projected_mutants
  in
  Alcotest.(check (list string))
    "mutants sorted by full ID"
    (List.sort String.compare projected_ids)
    projected_ids;
  let find mutant =
    let wanted = Core.Mutant.Id.full (Core.Mutant.id mutant) in
    List.find
      (fun json -> String.equal wanted (json |> member "id" |> to_string))
      projected_mutants
  in
  let check_status label expected mutant =
    Alcotest.(check string)
      label expected
      (find mutant |> member "status" |> to_string)
  in
  check_status "killed" "Killed" killed;
  check_status "unexpected survivor" "Survived" survived;
  check_status "expected survivor" "Ignored" expected;
  Alcotest.(check string)
    "expected survivor reason"
    "Expected survivor fulfilled: equivalent by construction"
    (find expected |> member "statusReason" |> to_string);
  check_status "confirmed timeout" "Timeout" timeout;
  check_status "unconfirmed timeout" "RuntimeError" unconfirmed;
  check_status "inconclusive" "RuntimeError" inconclusive;
  check_status "error" "RuntimeError" errored;
  check_status "not run" "Pending" not_run;
  Alcotest.(check int)
    "Stryker columns are one-based" 1
    (find killed |> member "location" |> member "start" |> member "column"
   |> to_int);
  let reordered = { run with results = List.rev run.results } in
  let read_source ~path:_ = Ok source in
  let reordered_json =
    match
      Engine.Stryker_report.to_yojson ~thresholds ~read_source reordered
    with
    | Ok json -> json
    | Error error -> Alcotest.failf "%a" Engine.Stryker_report.pp_error error
  in
  Alcotest.(check string)
    "projection is independent of execution order"
    (Yojson.Safe.to_string projected)
    (Yojson.Safe.to_string reordered_json);
  (match
     Engine.Stryker_report.to_yojson ~thresholds
       ~read_source:(fun ~path:_ -> Ok "false")
       run
   with
  | Error (Engine.Stryker_report.Source_digest_mismatch _) -> ()
  | Error error ->
      Alcotest.failf "unexpected projection error: %a"
        Engine.Stryker_report.pp_error error
  | Ok _ -> Alcotest.fail "projection accepted source with a different digest");
  let duplicate_reads = ref 0 in
  let duplicate_run =
    {
      run with
      results =
        [ result killed Core.Outcome.Killed; result killed Core.Outcome.Killed ];
      completeness = Engine.Run_store.Complete;
      expectations = [];
    }
  in
  (match
     Engine.Stryker_report.to_yojson ~thresholds
       ~read_source:(fun ~path:_ ->
         incr duplicate_reads;
         Ok source)
       duplicate_run
   with
  | Error
      (Engine.Stryker_report.Duplicate_mutant_id
         { mutant_id; first_path; duplicate_path }) ->
      Alcotest.(check string)
        "duplicate full ID"
        (Core.Mutant.Id.full (Core.Mutant.id killed))
        mutant_id;
      Alcotest.(check string) "duplicate first path" "lib/example.ml" first_path;
      Alcotest.(check string)
        "duplicate repeated path" "lib/example.ml" duplicate_path
  | Error error ->
      Alcotest.failf "unexpected duplicate error: %a"
        Engine.Stryker_report.pp_error error
  | Ok _ -> Alcotest.fail "projection accepted a duplicate full mutant ID");
  Alcotest.(check int)
    "duplicate IDs fail before reading source" 0 !duplicate_reads;
  let utf8_prefix = "α" in
  let unicode_source = utf8_prefix ^ "true" in
  let unicode_start = String.length utf8_prefix in
  let unicode_end = String.length unicode_source in
  let positioned_mutant ?(start_line = 1) ?(end_line = 1) ~start_column
      ~end_column () =
    let range =
      get_ok
        (Core.Source_range.make ~start_byte:unicode_start ~end_byte:unicode_end
           ~start_line ~start_column ~end_line ~end_column)
    in
    let unchecked =
      match
        Core.Mutant.unchecked ~path:"lib/unicode.ml" ~range
          ~rule:(rule "true-to-false@1") ~replacement:"false"
      with
      | Ok value -> value
      | Error error -> Alcotest.failf "%a" Core.Mutant.pp_validation_error error
    in
    match
      Core.Mutant.validate
        ~source:(Core.Source.of_string unicode_source)
        unchecked
    with
    | Ok value -> value
    | Error error -> Alcotest.failf "%a" Core.Mutant.pp_validation_error error
  in
  let unicode_mutant =
    positioned_mutant ~start_column:unicode_start ~end_column:unicode_end ()
  in
  let unicode_run =
    {
      run with
      results = [ result unicode_mutant Core.Outcome.Killed ];
      completeness = Engine.Run_store.Complete;
      expectations = [];
    }
  in
  let unicode_projection =
    match
      Engine.Stryker_report.to_yojson ~thresholds
        ~read_source:(fun ~path:_ -> Ok unicode_source)
        unicode_run
    with
    | Ok json -> json
    | Error error ->
        Alcotest.failf "valid UTF-8 projection failed: %a"
          Engine.Stryker_report.pp_error error
  in
  let unicode_location =
    unicode_projection |> member "files" |> member "lib/unicode.ml"
    |> member "mutants" |> index 0 |> member "location"
  in
  Alcotest.(check int)
    "UTF-8 start is a one-based byte column" (unicode_start + 1)
    (unicode_location |> member "start" |> member "column" |> to_int);
  Alcotest.(check int)
    "UTF-8 end is a one-based byte column" (unicode_end + 1)
    (unicode_location |> member "end" |> member "column" |> to_int);
  let check_location_mismatch ~label ~expected_endpoint mutant =
    let mismatch_run =
      { unicode_run with results = [ result mutant Core.Outcome.Killed ] }
    in
    match
      Engine.Stryker_report.to_yojson ~thresholds
        ~read_source:(fun ~path:_ -> Ok unicode_source)
        mismatch_run
    with
    | Error
        (Engine.Stryker_report.Source_location_mismatch
           {
             endpoint;
             recorded_line;
             recorded_column;
             derived_line;
             derived_column;
             _;
           }) ->
        Alcotest.(check bool)
          (label ^ " endpoint") true
          (endpoint = expected_endpoint);
        Alcotest.(check bool)
          (label ^ " reports unequal coordinates")
          true
          (recorded_line <> derived_line || recorded_column <> derived_column)
    | Error error ->
        Alcotest.failf "%s produced the wrong error: %a" label
          Engine.Stryker_report.pp_error error
    | Ok _ -> Alcotest.failf "%s was accepted" label
  in
  check_location_mismatch ~label:"wrong start coordinate"
    ~expected_endpoint:Engine.Stryker_report.Start
    (positioned_mutant ~start_column:(unicode_start - 1) ~end_column:unicode_end
       ());
  check_location_mismatch ~label:"wrong start line"
    ~expected_endpoint:Engine.Stryker_report.Start
    (positioned_mutant ~start_line:2 ~start_column:unicode_start
       ~end_column:unicode_end ());
  check_location_mismatch ~label:"wrong end coordinate"
    ~expected_endpoint:Engine.Stryker_report.End
    (positioned_mutant ~start_column:unicode_start ~end_column:(unicode_end - 1)
       ())

let test_pre_cancelled_process_does_not_spawn () =
  let cancel = Engine.Cancel.create () in
  Engine.Cancel.request cancel;
  let result =
    Engine.Process_supervisor.run ~cancel ~cwd:(Sys.getcwd ()) ~env:[]
      [ "ocaml-mutants-command-that-must-not-be-spawned" ]
  in
  match result.status with
  | Engine.Process_supervisor.Cancelled -> ()
  | status ->
      Alcotest.failf "pre-cancelled process returned %s"
        (Engine.Process_supervisor.status_string status)

let application_events = ref []

let record_application_event event =
  application_events := !application_events @ [ event ]

let application_signal_event operation signal =
  Printf.sprintf "signal-%s:%d" operation signal

module Fake_signals = struct
  type token = int * (unit -> unit) option

  let handlers = Hashtbl.create 2
  let install_attempts = ref []
  let restore_attempts = ref []
  let fail_install = ref None
  let fail_restore = ref []

  let install signal handler =
    record_application_event (application_signal_event "install" signal);
    install_attempts := !install_attempts @ [ signal ];
    if !fail_install = Some signal then failwith "injected install failure"
    else
      let previous = Hashtbl.find_opt handlers signal in
      Hashtbl.replace handlers signal handler;
      (signal, previous)

  let restore (signal, previous) =
    record_application_event (application_signal_event "restore" signal);
    restore_attempts := !restore_attempts @ [ signal ];
    if List.mem signal !fail_restore then failwith "injected restore failure"
    else
      match previous with
      | Some handler -> Hashtbl.replace handlers signal handler
      | None -> Hashtbl.remove handlers signal

  let trigger signal = (Hashtbl.find handlers signal) ()

  let reset () =
    Hashtbl.reset handlers;
    install_attempts := [];
    restore_attempts := [];
    fail_install := None;
    fail_restore := []
end

module Fake_application_services = struct
  module Signals = Fake_signals

  module Workspace = struct
    type snapshot = Snapshot

    type 'a bracket_outcome =
      | Acquisition_failed of Engine.Error.t
      | Action_returned of 'a * (unit, Engine.Error.t) result
      | Action_raised of
          exn * Printexc.raw_backtrace * (unit, Engine.Error.t) result

    let acquisition_error : Engine.Error.t option ref = ref None
    let cleanup_error : Engine.Error.t option ref = ref None

    let bracket _root action =
      record_application_event "snapshot-acquire";
      match !acquisition_error with
      | Some error -> Acquisition_failed error
      | None -> (
          let outcome =
            try `Returned (action Snapshot)
            with exception_ ->
              `Raised (exception_, Printexc.get_raw_backtrace ())
          in
          record_application_event "snapshot-cleanup";
          let cleanup =
            match !cleanup_error with
            | None -> Ok ()
            | Some error -> Error error
          in
          match outcome with
          | `Returned value -> Action_returned (value, cleanup)
          | `Raised (exception_, backtrace) ->
              Action_raised (exception_, backtrace, cleanup))

    let reset () =
      acquisition_error := None;
      cleanup_error := None
  end

  let clock_values = ref []
  let create_error : Engine.Error.t option ref = ref None
  let reserve_error : Engine.Error.t option ref = ref None
  let abandon_error : Engine.Error.t option ref = ref None
  let abandon_exception : exn option ref = ref None
  let commit_error : Engine.Error.t option ref = ref None
  let commit_exception : exn option ref = ref None
  let create_calls = ref 0
  let reserve_calls = ref 0
  let abandon_calls = ref 0
  let prepare_calls = ref 0
  let failure_draft_calls = ref 0
  let save_calls = ref 0
  let emit_calls = ref 0
  let observed_workspace : string option ref = ref None
  let observed_directory : string option ref = ref None
  let observed_started_at : string option ref = ref None
  let observed_finished_at : string option ref = ref None
  let observed_reservation : int option ref = ref None
  let trigger_signal_on_clock_value : string option ref = ref None

  let observed_report_status : Engine.Application.report_status option ref =
    ref None

  module Clock = struct
    let now () =
      match !clock_values with
      | value :: rest ->
          clock_values := rest;
          record_application_event ("clock:" ^ value);
          if !trigger_signal_on_clock_value = Some value then
            Fake_signals.trigger Sys.sigint;
          value
      | [] -> failwith "injected clock exhausted"
  end

  module Store = struct
    type t = Store
    type reservation = Reservation of int

    let create ?workspace ?directory () =
      record_application_event "store-create";
      incr create_calls;
      observed_workspace := workspace;
      observed_directory := directory;
      match !create_error with Some error -> Error error | None -> Ok Store

    let reserve Store ~started_at =
      record_application_event ("store-reserve:" ^ started_at);
      incr reserve_calls;
      match !reserve_error with
      | Some error -> Error error
      | None -> Ok (Reservation 17)

    let abandon_reservation Store (Reservation reservation) =
      record_application_event (Printf.sprintf "store-abandon:%d" reservation);
      incr abandon_calls;
      match !abandon_exception with
      | Some exception_ -> raise exception_
      | None -> (
          match !abandon_error with Some error -> Error error | None -> Ok ())

    let reservation_number (Reservation reservation) = reservation
  end

  type draft = unit

  let observed_cancel = ref false
  let observed_commit_cancel = ref false
  let observed_cancel_token : Engine.Cancel.t option ref = ref None
  let trigger_signal = ref None
  let trigger_signal_during_commit = ref None

  let prepare_verdict =
    ref
      (Engine.Application.Completed Engine.Application.All_detected
        : Engine.Application.verdict)

  let preparation_cleanup_errors : Engine.Error.t list ref = ref []
  let action_exception : exn option ref = ref None
  let failure_draft_exception : exn option ref = ref None

  let prepare_in_snapshot ~cancel ~store:_ ~reservation ~started_at ~root:_
      ~config:_ ~fresh:_ ~selection:_ ~output:_ ~snapshot:Workspace.Snapshot =
    record_application_event "action";
    incr prepare_calls;
    observed_started_at := Some started_at;
    observed_reservation := Some (Store.reservation_number reservation);
    observed_cancel_token := Some cancel;
    Option.iter Fake_signals.trigger !trigger_signal;
    observed_cancel := Engine.Cancel.is_requested cancel;
    match !action_exception with
    | Some exception_ -> raise exception_
    | None ->
        {
          Engine.Application.draft = ();
          verdict = !prepare_verdict;
          cleanup_errors = !preparation_cleanup_errors;
        }

  let prepare_failure ~cancel:_ ~store:_ ~reservation ~started_at ~root:_
      ~config:_ ~fresh:_ ~selection:_ ~output:_ error =
    record_application_event "failure-draft";
    incr failure_draft_calls;
    match !failure_draft_exception with
    | Some exception_ -> raise exception_
    | None ->
        observed_started_at := Some started_at;
        observed_reservation := Some (Store.reservation_number reservation);
        let verdict =
          match Engine.Error.cause error with
          | Engine.Error.Interrupted_by_user ->
              Engine.Application.Interrupted error
          | _ -> Engine.Application.Failed error
        in
        { Engine.Application.draft = (); verdict; cleanup_errors = [] }

  let commit_reserved ~store:_ ~reservation:_ ~finished_at ~resolution () =
    observed_finished_at := Some finished_at;
    observed_report_status := Some (Engine.Application.report_status resolution);
    record_application_event "report-save";
    incr save_calls;
    Option.iter Fake_signals.trigger !trigger_signal_during_commit;
    observed_commit_cancel :=
      Option.fold ~none:false ~some:Engine.Cancel.is_requested
        !observed_cancel_token;
    match !commit_exception with
    | Some exception_ -> raise exception_
    | None -> (
        match !commit_error with
        | Some error -> Error error
        | None ->
            record_application_event "report-emit";
            incr emit_calls;
            Ok resolution)

  let list_mutants ~cancel:_ ~root:_ ~config:_ ~selection:_ ~output:_ = Ok 0

  let reset () =
    application_events := [];
    Workspace.reset ();
    clock_values := [ "2026-01-01T00:00:00Z"; "2026-01-01T00:00:01Z" ];
    create_error := None;
    reserve_error := None;
    abandon_error := None;
    abandon_exception := None;
    commit_error := None;
    commit_exception := None;
    create_calls := 0;
    reserve_calls := 0;
    abandon_calls := 0;
    prepare_calls := 0;
    failure_draft_calls := 0;
    save_calls := 0;
    emit_calls := 0;
    observed_workspace := None;
    observed_directory := None;
    observed_started_at := None;
    observed_finished_at := None;
    observed_reservation := None;
    trigger_signal_on_clock_value := None;
    observed_report_status := None;
    observed_cancel := false;
    observed_commit_cancel := false;
    observed_cancel_token := None;
    trigger_signal := None;
    trigger_signal_during_commit := None;
    prepare_verdict :=
      Engine.Application.Completed Engine.Application.All_detected;
    preparation_cleanup_errors := [];
    action_exception := None;
    failure_draft_exception := None
end

module Fake_application = Engine.Application.Make (Fake_application_services)

let test_application_cancellation_and_signal_restoration () =
  let reset () =
    Fake_signals.reset ();
    Fake_application_services.reset ()
  in
  let invoke () =
    Fake_application.run ~root:"." ~config:Engine.Config.defaults ~fresh:true
      ~selection:Engine.Runner.All
      ~output:(Engine.Runner.Terminal { quiet = true; color = false })
  in
  let error_result label = function
    | Error error -> error
    | Ok code -> Alcotest.failf "%s returned %d" label code
  in
  let error_context label key error =
    match List.assoc_opt key (Engine.Error.context error) with
    | Some value -> value
    | None -> Alcotest.failf "%s omitted %s context" label key
  in
  let check_exception_evidence label ~operation ~report_state ~exception_ error
      =
    Alcotest.(check string)
      (label ^ " operation") operation
      (error_context label "operation" error);
    Alcotest.(check string)
      (label ^ " exception")
      (Printexc.to_string exception_)
      (error_context label "exception" error);
    ignore (error_context label "backtrace" error);
    Alcotest.(check string)
      (label ^ " report state") report_state
      (error_context label "authoritative_report_state" error)
  in
  let injected_error ~phase ~cause message =
    Engine.Error.create ~phase ~cause "%s" message
  in
  let committed_failure label =
    match !Fake_application_services.observed_report_status with
    | Some (Engine.Application.Report_failed error) -> error
    | Some Engine.Application.Report_completed ->
        Alcotest.failf "%s was committed as completed" label
    | Some Engine.Application.Report_interrupted ->
        Alcotest.failf "%s was committed as interrupted" label
    | None -> Alcotest.failf "%s was not committed" label
  in
  let check_single_commit label =
    Alcotest.(check int)
      (label ^ " save count") 1
      !Fake_application_services.save_calls;
    Alcotest.(check int)
      (label ^ " emit count") 1
      !Fake_application_services.emit_calls
  in

  reset ();
  (match invoke () with
  | Ok 0 -> ()
  | Ok code -> Alcotest.failf "application returned %d" code
  | Error error -> Alcotest.failf "%a" Engine.Error.pp error);
  Alcotest.(check bool)
    "successful action leaves cancellation clear" false
    !Fake_application_services.observed_cancel;
  Alcotest.(check (list int))
    "signals installed in declaration order"
    [ Sys.sigint; Sys.sigterm ]
    !Fake_signals.install_attempts;
  Alcotest.(check (list int))
    "handlers restored in reverse order"
    [ Sys.sigterm; Sys.sigint ]
    !Fake_signals.restore_attempts;
  Alcotest.(check int)
    "no handler leaked" 0
    (Hashtbl.length Fake_signals.handlers);
  Alcotest.(check (option string))
    "store receives workspace" (Some ".")
    !Fake_application_services.observed_workspace;
  Alcotest.(check (option string))
    "default cache directory remains absent" None
    !Fake_application_services.observed_directory;
  Alcotest.(check (option string))
    "clock value reserves run" (Some "2026-01-01T00:00:00Z")
    !Fake_application_services.observed_started_at;
  Alcotest.(check (option string))
    "finished timestamp follows cleanup" (Some "2026-01-01T00:00:01Z")
    !Fake_application_services.observed_finished_at;
  Alcotest.(check (option int))
    "reservation reaches engine" (Some 17)
    !Fake_application_services.observed_reservation;
  (match !Fake_application_services.observed_report_status with
  | Some Engine.Application.Report_completed -> ()
  | _ -> Alcotest.fail "successful run was not committed as completed");
  check_single_commit "successful run";
  Alcotest.(check int)
    "successful commit consumes rather than abandons reservation" 0
    !Fake_application_services.abandon_calls;
  Alcotest.(check (list string))
    "subscription remains active through save and emission"
    [
      application_signal_event "install" Sys.sigint;
      application_signal_event "install" Sys.sigterm;
      "store-create";
      "clock:2026-01-01T00:00:00Z";
      "store-reserve:2026-01-01T00:00:00Z";
      "snapshot-acquire";
      "action";
      "snapshot-cleanup";
      "clock:2026-01-01T00:00:01Z";
      "report-save";
      "report-emit";
      application_signal_event "restore" Sys.sigterm;
      application_signal_event "restore" Sys.sigint;
    ]
    !application_events;

  reset ();
  Fake_application_services.trigger_signal := None;
  Fake_application_services.trigger_signal_during_commit := Some Sys.sigint;
  (match invoke () with
  | Ok 0 -> ()
  | Ok code -> Alcotest.failf "commit interrupt probe returned %d" code
  | Error error -> Alcotest.failf "%a" Engine.Error.pp error);
  Alcotest.(check bool)
    "commit-time interrupt reaches the active cancellation token" true
    !Fake_application_services.observed_commit_cancel;
  Alcotest.(check (list int))
    "commit-time subscription deactivates only after publication"
    [ Sys.sigterm; Sys.sigint ]
    !Fake_signals.restore_attempts;
  Alcotest.(check int)
    "commit-time subscription leaves no active handler" 0
    (Hashtbl.length Fake_signals.handlers);

  reset ();
  Fake_application_services.trigger_signal := None;
  Fake_application_services.trigger_signal_on_clock_value :=
    Some "2026-01-01T00:00:01Z";
  let prepublication_interrupt =
    error_result "pre-publication interrupt" (invoke ())
  in
  Alcotest.(check int)
    "pre-publication interrupt exits 130" 130
    (Engine.Error.exit_code prepublication_interrupt);
  Alcotest.(check string)
    "pre-publication interrupt is authoritative"
    "run interrupted before report publication"
    (Engine.Error.message prepublication_interrupt);
  (match !Fake_application_services.observed_report_status with
  | Some Engine.Application.Report_interrupted -> ()
  | _ -> Alcotest.fail "pre-publication interrupt was not committed");
  check_single_commit "pre-publication interrupt";

  reset ();
  Fake_application_services.trigger_signal := None;
  let create_failure =
    injected_error ~phase:Engine.Error.Cache ~cause:Engine.Error.Io_failure
      "store create failure"
  in
  Fake_application_services.create_error := Some create_failure;
  let create_result = error_result "store create failure" (invoke ()) in
  Alcotest.(check string)
    "store creation error remains primary" "store create failure"
    (Engine.Error.message create_result);
  Alcotest.(check int)
    "reservation not acquired after create failure" 0
    !Fake_application_services.reserve_calls;
  Alcotest.(check int)
    "action not entered after create failure" 0
    !Fake_application_services.prepare_calls;
  Alcotest.(check int)
    "no report without draft" 0
    !Fake_application_services.save_calls;

  reset ();
  Fake_application_services.trigger_signal := None;
  let reserve_failure =
    injected_error ~phase:Engine.Error.Reporting ~cause:Engine.Error.Io_failure
      "store reserve failure"
  in
  Fake_application_services.reserve_error := Some reserve_failure;
  let reserve_result = error_result "store reserve failure" (invoke ()) in
  Alcotest.(check string)
    "reservation error remains primary" "store reserve failure"
    (Engine.Error.message reserve_result);
  Alcotest.(check int)
    "failed reservation is not abandoned" 0
    !Fake_application_services.abandon_calls;
  Alcotest.(check int)
    "failed reservation is not reported" 0
    !Fake_application_services.save_calls;

  reset ();
  Fake_application_services.trigger_signal := None;
  let snapshot_acquisition_failure =
    injected_error ~phase:Engine.Error.Snapshot
      ~cause:Engine.Error.Workspace_violation "snapshot acquisition failure"
  in
  Fake_application_services.Workspace.acquisition_error :=
    Some snapshot_acquisition_failure;
  let acquisition_result =
    error_result "snapshot acquisition failure" (invoke ())
  in
  Alcotest.(check string)
    "snapshot acquisition remains primary" "snapshot acquisition failure"
    (Engine.Error.message acquisition_result);
  Alcotest.(check int)
    "snapshot action not entered after acquisition failure" 0
    !Fake_application_services.prepare_calls;
  Alcotest.(check int)
    "acquisition failure creates one partial draft" 1
    !Fake_application_services.failure_draft_calls;
  check_single_commit "snapshot acquisition failure";
  Alcotest.(check int)
    "committed acquisition failure consumes reservation" 0
    !Fake_application_services.abandon_calls;
  Alcotest.(check (list string))
    "acquisition failure is reported before subscription deactivation"
    [
      application_signal_event "install" Sys.sigint;
      application_signal_event "install" Sys.sigterm;
      "store-create";
      "clock:2026-01-01T00:00:00Z";
      "store-reserve:2026-01-01T00:00:00Z";
      "snapshot-acquire";
      "failure-draft";
      "clock:2026-01-01T00:00:01Z";
      "report-save";
      "report-emit";
      application_signal_event "restore" Sys.sigterm;
      application_signal_event "restore" Sys.sigint;
    ]
    !application_events;

  reset ();
  Fake_signals.fail_install := Some Sys.sigterm;
  let install_error = error_result "partial install failure" (invoke ()) in
  Alcotest.(check string)
    "install failure remains primary" "cli"
    (Engine.Error.phase_name (Engine.Error.phase install_error));
  Alcotest.(check int)
    "no action after partial install" 0
    !Fake_application_services.prepare_calls;
  Alcotest.(check int)
    "no report after partial install" 0
    !Fake_application_services.save_calls;
  Alcotest.(check (list int))
    "partial install stops at failed acquisition"
    [ Sys.sigint; Sys.sigterm ]
    !Fake_signals.install_attempts;
  Alcotest.(check (list int))
    "partial install rolls back acquired handler" [ Sys.sigint ]
    !Fake_signals.restore_attempts;
  Alcotest.(check int)
    "process-lifetime rollback is total" 0
    (List.length (Engine.Error.suppressed install_error));

  reset ();
  Fake_application_services.trigger_signal := None;
  Fake_signals.fail_restore := [ Sys.sigterm ];
  let unsubscribe_contract =
    error_result "unsubscribe totality violation" (invoke ())
  in
  Alcotest.(check string)
    "nonconforming unsubscribe is an invariant" "invariant-violation"
    (Engine.Error.cause_name (Engine.Error.cause unsubscribe_contract));
  Alcotest.(check string)
    "unsubscribe contract is identified" "process-lifetime-unsubscribe-total"
    (error_context "unsubscribe contract" "contract" unsubscribe_contract);
  (match !Fake_application_services.observed_report_status with
  | Some Engine.Application.Report_completed -> ()
  | _ -> Alcotest.fail "unsubscribe violation changed the committed report");
  Alcotest.(check (list int))
    "all in-memory subscriptions deactivate after one violation"
    [ Sys.sigterm; Sys.sigint ]
    !Fake_signals.restore_attempts;
  check_single_commit "unsubscribe totality violation";

  reset ();
  Fake_application_services.trigger_signal := None;
  let primary =
    injected_error ~phase:Engine.Error.Execution
      ~cause:Engine.Error.Process_failure "primary action failure"
  in
  let snapshot_cleanup =
    injected_error ~phase:Engine.Error.Cleanup ~cause:Engine.Error.Io_failure
      "snapshot cleanup failure"
  in
  let preparation_cleanup =
    injected_error ~phase:Engine.Error.Cleanup ~cause:Engine.Error.Io_failure
      "preparation cleanup failure"
  in
  Fake_application_services.prepare_verdict := Engine.Application.Failed primary;
  Fake_application_services.preparation_cleanup_errors :=
    [ preparation_cleanup ];
  Fake_application_services.Workspace.cleanup_error := Some snapshot_cleanup;
  let combined = error_result "action and cleanup failures" (invoke ()) in
  let reported = committed_failure "action and cleanup failures" in
  Alcotest.(check string)
    "action error remains returned primary" "primary action failure"
    (Engine.Error.message combined);
  Alcotest.(check string)
    "action error remains report primary" "primary action failure"
    (Engine.Error.message reported);
  let check_suppressed label error =
    match Engine.Error.suppressed error with
    | [ preparation; snapshot ] ->
        Alcotest.(check string)
          (label ^ " preparation cleanup")
          "preparation cleanup failure"
          (Engine.Error.message preparation);
        Alcotest.(check string)
          (label ^ " snapshot cleanup")
          "snapshot cleanup failure"
          (Engine.Error.message snapshot)
    | errors ->
        Alcotest.failf "%s expected two cleanup errors, got %d" label
          (List.length errors)
  in
  check_suppressed "returned" combined;
  check_suppressed "reported" reported;
  check_single_commit "action and cleanup failures";
  Alcotest.(check int)
    "committed failed report consumes reservation" 0
    !Fake_application_services.abandon_calls;

  reset ();
  Fake_application_services.trigger_signal := Some Sys.sigint;
  let interruption =
    injected_error ~phase:Engine.Error.Execution
      ~cause:Engine.Error.Interrupted_by_user "interrupted action"
  in
  Fake_application_services.prepare_verdict :=
    Engine.Application.Interrupted interruption;
  let interrupted = error_result "interrupted action" (invoke ()) in
  Alcotest.(check bool)
    "signal requests the shared cancellation token" true
    !Fake_application_services.observed_cancel;
  Alcotest.(check string)
    "interruption remains primary" "interrupted action"
    (Engine.Error.message interrupted);
  Alcotest.(check int)
    "interruption retains exit 130" 130
    (Engine.Error.exit_code interrupted);
  (match !Fake_application_services.observed_report_status with
  | Some Engine.Application.Report_interrupted -> ()
  | _ -> Alcotest.fail "clean interruption was not reported as interrupted");
  check_single_commit "clean interruption";

  reset ();
  Fake_application_services.trigger_signal := None;
  Fake_application_services.action_exception := Some Exit;
  Fake_application_services.Workspace.cleanup_error := Some snapshot_cleanup;
  let action_exception =
    error_result "preparation exception partial report" (invoke ())
  in
  let reported_action_exception =
    committed_failure "preparation exception partial report"
  in
  check_exception_evidence "returned preparation exception"
    ~operation:"prepare-in-snapshot" ~report_state:"pending-partial-report"
    ~exception_:Exit action_exception;
  check_exception_evidence "reported preparation exception"
    ~operation:"prepare-in-snapshot" ~report_state:"pending-partial-report"
    ~exception_:Exit reported_action_exception;
  let check_action_cleanup label error =
    match Engine.Error.suppressed error with
    | [ snapshot ] ->
        Alcotest.(check string)
          (label ^ " snapshot cleanup")
          "snapshot cleanup failure"
          (Engine.Error.message snapshot)
    | errors ->
        Alcotest.failf "%s expected one cleanup error, got %d" label
          (List.length errors)
  in
  check_action_cleanup "returned preparation exception" action_exception;
  check_action_cleanup "reported preparation exception"
    reported_action_exception;
  Alcotest.(check (list int))
    "preparation exception runs every restorer"
    [ Sys.sigterm; Sys.sigint ]
    !Fake_signals.restore_attempts;
  Alcotest.(check int)
    "partial report consumes preparation exception reservation" 0
    !Fake_application_services.abandon_calls;
  Alcotest.(check int)
    "preparation exception builds one failure draft" 1
    !Fake_application_services.failure_draft_calls;
  check_single_commit "preparation exception partial report";
  Alcotest.(check (list string))
    "raised action keeps subscription through partial report publication"
    [
      application_signal_event "install" Sys.sigint;
      application_signal_event "install" Sys.sigterm;
      "store-create";
      "clock:2026-01-01T00:00:00Z";
      "store-reserve:2026-01-01T00:00:00Z";
      "snapshot-acquire";
      "action";
      "snapshot-cleanup";
      "failure-draft";
      "clock:2026-01-01T00:00:01Z";
      "report-save";
      "report-emit";
      application_signal_event "restore" Sys.sigterm;
      application_signal_event "restore" Sys.sigint;
    ]
    !application_events;

  reset ();
  Fake_application_services.trigger_signal := None;
  Fake_application_services.action_exception := Some Exit;
  Fake_application_services.Workspace.cleanup_error := Some snapshot_cleanup;
  Fake_application_services.failure_draft_exception := Some Not_found;
  let unavailable_reservation_cleanup =
    injected_error ~phase:Engine.Error.Cleanup ~cause:Engine.Error.Io_failure
      "reservation cleanup failure"
  in
  Fake_application_services.abandon_error :=
    Some unavailable_reservation_cleanup;
  let unavailable = error_result "failure draft exception" (invoke ()) in
  check_exception_evidence "failure draft primary"
    ~operation:"prepare-in-snapshot" ~report_state:"pending-partial-report"
    ~exception_:Exit unavailable;
  (match Engine.Error.suppressed unavailable with
  | [ snapshot; fallback; reservation ] ->
      Alcotest.(check string)
        "unreportable snapshot cleanup" "snapshot cleanup failure"
        (Engine.Error.message snapshot);
      check_exception_evidence "failure draft blocker"
        ~operation:"prepare-failure-draft" ~report_state:"unavailable-no-draft"
        ~exception_:Not_found fallback;
      Alcotest.(check string)
        "unreportable reservation cleanup" "reservation cleanup failure"
        (Engine.Error.message reservation)
  | errors ->
      Alcotest.failf "expected three unreportable cleanup errors, got %d"
        (List.length errors));
  Alcotest.(check int)
    "failed failure-draft construction is not committed" 0
    !Fake_application_services.save_calls;
  Alcotest.(check int)
    "failed failure-draft construction abandons reservation" 1
    !Fake_application_services.abandon_calls;

  reset ();
  Fake_application_services.trigger_signal := None;
  let reporting =
    injected_error ~phase:Engine.Error.Reporting ~cause:Engine.Error.Io_failure
      "report save failure"
  in
  let reservation_cleanup =
    injected_error ~phase:Engine.Error.Cleanup ~cause:Engine.Error.Io_failure
      "reservation cleanup failure"
  in
  Fake_application_services.commit_error := Some reporting;
  Fake_application_services.abandon_error := Some reservation_cleanup;
  let commit_failure = error_result "report commit failure" (invoke ()) in
  Alcotest.(check string)
    "save failure is primary" "report save failure"
    (Engine.Error.message commit_failure);
  (match Engine.Error.suppressed commit_failure with
  | [ cleanup ] ->
      Alcotest.(check string)
        "reservation cleanup failure is retained" "reservation cleanup failure"
        (Engine.Error.message cleanup)
  | errors ->
      Alcotest.failf "expected one reservation cleanup error, got %d"
        (List.length errors));
  Alcotest.(check int)
    "failed save attempted once" 1
    !Fake_application_services.save_calls;
  Alcotest.(check int)
    "failed save emits nothing" 0
    !Fake_application_services.emit_calls;
  Alcotest.(check int)
    "failed save abandons reservation once" 1
    !Fake_application_services.abandon_calls;

  reset ();
  Fake_application_services.trigger_signal := None;
  Fake_application_services.commit_exception := Some Exit;
  Fake_application_services.abandon_exception := Some Not_found;
  let commit_exception = error_result "commit exception" (invoke ()) in
  check_exception_evidence "commit exception" ~operation:"commit-reserved"
    ~report_state:"indeterminate" ~exception_:Exit commit_exception;
  (match Engine.Error.suppressed commit_exception with
  | [ cleanup ] ->
      Alcotest.(check string)
        "commit exception retains abandon exception"
        (Printexc.to_string Not_found)
        (error_context "commit abandon" "exception" cleanup)
  | errors ->
      Alcotest.failf "expected one commit cleanup error, got %d"
        (List.length errors));
  Alcotest.(check int)
    "commit exception attempts one save" 1
    !Fake_application_services.save_calls;
  Alcotest.(check int)
    "commit exception emits nothing" 0
    !Fake_application_services.emit_calls;
  Alcotest.(check int)
    "commit exception abandons reservation" 1
    !Fake_application_services.abandon_calls;
  Alcotest.(check (list string))
    "process-lifetime subscription covers one-shot commit"
    [
      application_signal_event "install" Sys.sigint;
      application_signal_event "install" Sys.sigterm;
      "store-create";
      "clock:2026-01-01T00:00:00Z";
      "store-reserve:2026-01-01T00:00:00Z";
      "snapshot-acquire";
      "action";
      "snapshot-cleanup";
      "clock:2026-01-01T00:00:01Z";
      "report-save";
      application_signal_event "restore" Sys.sigterm;
      application_signal_event "restore" Sys.sigint;
      "store-abandon:17";
    ]
    !application_events

let test_process_capture_and_timeout () =
  let executable = Unix.realpath Sys.executable_name in
  let capture_capacity = Engine.Process_supervisor.capture_capacity_bytes in
  let payload_bytes = capture_capacity + 1024 in
  let captured =
    Engine.Process_supervisor.run ~timeout:30. ~cwd:(Sys.getcwd ())
      ~env:[ ("OCAML_MUTANTS_TEST_CHILD", Some "large-output") ]
      [ executable ]
  in
  (match captured.status with
  | Engine.Process_supervisor.Exited 0 -> ()
  | status ->
      Alcotest.failf "large-output child returned %s"
        (Engine.Process_supervisor.status_string status));
  Alcotest.(check int) "stdout total bytes" payload_bytes captured.stdout_bytes;
  Alcotest.(check int) "stderr total bytes" payload_bytes captured.stderr_bytes;
  Alcotest.(check bool) "stdout truncated" true captured.stdout_truncated;
  Alcotest.(check bool) "stderr truncated" true captured.stderr_truncated;
  Alcotest.(check int)
    "stdout retained limit" capture_capacity
    (String.length captured.stdout);
  Alcotest.(check char) "stdout keeps head" 'H' captured.stdout.[0];
  Alcotest.(check char)
    "stdout keeps tail" 'T'
    captured.stdout.[String.length captured.stdout - 1];
  Alcotest.(check char) "stderr keeps head" 'E' captured.stderr.[0];
  Alcotest.(check char)
    "stderr keeps tail" 'R'
    captured.stderr.[String.length captured.stderr - 1];
  let timed_out =
    Engine.Process_supervisor.run ~timeout:0.05 ~cwd:(Sys.getcwd ())
      ~env:[ ("OCAML_MUTANTS_TEST_CHILD", Some "sleep") ]
      [ executable ]
  in
  (match timed_out.status with
  | Engine.Process_supervisor.Timed_out -> ()
  | status ->
      Alcotest.failf "sleeping child returned %s"
        (Engine.Process_supervisor.status_string status));
  let marker = Filename.temp_file "ocaml-mutants-grandchild-" ".pid" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove marker with Sys_error _ -> ())
    (fun () ->
      let tree =
        Engine.Process_supervisor.run ~timeout:0.2 ~cwd:(Sys.getcwd ())
          ~env:
            [
              ("OCAML_MUTANTS_TEST_CHILD", Some "grandchild");
              ("OCAML_MUTANTS_TEST_GRANDCHILD_PID", Some marker);
            ]
          [ executable ]
      in
      (match tree.status with
      | Engine.Process_supervisor.Timed_out -> ()
      | status ->
          Alcotest.failf "grandchild tree returned %s"
            (Engine.Process_supervisor.status_string status));
      let pid =
        match Engine.Util.read_file marker with
        | Ok value -> int_of_string (String.trim value)
        | Error message -> Alcotest.fail message
      in
      let started = Unix.gettimeofday () in
      while
        Engine.Process_supervisor.process_is_alive pid
        && Unix.gettimeofday () -. started < 2.
      do
        Unix.sleepf 0.02
      done;
      Alcotest.(check bool)
        "grandchild was terminated" false
        (Engine.Process_supervisor.process_is_alive pid))

let test_cache_never_returns_unproven_outcomes () =
  let directory = Filename.temp_file "ocaml-mutants-cache-test-" ".tmp" in
  Sys.remove directory;
  Unix.mkdir directory 0o700;
  Fun.protect
    ~finally:(fun () -> ignore (Engine.Util.remove_tree directory))
    (fun () ->
      let store =
        match Engine.Run_store.create ~directory () with
        | Ok store -> store
        | Error error -> Alcotest.failf "%a" Engine.Error.pp error
      in
      let source = Core.Source.of_string "true" in
      let candidate = mutant ~source:"true" ~start_byte:0 ~end_byte:4 "false" in
      let duration = get_ok (Core.Duration.of_seconds 0.25) in
      let result : Engine.Run_store.mutant_result =
        {
          mutant = candidate;
          outcome = Core.Outcome.Killed;
          duration;
          cached = false;
          stages = [];
          timeout_confirmed = false;
          timeout_retry = None;
          expected_reason = None;
          stdout = Engine.Run_store.captured "stdout";
          stderr = Engine.Run_store.captured "stderr";
        }
      in
      let key =
        Engine.Run_store.run_key [ "snapshot"; "argv"; "environment" ]
      in
      (match Engine.Run_store.save_mutant store ~key result with
      | Ok () -> ()
      | Error error -> Alcotest.failf "%a" Engine.Error.pp error);
      let exact =
        Engine.Run_store.load_mutant store ~key ~source ~expected:candidate
      in
      Alcotest.(check bool)
        "exact proof resumes" true
        (Option.fold ~none:false
           ~some:(fun result -> result.Engine.Run_store.cached)
           exact);
      Alcotest.(check bool)
        "changed source invalidates" true
        (Engine.Run_store.load_mutant store ~key
           ~source:(Core.Source.of_string "false")
           ~expected:candidate
        = None);
      let path =
        Filename.concat
          (Filename.concat (Filename.concat directory "o") key)
          (Core.Mutant.Id.short (Core.Mutant.id candidate) ^ ".json")
      in
      (match Engine.Util.write_file path "{broken" with
      | Ok () -> ()
      | Error message -> Alcotest.fail message);
      Alcotest.(check bool)
        "corrupt entry is a miss" true
        (Engine.Run_store.load_mutant store ~key ~source ~expected:candidate
        = None);
      let error_result =
        { result with outcome = Core.Outcome.Error "infrastructure" }
      in
      let error_key = Engine.Run_store.run_key [ "error" ] in
      (match Engine.Run_store.save_mutant store ~key:error_key error_result with
      | Ok () -> ()
      | Error error -> Alcotest.failf "%a" Engine.Error.pp error);
      Alcotest.(check bool)
        "errors are never cached" true
        (Engine.Run_store.load_mutant store ~key:error_key ~source
           ~expected:candidate
        = None);
      Alcotest.(check int) "full cache key" 64 (String.length key);
      Alcotest.(check bool)
        "key order is significant" true
        (key <> Engine.Run_store.run_key [ "environment"; "argv"; "snapshot" ]);
      Alcotest.(check bool)
        "key inputs are length framed" true
        (Engine.Run_store.run_key [ "a"; "b\000c" ]
        <> Engine.Run_store.run_key [ "a\000b"; "c" ]))

let test_cache_ownership_and_concurrency () =
  let parent = Filename.temp_file "ocaml-mutants-cache-roots-" ".tmp" in
  Sys.remove parent;
  Unix.mkdir parent 0o700;
  Fun.protect
    ~finally:(fun () -> ignore (Engine.Util.remove_tree parent))
    (fun () ->
      let workspace = Filename.concat parent "workspace" in
      let inside = Filename.concat workspace "cache" in
      let unowned = Filename.concat parent "unowned" in
      let cache = Filename.concat parent "cache" in
      List.iter
        (fun path -> Unix.mkdir path 0o700)
        [ workspace; inside; unowned; cache ];
      Alcotest.(check bool)
        "workspace cache rejected" true
        (Result.is_error
           (Engine.Run_store.create ~workspace ~directory:inside ()));
      (match
         Engine.Util.write_file (Filename.concat unowned "foreign") "data"
       with
      | Ok () -> ()
      | Error message -> Alcotest.fail message);
      Alcotest.(check bool)
        "unowned nonempty cache rejected" true
        (Result.is_error
           (Engine.Run_store.create ~workspace ~directory:unowned ()));
      let store =
        match Engine.Run_store.create ~workspace ~directory:cache () with
        | Ok store -> store
        | Error error -> Alcotest.failf "%a" Engine.Error.pp error
      in
      let source = Core.Source.of_string "true" in
      let candidate = mutant ~source:"true" ~start_byte:0 ~end_byte:4 "false" in
      let result : Engine.Run_store.mutant_result =
        {
          mutant = candidate;
          outcome = Core.Outcome.Killed;
          duration = get_ok (Core.Duration.of_seconds 0.1);
          cached = false;
          stages = [];
          timeout_confirmed = false;
          timeout_retry = None;
          expected_reason = None;
          stdout = Engine.Run_store.captured "";
          stderr = Engine.Run_store.captured "";
        }
      in
      let key = Engine.Run_store.run_key [ "concurrent" ] in
      let writers =
        List.init 4 (fun _ ->
            Domain.spawn (fun () ->
                Engine.Run_store.save_mutant store ~key result))
      in
      List.iter
        (fun writer ->
          match Domain.join writer with
          | Ok () -> ()
          | Error error -> Alcotest.failf "%a" Engine.Error.pp error)
        writers;
      Alcotest.(check bool)
        "concurrent entry remains decodable" true
        (Engine.Run_store.load_mutant store ~key ~source ~expected:candidate
        <> None))

let test_snapshot_manifest_and_cleanup () =
  let workspace = Filename.temp_file "ocaml-mutants-workspace-" ".tmp" in
  Sys.remove workspace;
  Unix.mkdir workspace 0o700;
  Fun.protect
    ~finally:(fun () -> ignore (Engine.Util.remove_tree workspace))
    (fun () ->
      let source_path = Filename.concat workspace "subject.ml" in
      (match Engine.Util.write_file source_path "let value = 1\r\n" with
      | Ok () -> ()
      | Error message -> Alcotest.fail message);
      let snapshot_path = ref None in
      let first_digest =
        match
          Engine.Workspace_snapshot.with_snapshot workspace (fun snapshot ->
              let copied = Engine.Workspace_snapshot.root snapshot in
              snapshot_path := Some copied;
              let copied_source =
                match
                  Engine.Util.read_file (Filename.concat copied "subject.ml")
                with
                | Ok value -> value
                | Error message -> Alcotest.fail message
              in
              Alcotest.(check string)
                "snapshot preserves CRLF bytes" "let value = 1\r\n"
                copied_source;
              Ok (Engine.Workspace_snapshot.manifest_digest snapshot))
        with
        | Ok digest -> digest
        | Error error -> Alcotest.failf "%a" Engine.Error.pp error
      in
      Alcotest.(check bool)
        "snapshot removed after success" false
        (Sys.file_exists (Option.get !snapshot_path));
      (match Engine.Util.write_file source_path "let value = 2\r\n" with
      | Ok () -> ()
      | Error message -> Alcotest.fail message);
      let second_digest =
        match
          Engine.Workspace_snapshot.with_snapshot workspace (fun snapshot ->
              Ok (Engine.Workspace_snapshot.manifest_digest snapshot))
        with
        | Ok digest -> digest
        | Error error -> Alcotest.failf "%a" Engine.Error.pp error
      in
      Alcotest.(check bool)
        "file bytes invalidate manifest" true
        (first_digest <> second_digest);
      let failed_snapshot = ref None in
      let primary =
        Engine.Error.create ~phase:Engine.Error.Analysis
          ~cause:Engine.Error.Invariant_violation "injected failure"
      in
      let failed =
        Engine.Workspace_snapshot.with_snapshot workspace (fun snapshot ->
            failed_snapshot := Some (Engine.Workspace_snapshot.root snapshot);
            Error primary)
      in
      (match failed with
      | Error error ->
          Alcotest.(check string)
            "primary error wins" "analysis"
            (Engine.Error.phase_name (Engine.Error.phase error))
      | Ok _ -> Alcotest.fail "injected snapshot action unexpectedly passed");
      Alcotest.(check bool)
        "snapshot removed after failure" false
        (Sys.file_exists (Option.get !failed_snapshot));
      Alcotest.(check bool)
        "nested build directory is skipped" true
        (Engine.Workspace_snapshot.default_skip
           "fixtures/basic/_build/default/generated.ml");
      (match
         Engine.Workspace_snapshot.with_snapshot workspace (fun outer ->
             let outer_root = Engine.Workspace_snapshot.root outer in
             Engine.Workspace_snapshot.with_snapshot workspace (fun _inner ->
                 Alcotest.(check bool)
                   "recovery preserves a live same-process snapshot" true
                   (Sys.file_exists outer_root);
                 Ok ()))
       with
      | Ok () -> ()
      | Error error -> Alcotest.failf "%a" Engine.Error.pp error);
      let raised_snapshot = ref None in
      (try
         ignore
           (Engine.Workspace_snapshot.with_snapshot workspace (fun snapshot ->
                raised_snapshot :=
                  Some (Engine.Workspace_snapshot.root snapshot);
                raise Exit));
         Alcotest.fail "snapshot action exception was swallowed"
       with Exit -> ());
      Alcotest.(check bool)
        "snapshot removed after exception" false
        (Sys.file_exists (Option.get !raised_snapshot)))

let test_corrupt_cmt_is_fatal () =
  let path = Filename.temp_file "ocaml-mutants-corrupt-" ".cmt" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      (match Engine.Util.write_file path "not a compiler artifact" with
      | Ok () -> ()
      | Error message -> Alcotest.fail message);
      match
        Engine.Ocaml_frontend.discover ~root:(Sys.getcwd ()) ~cmt_files:[ path ]
          ~selected_source:(fun _ -> true)
          ~operators:[ Core.Operator.Comparison ]
      with
      | Error error ->
          Alcotest.(check string)
            "fatal analysis phase" "analysis"
            (Engine.Error.phase_name (Engine.Error.phase error));
          Alcotest.(check string)
            "fatal decode cause" "decode-failure"
            (Engine.Error.cause_name (Engine.Error.cause error))
      | Ok _ -> Alcotest.fail "corrupt CMT was downgraded to a skip")

let stable_id_property =
  let open QCheck in
  Test.make ~name:"stable IDs are deterministic" ~count:500
    (triple nat_small nat_small string) (fun (start, width, replacement) ->
      let start = start mod 20 in
      let width = 1 + (width mod 10) in
      let source = String.make (start + width) 'x' in
      let make () =
        mutant ~rule_name:"negate-condition@1" ~source ~start_byte:start
          ~end_byte:(start + width)
          (if String.trim replacement = "" then "false" else replacement)
      in
      String.equal (id (make ())) (id (make ())))

let no_mutant_property =
  let open QCheck in
  Test.make ~name:"empty instrumentation preserves every byte" ~count:500 string
    (fun source -> instrument source [] = Ok source)

let span_algebra_property =
  let open QCheck in
  Test.make ~name:"span containment is transitive" ~count:500
    (triple nat_small nat_small nat_small) (fun (start, a, b) ->
      let start = start mod 20 in
      let inner_end = start + 1 in
      let middle_end = inner_end + 1 + (a mod 10) in
      let outer_end = middle_end + 1 + (b mod 10) in
      let inner = range start inner_end in
      let middle = range start middle_end in
      let outer = range start outer_end in
      Core.Source_range.contains ~outer ~inner:middle
      && Core.Source_range.contains ~outer:middle ~inner
      && Core.Source_range.contains ~outer ~inner)

let byte_span_algebra_property =
  let open QCheck in
  Test.make ~name:"byte span overlap is symmetric" ~count:500
    (quad nat_small nat_small nat_small nat_small)
    (fun (left_start, left_width, right_start, right_width) ->
      let left =
        get_ok
          (Core.Byte_span.make ~start_byte:(left_start mod 20)
             ~end_byte:((left_start mod 20) + 1 + (left_width mod 10)))
      in
      let right =
        get_ok
          (Core.Byte_span.make ~start_byte:(right_start mod 20)
             ~end_byte:((right_start mod 20) + 1 + (right_width mod 10)))
      in
      Core.Byte_span.overlaps left right = Core.Byte_span.overlaps right left)

let catalog_permutation_property =
  let open QCheck in
  Test.make ~name:"catalog is permutation invariant" ~count:200
    (list_size (Gen.int_bound 20) nat_small)
    (fun values ->
      let values = List.sort_uniq Int.compare values in
      let mutants =
        List.map
          (fun value ->
            let source = string_of_int value in
            mutant ~rule_name:"negate-condition@1" ~source ~start_byte:0
              ~end_byte:(String.length source)
              ("(" ^ source ^ ")"))
          values
      in
      let shuffled = List.rev mutants in
      match (Core.Catalog.of_list mutants, Core.Catalog.of_list shuffled) with
      | Ok left, Ok right ->
          List.map id (Core.Catalog.to_list left)
          = List.map id (Core.Catalog.to_list right)
      | _ -> false)

let operator_registry_lookup_property =
  let open QCheck in
  Test.make ~name:"every registered rule round-trips by stable name" ~count:1
    unit (fun () ->
      List.for_all
        (fun expected ->
          match
            Core.Operator.Rule.of_stable_name
              (Core.Operator.Rule.stable_name expected)
          with
          | Ok actual -> Core.Operator.Rule.equal expected actual
          | Error _ -> false)
        Core.Operator.Rule.all)

let operator_compatibility_property =
  let open QCheck in
  Test.make ~name:"every compatibility witness resolves uniquely after trimming"
    ~count:1 unit (fun () ->
      Core.Operator.Rule.For_testing.compatibility_examples ()
      |> List.for_all (fun (expected, original, replacement) ->
          match
            Core.Operator.rule_for_replacement
              (Core.Operator.Rule.family expected)
              ~original:(" \t" ^ original ^ "\n")
              ~replacement:(" \r" ^ replacement ^ " ")
          with
          | Ok actual -> Core.Operator.Rule.equal expected actual
          | Error _ -> false))

let run_suite () =
  Alcotest.run "ocaml-mutants"
    [
      ( "core",
        [
          Alcotest.test_case "refined values" `Quick test_refined_values;
          Alcotest.test_case "stable id" `Quick test_stable_id;
          Alcotest.test_case "identity inputs" `Quick test_stable_id_changes;
          Alcotest.test_case "user-value operator catalog" `Quick
            test_user_value_operator_catalog;
          Alcotest.test_case "operator registry contract" `Quick
            test_operator_registry_contract;
          Alcotest.test_case "Boolean operator Spec shadow" `Quick
            test_boolean_literal_spec_shadow;
          Alcotest.test_case "decode revalidation" `Quick
            test_decoded_mutant_revalidation;
          Alcotest.test_case "mutant ID prefix validation" `Quick
            test_mutant_id_prefix_validation;
          Alcotest.test_case "instrumentation guard shape" `Quick
            test_instrumentation_guard_shape;
          Alcotest.test_case "instrumentation original-first typing" `Quick
            test_instrumentation_original_first_type_inference;
          Alcotest.test_case "nested ranges" `Quick test_nested_instrumentation;
          Alcotest.test_case "same range/permutation" `Quick
            test_same_range_and_permutation;
          Alcotest.test_case "Parsetree module freshness" `Quick
            test_runtime_module_freshness_uses_parsetree;
          Alcotest.test_case "crossing range" `Quick test_crossing_rejected;
          Alcotest.test_case "catalog/summary" `Quick test_catalog_and_summary;
          Alcotest.test_case "catalog collision proof" `Quick
            test_catalog_distinguishes_collision_from_duplicate;
        ] );
      ( "adapters",
        [
          Alcotest.test_case "OTOML config" `Quick test_config;
          Alcotest.test_case "applicative diagnostics" `Quick
            test_config_accumulates_errors;
          Alcotest.test_case "stages/profiles/expectations" `Quick
            test_config_stages_profiles_and_expectations;
          Alcotest.test_case "canonical sexp" `Quick test_csexp;
          Alcotest.test_case "Dune workspace v0.1" `Quick
            test_dune_workspace_decoder;
          Alcotest.test_case "Dune tests/roles" `Quick
            test_dune_tests_decoder_and_roles;
          Alcotest.test_case "report codec" `Quick test_report_codec;
          Alcotest.test_case "Stryker thresholds" `Quick
            test_stryker_threshold_validation;
          Alcotest.test_case "Stryker global mutant IDs" `Quick
            test_stryker_global_mutant_ids;
          Alcotest.test_case "Stryker report projection" `Quick
            test_stryker_report_projection;
          Alcotest.test_case "pre-cancelled process" `Quick
            test_pre_cancelled_process_does_not_spawn;
          Alcotest.test_case "application process-lifetime cancellation" `Quick
            test_application_cancellation_and_signal_restoration;
          Alcotest.test_case "process capture/timeout" `Quick
            test_process_capture_and_timeout;
          Alcotest.test_case "cache proof validation" `Quick
            test_cache_never_returns_unproven_outcomes;
          Alcotest.test_case "cache ownership/concurrency" `Quick
            test_cache_ownership_and_concurrency;
          Alcotest.test_case "snapshot manifest/cleanup" `Quick
            test_snapshot_manifest_and_cleanup;
          Alcotest.test_case "corrupt CMT is fatal" `Quick
            test_corrupt_cmt_is_fatal;
        ] );
      ( "properties",
        List.map QCheck_alcotest.to_alcotest
          [
            stable_id_property;
            no_mutant_property;
            span_algebra_property;
            byte_span_algebra_property;
            catalog_permutation_property;
            operator_registry_lookup_property;
            operator_compatibility_property;
          ] );
    ]

let () =
  if Engine.Process_supervisor.helper_requested Sys.argv then
    exit (Engine.Process_supervisor.run_helper Sys.argv)
  else (
    Engine.Process_supervisor.configure_helper_executable
      (Unix.realpath Sys.executable_name);
    match Sys.getenv_opt "OCAML_MUTANTS_TEST_CHILD" with
    | Some "large-output" ->
        let capture_capacity =
          Engine.Process_supervisor.capture_capacity_bytes
        in
        output_char stdout 'H';
        output_string stdout (String.make (capture_capacity + 1022) 'o');
        output_char stdout 'T';
        flush stdout;
        output_char stderr 'E';
        output_string stderr (String.make (capture_capacity + 1022) 'e');
        output_char stderr 'R';
        flush stderr
    | Some "sleep" -> Unix.sleepf 60.
    | Some "grandchild" ->
        let marker = Sys.getenv "OCAML_MUTANTS_TEST_GRANDCHILD_PID" in
        Unix.putenv "OCAML_MUTANTS_TEST_CHILD" "sleep";
        let executable = Unix.realpath Sys.executable_name in
        let child =
          Unix.create_process executable [| executable |] Unix.stdin Unix.stdout
            Unix.stderr
        in
        let pid = Engine.Process_supervisor.process_id child in
        (match Engine.Util.atomic_write marker (string_of_int pid ^ "\n") with
        | Ok () -> ()
        | Error message -> failwith message);
        ignore (Unix.waitpid [] child)
    | Some mode -> failwith ("unknown test child mode: " ^ mode)
    | None -> run_suite ())
