module Core = Ocaml_mutants_core

let get_ok = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let typed_expression environment source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf "operator-spec-contract.ml";
  try Typecore.type_expression environment (Parse.expression lexbuf)
  with exn ->
    Alcotest.failf "cannot type fixed expression %S: %s" source
      (Printexc.to_string exn)

let environment_after source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf "operator-spec-environment.ml";
  let parsed = Parse.implementation lexbuf in
  let _, _, _, _, environment =
    Typemod.type_structure (Compmisc.initial_env ()) parsed
  in
  environment

let candidate_plan = function
  | [ Core.Operator.Spec.Candidate { plan; _ } ] -> plan
  | [] -> Alcotest.fail "shadow evaluation produced no candidate"
  | [ Core.Operator.Spec.Rejection { reason; _ } ] ->
      Alcotest.failf "shadow evaluation rejected the candidate: %s"
        (Core.Operator.Spec.rejection_name reason)
  | _ -> Alcotest.fail "shadow evaluation produced multiple decisions"

let decision_rule_names evaluations =
  List.map
    (function
      | Core.Operator.Spec.Candidate { rule; _ }
      | Core.Operator.Spec.Rejection { rule; _ } ->
          Core.Operator.Rule.stable_name rule)
    evaluations

let expect_plan_error label expected = function
  | Error actual -> Alcotest.(check string) label expected actual
  | Ok _ -> Alcotest.failf "%s unexpectedly produced a plan" label

let test_checked_replacement_plan_invariants () =
  let create = Core.Operator.Spec.Replacement_plan.For_testing.create in
  expect_plan_error "empty source" "source bytes are empty"
    (create ~source_bytes:" \t" ~replacement_bytes:"replacement"
       ~semantic_key:"key");
  expect_plan_error "empty replacement" "replacement bytes are empty"
    (create ~source_bytes:"source" ~replacement_bytes:"\r\n" ~semantic_key:"key");
  expect_plan_error "empty semantic key" "semantic key is empty"
    (create ~source_bytes:"source" ~replacement_bytes:"replacement"
       ~semantic_key:" ");
  let identical =
    get_ok
      (create ~source_bytes:"same" ~replacement_bytes:"same"
         ~semantic_key:"replacement:opposite-if-branch")
  in
  Alcotest.(check string)
    "byte-identical branch bytes remain catalog-compatible" "same"
    (Core.Operator.Spec.Replacement_plan.source_bytes identical);
  Alcotest.(check string)
    "byte-identical replacement remains exact" "same"
    (Core.Operator.Spec.Replacement_plan.replacement_bytes identical);
  Alcotest.check_raises "invalid static constants fail fast"
    (Invalid_argument
       "invalid fixture replacement plan: replacement bytes are empty")
    (fun () ->
      ignore
        (Core.Operator.Spec.Replacement_plan.For_testing.require_static
           ~context:"fixture" ~source_bytes:"source" ~replacement_bytes:" "
           ~semantic_key:"key"));
  Alcotest.check_raises "static transformations must change their constant"
    (Invalid_argument
       "invalid fixture replacement plan: replacement does not change the \
        source bytes") (fun () ->
      ignore
        (Core.Operator.Spec.Replacement_plan.For_testing.require_static
           ~context:"fixture" ~source_bytes:"same" ~replacement_bytes:"same"
           ~semantic_key:"key"))

let test_packed_registry_contract () =
  let first = Core.Operator.Spec.For_testing.boolean_literal_specs () in
  let second = Core.Operator.Spec.For_testing.boolean_literal_specs () in
  Alcotest.(check bool)
    "the validated shadow registry is constructed once" true (first == second);
  Alcotest.(check (list string))
    "packed rules remain ordered and stable"
    [ "true-to-false@1"; "false-to-true@1" ]
    (List.map
       (fun packed ->
         Core.Operator.Spec.rule packed |> Core.Operator.Rule.stable_name)
       first);
  Alcotest.(check bool)
    "packed rules retain their production metadata" true
    (List.for_all
       (fun packed ->
         Core.Operator.Rule.profile (Core.Operator.Spec.rule packed)
         = Core.Operator.Profile.Balanced)
       first)

let test_source_proof_and_shadow_boundaries () =
  Compmisc.init_path ();
  let environment = Compmisc.initial_env () in
  let typed_true = typed_expression environment "true" in
  let typed_none = typed_expression environment "None" in
  let definitions = Core.Operator.Spec.For_testing.boolean_literal_specs () in
  let evaluate source_bytes expression =
    Core.Operator.Spec.For_testing.evaluate_expression ~definitions
      ~source_bytes expression
  in
  let first_plan = candidate_plan (evaluate "true" typed_true) in
  let second_plan = candidate_plan (evaluate "true" typed_true) in
  Alcotest.(check bool)
    "evaluation reuses the statically validated plan" true
    (first_plan == second_plan);
  Alcotest.(check string)
    "plan owns exact source bytes" "true"
    (Core.Operator.Spec.Replacement_plan.source_bytes first_plan);
  Alcotest.(check string)
    "plan owns exact replacement bytes" "false"
    (Core.Operator.Spec.Replacement_plan.replacement_bytes first_plan);
  Alcotest.(check string)
    "family-qualified key remains shadow-only" "boolean-literal:true->false"
    (Core.Operator.Spec.Replacement_plan.semantic_key first_plan);
  (match evaluate " true " typed_true with
  | [ Core.Operator.Spec.Candidate { rule; plan } ] ->
      Alcotest.(check string)
        "reparsed source keeps the existing rule" "true-to-false@1"
        (Core.Operator.Rule.stable_name rule);
      Alcotest.(check string)
        "plan owns the exact reparsed slice" " true "
        (Core.Operator.Spec.Replacement_plan.source_bytes plan)
  | _ -> Alcotest.fail "reparsed Boolean source was not accepted");
  (match evaluate "not true" typed_true with
  | [ Core.Operator.Spec.Rejection { rule; reason } ] ->
      Alcotest.(check string)
        "malformed ownership keeps the existing rule" "true-to-false@1"
        (Core.Operator.Rule.stable_name rule);
      Alcotest.(check string)
        "typed/source mismatch remains explicit" "source-bytes-mismatch"
        (Core.Operator.Spec.rejection_name reason)
  | _ -> Alcotest.fail "typed/source mismatch was not rejected");
  let legacy_trimmed_rule =
    get_ok
      (Core.Operator.rule_for_replacement Core.Operator.Boolean_literal
         ~original:" true " ~replacement:"false")
  in
  Alcotest.(check string)
    "production compatibility still trims independently of the shadow"
    "true-to-false@1"
    (Core.Operator.Rule.stable_name legacy_trimmed_rule);
  Alcotest.(check int)
    "unrelated typed nodes emit no rejection" 0
    (List.length (evaluate "None" typed_none))

let test_typed_visit_sites_are_disjoint () =
  Compmisc.init_path ();
  let environment = Compmisc.initial_env () in
  let definitions = Core.Operator.Spec.For_testing.all_specs () in
  List.iter
    (fun definition ->
      let rule = Core.Operator.Spec.rule definition in
      let expected =
        match Core.Operator.Rule.family rule with
        | Core.Operator.Boolean_literal -> "boolean-literal"
        | Core.Operator.Condition_negation -> "condition"
        | Core.Operator.Boolean_connective | Core.Operator.Comparison
        | Core.Operator.Integer_arithmetic | Core.Operator.Float_arithmetic ->
            "binary-application"
        | Core.Operator.If_branch -> "full-if-branch-target"
        | Core.Operator.Sequence_deletion -> "sequence-expression"
        | Core.Operator.Return_replacement -> "function-body-return"
        | Core.Operator.Match_arm -> "match-arm"
        | Core.Operator.Constructor_replacement -> "constructor-application"
      in
      Alcotest.(check string)
        (Core.Operator.Rule.stable_name rule ^ " visit site")
        expected
        (Core.Operator.Spec.For_testing.visit_site_name definition))
    definitions;
  let check_site label expected evaluations =
    Alcotest.(check (list string))
      label expected
      (decision_rule_names evaluations)
  in
  let typed source = typed_expression environment source in
  let typed_true = typed "true" in
  check_site "Boolean literal excludes condition and return rules"
    [ "true-to-false@1" ]
    (Core.Operator.Spec.For_testing.evaluate_expression ~definitions
       ~source_bytes:"true" typed_true);
  check_site "condition excludes literal and return rules"
    [ "negate-condition@1" ]
    (Core.Operator.Spec.For_testing.evaluate_condition_at_site ~definitions
       ~source_bytes:"true" typed_true);
  check_site "function-body return excludes literal and condition rules"
    [ "return-false@1" ]
    (Core.Operator.Spec.For_testing.evaluate_function_body_return_at_site
       ~definitions ~source_bytes:"true" typed_true);
  let binary_source = "1 + 2" in
  check_site "binary application excludes integer return rules"
    [ "int-add-to-sub@1" ]
    (Core.Operator.Spec.For_testing.evaluate_binary_application_at_site
       ~definitions ~source_bytes:binary_source (typed binary_source));
  let if_source = "if true then 1 else 2" in
  check_site "full-if branch excludes integer return rules"
    [ "select-else-branch@1" ]
    (Core.Operator.Spec.For_testing.evaluate_full_if_branch_at_site ~definitions
       ~source_bytes:if_source ~target:Core.Operator.Spec.Then_branch
       (typed if_source));
  let identical_if_source = "if true then 1 else 1" in
  let identical_branch_plan =
    candidate_plan
      (Core.Operator.Spec.For_testing.evaluate_full_if_branch_at_site
         ~definitions ~source_bytes:identical_if_source
         ~target:Core.Operator.Spec.Then_branch
         (typed identical_if_source))
  in
  Alcotest.(check string)
    "identical target bytes remain owned" "1"
    (Core.Operator.Spec.Replacement_plan.source_bytes identical_branch_plan);
  Alcotest.(check string)
    "identical opposite branch remains an exact replacement" "1"
    (Core.Operator.Spec.Replacement_plan.replacement_bytes identical_branch_plan);
  let sequence_source = "ignore (); 1" in
  check_site "sequence excludes integer return rules"
    [ "delete-left-sequence@1" ]
    (Core.Operator.Spec.For_testing.evaluate_sequence_expression_at_site
       ~definitions ~source_bytes:sequence_source (typed sequence_source))

let test_manifest_aliases_stop_at_abstract_boundaries () =
  Compmisc.init_path ();
  let alias_environment =
    environment_after "type count = int\ntype truth = bool"
  in
  let count = typed_expression alias_environment "(1 : count)" in
  let truth = typed_expression alias_environment "(true : truth)" in
  Alcotest.(check bool)
    "manifest integer alias normalizes" true
    (Core.Operator.Spec.Typed_evidence.primitive ~environment:alias_environment
       count.Typedtree.exp_type
    = Core.Operator.Spec.Typed_evidence.Int);
  Alcotest.(check bool)
    "manifest Boolean alias normalizes" true
    (Core.Operator.Spec.Typed_evidence.primitive ~environment:alias_environment
       truth.Typedtree.exp_type
    = Core.Operator.Spec.Typed_evidence.Bool);
  let abstract_environment =
    environment_after
      "module M : sig type t val value : t end = struct type t = int let value \
       = 1 end"
  in
  let abstract = typed_expression abstract_environment "M.value" in
  Alcotest.(check bool)
    "abstract boundary remains opaque" true
    (Core.Operator.Spec.Typed_evidence.primitive
       ~environment:abstract_environment abstract.Typedtree.exp_type
    = Core.Operator.Spec.Typed_evidence.Other)

(* Ordered equality subsumes size, duplicate, and missing-definition checks: the
   Spec registry must be exactly the production rule registry. *)
let test_every_registered_rule_has_one_definition () =
  let registered_names =
    List.map Core.Operator.Rule.stable_name Core.Operator.Rule.all
  in
  let definition_names =
    Core.Operator.Spec.For_testing.all_specs ()
    |> List.map (fun definition ->
        Core.Operator.Spec.rule definition |> Core.Operator.Rule.stable_name)
  in
  Alcotest.(check int)
    "production registry size" 40
    (List.length registered_names);
  Alcotest.(check (list string))
    "the Spec registry is the ordered production registry" registered_names
    definition_names

(* The complete family-to-profile tier map. A change here is an explicit
   operator-contract change: it moves rules in or out of the default Balanced
   catalog. *)
let test_family_profile_tiers () =
  let expected_profile = function
    | Core.Operator.If_branch -> Core.Operator.Profile.Strong
    | Core.Operator.Sequence_deletion -> Core.Operator.Profile.All
    | Core.Operator.Boolean_literal | Core.Operator.Condition_negation
    | Core.Operator.Boolean_connective | Core.Operator.Comparison
    | Core.Operator.Integer_arithmetic | Core.Operator.Float_arithmetic
    | Core.Operator.Return_replacement | Core.Operator.Match_arm
    | Core.Operator.Constructor_replacement ->
        Core.Operator.Profile.Balanced
  in
  List.iter
    (fun rule ->
      Alcotest.(check string)
        (Printf.sprintf "%s profile tier" (Core.Operator.Rule.stable_name rule))
        (Core.Operator.Profile.name
           (expected_profile (Core.Operator.Rule.family rule)))
        (Core.Operator.Profile.name (Core.Operator.Rule.profile rule)))
    Core.Operator.Rule.all;
  Alcotest.(check bool)
    "balanced admits only balanced rules" true
    (List.for_all
       (fun rule ->
         Core.Operator.Profile.includes Core.Operator.Profile.Balanced
           (Core.Operator.Rule.profile rule)
         = (Core.Operator.Rule.profile rule = Core.Operator.Profile.Balanced))
       Core.Operator.Rule.all);
  Alcotest.(check bool)
    "all admits every rule" true
    (List.for_all
       (fun rule ->
         Core.Operator.Profile.includes Core.Operator.Profile.All
           (Core.Operator.Rule.profile rule))
       Core.Operator.Rule.all)

let () =
  Alcotest.run "operator-spec-contract"
    [
      ( "shadow",
        [
          Alcotest.test_case "packed registry" `Quick
            test_packed_registry_contract;
          Alcotest.test_case "every rule has exactly one definition" `Quick
            test_every_registered_rule_has_one_definition;
          Alcotest.test_case "family profile tiers" `Quick
            test_family_profile_tiers;
          Alcotest.test_case "checked replacement-plan invariants" `Quick
            test_checked_replacement_plan_invariants;
          Alcotest.test_case "source proof and boundaries" `Quick
            test_source_proof_and_shadow_boundaries;
          Alcotest.test_case "typed visit sites are disjoint" `Quick
            test_typed_visit_sites_are_disjoint;
          Alcotest.test_case "manifest aliases and abstract boundary" `Quick
            test_manifest_aliases_stop_at_abstract_boundaries;
        ] );
    ]
