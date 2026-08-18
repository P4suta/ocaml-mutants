module Core = Ocaml_mutants_core
module Engine = Ocaml_mutants_engine

let get_ok = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let typed_expression ?environment source_text =
  let environment =
    Option.value environment ~default:(Compmisc.initial_env ())
  in
  let lexbuf = Lexing.from_string source_text in
  Lexing.set_filename lexbuf "lib/condition_shadow_fixture.ml";
  try Typecore.type_expression environment (Parse.expression lexbuf)
  with exception_ ->
    Alcotest.failf "cannot type condition fixture %S: %s" source_text
      (Printexc.to_string exception_)

let environment_after source_text =
  let lexbuf = Lexing.from_string source_text in
  Lexing.set_filename lexbuf "lib/condition_shadow_types.ml";
  let parsed = Parse.implementation lexbuf in
  let _, _, _, _, environment =
    Typemod.type_structure (Compmisc.initial_env ()) parsed
  in
  environment

let range_of_expression expression =
  let location = expression.Typedtree.exp_loc in
  get_ok
    (Core.Source_range.make
       ~start_byte:location.Location.loc_start.Lexing.pos_cnum
       ~end_byte:location.loc_end.pos_cnum
       ~start_line:location.loc_start.pos_lnum
       ~start_column:(location.loc_start.pos_cnum - location.loc_start.pos_bol)
       ~end_line:location.loc_end.pos_lnum
       ~end_column:(location.loc_end.pos_cnum - location.loc_end.pos_bol))

let source_slice source range =
  match Core.Source.slice source range with
  | Ok bytes -> bytes
  | Error error ->
      Alcotest.failf "cannot slice condition fixture: %a" Core.Source.pp_error
        error

let condition_of expression =
  match expression.Typedtree.exp_desc with
  | Typedtree.Texp_ifthenelse (condition, _, _) -> condition
  | _ -> Alcotest.fail "fixture is not an if expression"

let condition_rule original replacement =
  get_ok
    (Core.Operator.rule_for_replacement Core.Operator.Condition_negation
       ~original ~replacement)

let make_legacy ?(path = "lib/condition_shadow_fixture.ml") ~source_text ~range
    ~rule ~replacement () =
  let source = Core.Source.of_string source_text in
  let unchecked =
    match Core.Mutant.unchecked ~path ~range ~rule ~replacement with
    | Ok unchecked -> unchecked
    | Error error ->
        Alcotest.failf "cannot construct condition legacy fixture: %a"
          Core.Mutant.pp_validation_error error
  in
  let legacy =
    match Core.Mutant.validate ~source unchecked with
    | Ok mutant -> mutant
    | Error error ->
        Alcotest.failf "cannot validate condition legacy fixture: %a"
          Core.Mutant.pp_validation_error error
  in
  (source, legacy)

let candidate source_bytes expression =
  match
    Core.Operator.Spec.evaluate_condition_negation ~source_bytes expression
  with
  | [ Core.Operator.Spec.Candidate { rule; plan } ] -> (rule, plan)
  | [ Core.Operator.Spec.Rejection { rule; reason } ] ->
      Alcotest.failf "condition fixture %s was rejected: %s"
        (Core.Operator.Rule.stable_name rule)
        (Core.Operator.Spec.rejection_name reason)
  | [] -> Alcotest.fail "condition fixture produced no shadow event"
  | decisions ->
      Alcotest.failf "condition fixture produced %d shadow events"
        (List.length decisions)

let verify source range expression legacy =
  match
    Engine.Ocaml_frontend.For_testing.verify_condition_shadow_event ~source
      ~path:(Core.Mutant.path legacy) ~range ~expression ~legacy:[ legacy ]
  with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "condition parity failed: %a" Engine.Error.pp error

let expect_invariant label = function
  | Ok () -> Alcotest.failf "%s unexpectedly preserved parity" label
  | Error error ->
      Alcotest.(check string)
        (label ^ " cause") "invariant-violation"
        (Engine.Error.cause_name (Engine.Error.cause error));
      error

let check_parity ?environment label source_text =
  let root = typed_expression ?environment source_text in
  let expression = condition_of root in
  let source = Core.Source.of_string source_text in
  let range = range_of_expression expression in
  let source_bytes = source_slice source range in
  let replacement = "Stdlib.not (" ^ source_bytes ^ ")" in
  let expected_rule = condition_rule source_bytes replacement in
  let rule, plan = candidate source_bytes expression in
  Alcotest.(check string)
    (label ^ " rule") "negate-condition@1"
    (Core.Operator.Rule.stable_name rule);
  Alcotest.(check bool)
    (label ^ " legacy rule") true
    (Core.Operator.Rule.equal expected_rule rule);
  Alcotest.(check string)
    (label ^ " raw source") source_bytes
    (Core.Operator.Spec.Replacement_plan.source_bytes plan);
  Alcotest.(check string)
    (label ^ " rendered bytes")
    replacement
    (Core.Operator.Spec.Replacement_plan.replacement_bytes plan);
  Alcotest.(check string)
    (label ^ " target-shape key")
    "wrapper:Stdlib.not"
    (Core.Operator.Spec.Replacement_plan.semantic_key plan);
  let source, legacy =
    make_legacy ~source_text ~range ~rule:expected_rule ~replacement ()
  in
  let full_id = Core.Mutant.Id.full (Core.Mutant.id legacy) in
  verify source range expression legacy;
  Alcotest.(check string)
    (label ^ " production ID") full_id
    (Core.Mutant.Id.full (Core.Mutant.id legacy))

let test_registry_and_effectful_raw_rendering () =
  Compmisc.init_path ();
  let definitions =
    Core.Operator.Spec.For_testing.condition_negation_specs ()
  in
  Alcotest.(check (list string))
    "ordered registry" [ "negate-condition@1" ]
    (List.map
       (fun definition ->
         Core.Operator.Spec.rule definition |> Core.Operator.Rule.stable_name)
       definitions);
  check_parity "effectful comments and attribute"
    "if ((let hits = ref 0 in incr hits; true (* keep *)) [@warning \"-32\"])\n\
    \     then 1 else 2"

let test_manifest_bool_alias () =
  Compmisc.init_path ();
  let environment = environment_after "type truth = bool" in
  check_parity ~environment "manifest bool alias"
    "if (true : truth) then 1 else 2"

let test_not_applicable_source_and_range_mismatch () =
  Compmisc.init_path ();
  let integer = typed_expression "1" in
  Alcotest.(check int)
    "non-Boolean expression is silent" 0
    (List.length
       (Core.Operator.Spec.evaluate_condition_negation ~source_bytes:"1" integer));
  let source_text = "if true then 1 else 2" in
  let root = typed_expression source_text in
  let expression = condition_of root in
  (match
     Core.Operator.Spec.evaluate_condition_negation ~source_bytes:"(" expression
   with
  | [ Core.Operator.Spec.Rejection { reason; _ } ] ->
      Alcotest.(check string)
        "malformed source rejection" "source-bytes-mismatch"
        (Core.Operator.Spec.rejection_name reason)
  | _ -> Alcotest.fail "malformed condition source was not rejected once");
  let source = Core.Source.of_string source_text in
  let wrong_range = range_of_expression root in
  let wrong_original = source_slice source wrong_range in
  let replacement = "Stdlib.not (" ^ wrong_original ^ ")" in
  let rule = condition_rule wrong_original replacement in
  let source, legacy =
    make_legacy ~source_text ~range:wrong_range ~rule ~replacement ()
  in
  let error =
    expect_invariant "range mismatch"
      (Engine.Ocaml_frontend.For_testing.verify_condition_shadow ~source
         ~path:(Core.Mutant.path legacy) ~range:wrong_range ~expression ~legacy)
  in
  Alcotest.(check bool)
    "typed range evidence is reported" true
    (List.mem_assoc "decision" (Engine.Error.context error))

let () =
  Alcotest.run "Condition shadow parity contract"
    [
      ( "condition-negation",
        [
          Alcotest.test_case "registry, effects, comments, attributes" `Quick
            test_registry_and_effectful_raw_rendering;
          Alcotest.test_case "manifest Boolean alias" `Quick
            test_manifest_bool_alias;
          Alcotest.test_case "Not_applicable and mismatches" `Quick
            test_not_applicable_source_and_range_mismatch;
        ] );
    ]
