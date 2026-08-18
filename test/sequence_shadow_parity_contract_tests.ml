module Core = Ocaml_mutants_core
module Engine = Ocaml_mutants_engine

let get_ok = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let typed_expression source_text =
  let lexbuf = Lexing.from_string source_text in
  Lexing.set_filename lexbuf "lib/sequence_shadow_fixture.ml";
  try
    Typecore.type_expression (Compmisc.initial_env ()) (Parse.expression lexbuf)
  with exception_ ->
    Alcotest.failf "cannot type sequence fixture %S: %s" source_text
      (Printexc.to_string exception_)

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
      Alcotest.failf "cannot slice sequence fixture: %a" Core.Source.pp_error
        error

let sequence_parts expression =
  match expression.Typedtree.exp_desc with
  | Typedtree.Texp_sequence (left, right) -> (left, right)
  | _ -> Alcotest.fail "fixture is not a sequence"

let sequence_rule original replacement =
  get_ok
    (Core.Operator.rule_for_replacement Core.Operator.Sequence_deletion
       ~original ~replacement)

let make_legacy ?(path = "lib/sequence_shadow_fixture.ml") ~source_text
    ~expression ~rule ~replacement () =
  let source = Core.Source.of_string source_text in
  let range = range_of_expression expression in
  let unchecked =
    match Core.Mutant.unchecked ~path ~range ~rule ~replacement with
    | Ok unchecked -> unchecked
    | Error error ->
        Alcotest.failf "cannot construct sequence legacy fixture: %a"
          Core.Mutant.pp_validation_error error
  in
  let legacy =
    match Core.Mutant.validate ~source unchecked with
    | Ok mutant -> mutant
    | Error error ->
        Alcotest.failf "cannot validate sequence legacy fixture: %a"
          Core.Mutant.pp_validation_error error
  in
  (source, range, legacy)

let candidate source_bytes expression =
  match
    Core.Operator.Spec.evaluate_sequence_deletion ~source_bytes expression
  with
  | [ Core.Operator.Spec.Candidate { rule; plan } ] -> (rule, plan)
  | [ Core.Operator.Spec.Rejection { rule; reason } ] ->
      Alcotest.failf "sequence fixture %s was rejected: %s"
        (Core.Operator.Rule.stable_name rule)
        (Core.Operator.Spec.rejection_name reason)
  | [] -> Alcotest.fail "sequence fixture produced no shadow event"
  | decisions ->
      Alcotest.failf "sequence fixture produced %d shadow events"
        (List.length decisions)

let expect_invariant label = function
  | Ok () -> Alcotest.failf "%s unexpectedly preserved parity" label
  | Error error ->
      Alcotest.(check string)
        (label ^ " cause") "invariant-violation"
        (Engine.Error.cause_name (Engine.Error.cause error));
      error

let test_registry_effects_and_exact_rendering () =
  Compmisc.init_path ();
  Alcotest.(check (list string))
    "ordered registry"
    [ "delete-left-sequence@1" ]
    (Core.Operator.Spec.For_testing.sequence_deletion_specs ()
    |> List.map (fun definition ->
        Core.Operator.Spec.rule definition |> Core.Operator.Rule.stable_name));
  let source_text =
    "(print_endline \"left\" (* removed effect *); ((print_endline \"right\"; \
     7 (* retained effect *)) [@warning \"-32\"]))"
  in
  let expression = typed_expression source_text in
  let _, right = sequence_parts expression in
  let source = Core.Source.of_string source_text in
  let range = range_of_expression expression in
  let source_bytes = source_slice source range in
  let right_bytes = source_slice source (range_of_expression right) in
  let expected_rule = sequence_rule source_bytes right_bytes in
  let actual_rule, plan = candidate source_bytes expression in
  Alcotest.(check bool)
    "legacy rule" true
    (Core.Operator.Rule.equal expected_rule actual_rule);
  Alcotest.(check string)
    "raw sequence bytes" source_bytes
    (Core.Operator.Spec.Replacement_plan.source_bytes plan);
  Alcotest.(check string)
    "right expression is retained byte-for-byte" right_bytes
    (Core.Operator.Spec.Replacement_plan.replacement_bytes plan);
  Alcotest.(check string)
    "target-shape semantic key" "replacement:sequence-right"
    (Core.Operator.Spec.Replacement_plan.semantic_key plan);
  let source, range, legacy =
    make_legacy ~source_text ~expression ~rule:expected_rule
      ~replacement:right_bytes ()
  in
  let full_id = Core.Mutant.Id.full (Core.Mutant.id legacy) in
  (match
     Engine.Ocaml_frontend.For_testing.verify_sequence_shadow_event ~source
       ~path:(Core.Mutant.path legacy) ~range ~expression ~legacy:[ legacy ]
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "sequence parity failed: %a" Engine.Error.pp error);
  Alcotest.(check string)
    "production ID" full_id
    (Core.Mutant.Id.full (Core.Mutant.id legacy))

let test_not_applicable_source_and_range_mismatch () =
  Compmisc.init_path ();
  let integer = typed_expression "1" in
  Alcotest.(check int)
    "non-sequence expression is silent" 0
    (List.length
       (Core.Operator.Spec.evaluate_sequence_deletion ~source_bytes:"1" integer));
  let source_text = "print_endline \"left\"; print_endline \"right\"" in
  let expression = typed_expression source_text in
  let _, right = sequence_parts expression in
  let source = Core.Source.of_string source_text in
  let range = range_of_expression expression in
  let source_bytes = source_slice source range in
  let shortened = String.sub source_bytes 0 (String.length source_bytes - 1) in
  (match
     Core.Operator.Spec.evaluate_sequence_deletion ~source_bytes:shortened
       expression
   with
  | [ Core.Operator.Spec.Rejection { reason; _ } ] ->
      Alcotest.(check string)
        "short source rejection" "source-bytes-mismatch"
        (Core.Operator.Spec.rejection_name reason)
  | _ -> Alcotest.fail "short sequence source was not rejected once");
  let right_range = range_of_expression right in
  let right_original = source_slice source right_range in
  let rule = sequence_rule right_original "()" in
  let source, wrong_range, legacy =
    make_legacy ~source_text ~expression:right ~rule ~replacement:"()" ()
  in
  ignore
    (expect_invariant "range mismatch"
       (Engine.Ocaml_frontend.For_testing.verify_sequence_shadow ~source
          ~path:(Core.Mutant.path legacy) ~range:wrong_range ~expression ~legacy))

let () =
  Alcotest.run "Sequence shadow parity contract"
    [
      ( "sequence-deletion",
        [
          Alcotest.test_case "registry, effects, comments, attributes" `Quick
            test_registry_effects_and_exact_rendering;
          Alcotest.test_case "Not_applicable and mismatches" `Quick
            test_not_applicable_source_and_range_mismatch;
        ] );
    ]
