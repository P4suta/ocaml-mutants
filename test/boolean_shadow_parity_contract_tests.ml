module Core = Ocaml_mutants_core
module Engine = Ocaml_mutants_engine

let get_ok = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let typed_expression environment source =
  let lexbuf = Lexing.from_string source in
  Location.init lexbuf "lib/boolean_shadow_fixture.ml";
  try Typecore.type_expression environment (Parse.expression lexbuf)
  with exception_ ->
    Alcotest.failf "cannot type Boolean fixture %S: %s" source
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

let literal_transition expression =
  match expression.Typedtree.exp_desc with
  | Typedtree.Texp_construct (_, constructor, [])
    when String.equal constructor.cstr_name "true" ->
      ("true", "false")
  | Typedtree.Texp_construct (_, constructor, [])
    when String.equal constructor.cstr_name "false" ->
      ("false", "true")
  | _ -> Alcotest.fail "fixture did not type as a Boolean literal"

let literal_rule expression =
  let original, replacement = literal_transition expression in
  get_ok
    (Core.Operator.rule_for_replacement Core.Operator.Boolean_literal ~original
       ~replacement)

let source_slice source range =
  match Core.Source.slice source range with
  | Ok bytes -> bytes
  | Error error ->
      Alcotest.failf "cannot slice Boolean fixture: %a" Core.Source.pp_error
        error

let make_legacy ?(path = "lib/boolean_shadow_fixture.ml") ?range ?rule
    ?replacement ~source expression =
  let canonical_original, canonical_replacement =
    literal_transition expression
  in
  let replacement = Option.value replacement ~default:canonical_replacement in
  let range = Option.value range ~default:(range_of_expression expression) in
  let rule =
    match rule with
    | Some rule -> rule
    | None ->
        let raw_original = source_slice source range in
        get_ok
          (Core.Operator.rule_for_replacement Core.Operator.Boolean_literal
             ~original:raw_original ~replacement:canonical_replacement)
  in
  let unchecked =
    match Core.Mutant.unchecked ~path ~range ~rule ~replacement with
    | Ok unchecked -> unchecked
    | Error error ->
        Alcotest.failf "cannot construct legacy Boolean fixture: %a"
          Core.Mutant.pp_validation_error error
  in
  match Core.Mutant.validate ~source unchecked with
  | Ok mutant -> mutant
  | Error error ->
      Alcotest.failf "cannot validate legacy Boolean fixture: %a"
        Core.Mutant.pp_validation_error error

let expect_invariant label = function
  | Ok () -> Alcotest.failf "%s unexpectedly preserved parity" label
  | Error error ->
      Alcotest.(check string)
        (label ^ " phase") "analysis"
        (Engine.Error.phase_name (Engine.Error.phase error));
      Alcotest.(check string)
        (label ^ " cause") "invariant-violation"
        (Engine.Error.cause_name (Engine.Error.cause error));
      error

let find_substring ~needle value =
  let rec search offset =
    if offset + String.length needle > String.length value then None
    else if String.equal (String.sub value offset (String.length needle)) needle
    then Some offset
    else search (offset + 1)
  in
  search 0

let remove_if_present path = try Sys.remove path with Sys_error _ -> ()

let with_serialized_cmt ~source use =
  let directory =
    Filename.temp_dir ~perms:0o700 "ocaml-mutants-assert-false-" ".tmp"
  in
  let source_path = Filename.concat directory "assert_false_fixture.ml" in
  let cmi_path = Filename.concat directory "assert_false_fixture.cmi" in
  let cmo_path = Filename.concat directory "assert_false_fixture.cmo" in
  let cmt_path = Filename.concat directory "assert_false_fixture.cmt" in
  let cleanup () =
    List.iter remove_if_present [ cmt_path; cmo_path; cmi_path; source_path ];
    try Sys.rmdir directory with Sys_error _ -> ()
  in
  Fun.protect ~finally:cleanup (fun () ->
      let channel = open_out_bin source_path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () -> output_string channel source);
      let compiler_name = "ocamlc" ^ Config.ext_exe in
      let configured_compiler = Filename.concat Config.bindir compiler_name in
      let compiler =
        if Sys.file_exists configured_compiler then configured_compiler
        else
          match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
          | Some prefix ->
              Filename.concat (Filename.concat prefix "bin") compiler_name
          | None -> compiler_name
      in
      let command =
        Filename.quote_command compiler
          [ "-bin-annot"; "-c"; "-o"; cmo_path; source_path ]
      in
      let status = Sys.command command in
      Alcotest.(check int) "fixture compiler exit" 0 status;
      Alcotest.(check bool)
        "fixture emitted a serialized CMT" true (Sys.file_exists cmt_path);
      use ~root:directory ~cmt_path)

let check_field_order label value =
  let fields =
    [
      "rule=";
      "path=";
      "range=";
      "original=";
      "replacement=";
      "source_digest=";
      "full_id=";
    ]
  in
  let rec check previous = function
    | [] -> ()
    | field :: rest -> (
        match find_substring ~needle:field value with
        | Some offset when offset > previous -> check offset rest
        | Some _ -> Alcotest.failf "%s field %s is out of order" label field
        | None -> Alcotest.failf "%s field %s is absent" label field)
  in
  check (-1) fields

let candidate_plan source_bytes expression =
  match
    Core.Operator.Spec.evaluate_boolean_literal ~source_bytes expression
  with
  | [ Core.Operator.Spec.Candidate { plan; _ } ] -> plan
  | [ Core.Operator.Spec.Rejection { reason; _ } ] ->
      Alcotest.failf "fixture source proof was rejected: %s"
        (Core.Operator.Spec.rejection_name reason)
  | [] -> Alcotest.fail "fixture produced no Boolean shadow event"
  | _ -> Alcotest.fail "fixture produced multiple Boolean shadow events"

let test_source_forms_preserve_exact_parity () =
  Compmisc.init_path ();
  let environment = Compmisc.initial_env () in
  let fixtures =
    [
      ("true", "true", "plain true");
      ("false", "false", "plain false");
      (" \tfalse\n", "false", "outer whitespace");
      ("(* before *) false (* after *)", "false", "comments");
      ("true [@warning \"-26\"]", "true", "attribute");
    ]
  in
  List.iter
    (fun (source_text, expected_raw, label) ->
      let expression = typed_expression environment source_text in
      let range = range_of_expression expression in
      let source = Core.Source.of_string source_text in
      let raw_original = source_slice source range in
      Alcotest.(check string)
        (label ^ " Typedtree range")
        expected_raw raw_original;
      let plan = candidate_plan raw_original expression in
      Alcotest.(check string)
        (label ^ " plan owns raw source")
        raw_original
        (Core.Operator.Spec.Replacement_plan.source_bytes plan);
      let legacy = make_legacy ~source expression in
      let original_id = Core.Mutant.Id.full (Core.Mutant.id legacy) in
      (match
         Engine.Ocaml_frontend.For_testing.verify_boolean_shadow_event ~source
           ~path:(Core.Mutant.path legacy) ~range ~expression ~legacy:[ legacy ]
       with
      | Ok () -> ()
      | Error error -> Alcotest.failf "%s: %a" label Engine.Error.pp error);
      Alcotest.(check string)
        (label ^ " leaves the production ID unchanged")
        original_id
        (Core.Mutant.Id.full (Core.Mutant.id legacy)))
    fixtures

let test_parenthesized_source_policy_is_explicit () =
  Compmisc.init_path ();
  let environment = Compmisc.initial_env () in
  let source_text = "( true )" in
  let expression = typed_expression environment source_text in
  let range = range_of_expression expression in
  let source = Core.Source.of_string source_text in
  let raw_original = source_slice source range in
  Alcotest.(check string)
    "Typedtree range owns the parentheses" source_text raw_original;
  let plan = candidate_plan raw_original expression in
  Alcotest.(check string)
    "Spec plan owns the parenthesized slice" raw_original
    (Core.Operator.Spec.Replacement_plan.source_bytes plan);
  let _, replacement = literal_transition expression in
  match
    Core.Operator.rule_for_replacement Core.Operator.Boolean_literal
      ~original:raw_original ~replacement
  with
  | Error _ -> ()
  | Ok _ ->
      Alcotest.fail
        "parenthesized fixture unexpectedly crossed the unchanged legacy gate"

let test_not_applicable_emits_no_event () =
  Compmisc.init_path ();
  let environment = Compmisc.initial_env () in
  let expression = typed_expression environment "None" in
  Alcotest.(check int)
    "non-Boolean expression" 0
    (List.length
       (Core.Operator.Spec.evaluate_boolean_literal ~source_bytes:"None"
          expression));
  let legacy_expression = typed_expression environment "true" in
  let source = Core.Source.of_string "true" in
  let legacy = make_legacy ~source legacy_expression in
  let error =
    expect_invariant "verifier not-applicable"
      (Engine.Ocaml_frontend.For_testing.verify_boolean_shadow ~source
         ~path:(Core.Mutant.path legacy)
         ~range:(range_of_expression legacy_expression)
         ~expression ~legacy)
  in
  Alcotest.(check (option string))
    "verifier records the absent event" (Some "not-applicable")
    (List.assoc_opt "decision" (Engine.Error.context error))

let test_stale_and_malformed_source_are_fatal () =
  Compmisc.init_path ();
  let environment = Compmisc.initial_env () in
  let stale_expression = typed_expression environment "false" in
  let stale_source = Core.Source.of_string "true " in
  let stale_legacy =
    make_legacy
      ~rule:(literal_rule stale_expression)
      ~source:stale_source stale_expression
  in
  let stale_error =
    expect_invariant "stale source"
      (Engine.Ocaml_frontend.For_testing.verify_boolean_shadow
         ~source:stale_source
         ~path:(Core.Mutant.path stale_legacy)
         ~range:(range_of_expression stale_expression)
         ~expression:stale_expression ~legacy:stale_legacy)
  in
  Alcotest.(check (option string))
    "stale source is an explicit rejection"
    (Some "rejection:false-to-true@1:source-bytes-mismatch")
    (List.assoc_opt "decision" (Engine.Error.context stale_error));
  let malformed_expression = typed_expression environment "true" in
  let malformed_source = Core.Source.of_string "@#$%" in
  let malformed_legacy =
    make_legacy
      ~rule:(literal_rule malformed_expression)
      ~source:malformed_source malformed_expression
  in
  ignore
    (expect_invariant "malformed source"
       (Engine.Ocaml_frontend.For_testing.verify_boolean_shadow
          ~source:malformed_source
          ~path:(Core.Mutant.path malformed_legacy)
          ~range:(range_of_expression malformed_expression)
          ~expression:malformed_expression ~legacy:malformed_legacy))

let test_ordered_identity_fields_are_all_compared () =
  Compmisc.init_path ();
  let environment = Compmisc.initial_env () in
  let expression = typed_expression environment "true" in
  let range = range_of_expression expression in
  let source = Core.Source.of_string "true" in
  let false_expression = typed_expression environment "false" in
  let wrong_rule_legacy =
    make_legacy ~rule:(literal_rule false_expression) ~source expression
  in
  ignore
    (expect_invariant "rule mismatch"
       (Engine.Ocaml_frontend.For_testing.verify_boolean_shadow ~source
          ~path:(Core.Mutant.path wrong_rule_legacy)
          ~range ~expression ~legacy:wrong_rule_legacy));
  let wrong_replacement_legacy =
    make_legacy ~replacement:" false " ~source expression
  in
  ignore
    (expect_invariant "replacement mismatch"
       (Engine.Ocaml_frontend.For_testing.verify_boolean_shadow ~source
          ~path:(Core.Mutant.path wrong_replacement_legacy)
          ~range ~expression ~legacy:wrong_replacement_legacy));
  let legacy = make_legacy ~path:"lib/legacy.ml" ~source expression in
  let path_error =
    expect_invariant "path mismatch"
      (Engine.Ocaml_frontend.For_testing.verify_boolean_shadow ~source
         ~path:"lib/shadow.ml" ~range ~expression ~legacy)
  in
  List.iter
    (fun key ->
      match List.assoc_opt key (Engine.Error.context path_error) with
      | Some value -> check_field_order key value
      | None -> Alcotest.failf "path mismatch has no %s field list" key)
    [ "legacy"; "spec" ];
  let repeated_source = Core.Source.of_string "truetrue" in
  let later_range =
    get_ok
      (Core.Source_range.make ~start_byte:4 ~end_byte:8 ~start_line:1
         ~start_column:4 ~end_line:1 ~end_column:8)
  in
  let range_legacy =
    make_legacy ~source:repeated_source ~range:later_range expression
  in
  ignore
    (expect_invariant "range mismatch"
       (Engine.Ocaml_frontend.For_testing.verify_boolean_shadow
          ~source:repeated_source
          ~path:(Core.Mutant.path range_legacy)
          ~range ~expression ~legacy:range_legacy));
  let prior_source = Core.Source.of_string "true " in
  let prior_legacy = make_legacy ~source:prior_source expression in
  let digest_error =
    expect_invariant "source digest and ID mismatch"
      (Engine.Ocaml_frontend.For_testing.verify_boolean_shadow ~source
         ~path:(Core.Mutant.path prior_legacy)
         ~range ~expression ~legacy:prior_legacy)
  in
  let legacy_fields =
    Option.value
      (List.assoc_opt "legacy" (Engine.Error.context digest_error))
      ~default:""
  in
  let spec_fields =
    Option.value
      (List.assoc_opt "spec" (Engine.Error.context digest_error))
      ~default:""
  in
  Alcotest.(check bool)
    "source digest participates in parity" true
    (not (String.equal legacy_fields spec_fields));
  check_field_order "digest legacy" legacy_fields;
  check_field_order "digest spec" spec_fields

let test_direct_assert_false_is_filtered_from_serialized_cmt () =
  let source =
    String.concat "\n"
      [
        "let head = function";
        "  | [] -> assert false";
        "  | head :: _ -> head";
        "";
        "let direct_unit () = assert (false)";
        "let ordinary = false";
        "";
        "let nested flag = assert (flag || false)";
        "";
      ]
  in
  with_serialized_cmt ~source (fun ~root ~cmt_path ->
      let discovery =
        match
          Engine.Ocaml_frontend.discover ~root ~cmt_files:[ cmt_path ]
            ~selected_source:(fun _ -> true)
            ~operators:[ Core.Operator.Boolean_literal ]
        with
        | Ok discovery -> discovery
        | Error error ->
            Alcotest.failf "cannot discover serialized assert fixture: %a"
              Engine.Error.pp error
      in
      let mutants = Core.Catalog.to_list discovery.catalog in
      Alcotest.(check int)
        "only ordinary and nested false literals remain" 2 (List.length mutants);
      Alcotest.(check (list int))
        "direct assertions are absent by typed parentage" [ 6; 8 ]
        (List.map
           (fun mutant ->
             Core.Mutant.range mutant |> Core.Source_range.start_line)
           mutants);
      Alcotest.(check (list string))
        "remaining rule IDs are unchanged"
        [ "false-to-true@1"; "false-to-true@1" ]
        (List.map
           (fun mutant ->
             Core.Mutant.rule mutant |> Core.Operator.Rule.stable_name)
           mutants);
      Alcotest.(check (list string))
        "remaining replacements preserve the normal Boolean rule"
        [ "true"; "true" ]
        (List.map Core.Mutant.replacement mutants))

let () =
  Alcotest.run "Boolean shadow parity contract"
    [
      ( "parity",
        [
          Alcotest.test_case "source forms and exact ownership" `Quick
            test_source_forms_preserve_exact_parity;
          Alcotest.test_case "parenthesized source policy" `Quick
            test_parenthesized_source_policy_is_explicit;
          Alcotest.test_case "not applicable has no event" `Quick
            test_not_applicable_emits_no_event;
          Alcotest.test_case "stale and malformed source" `Quick
            test_stale_and_malformed_source_are_fatal;
          Alcotest.test_case "ordered identity fields" `Quick
            test_ordered_identity_fields_are_all_compared;
          Alcotest.test_case "direct assert false is filtered from CMT" `Quick
            test_direct_assert_false_is_filtered_from_serialized_cmt;
        ] );
    ]
