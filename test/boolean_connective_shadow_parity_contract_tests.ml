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
  Lexing.set_filename lexbuf "lib/connective_shadow_fixture.ml";
  try Typecore.type_expression environment (Parse.expression lexbuf)
  with exception_ ->
    Alcotest.failf "cannot type connective fixture %S: %s" source_text
      (Printexc.to_string exception_)

let environment_after source_text =
  let lexbuf = Lexing.from_string source_text in
  Lexing.set_filename lexbuf "lib/connective_shadow_types.ml";
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

let slice_location source_text location =
  let start_byte = location.Location.loc_start.Lexing.pos_cnum in
  let end_byte = location.loc_end.pos_cnum in
  if
    start_byte < 0
    || end_byte > String.length source_text
    || start_byte >= end_byte
  then
    Alcotest.failf "invalid fixture location %d..%d for %S" start_byte end_byte
      source_text
  else String.sub source_text start_byte (end_byte - start_byte)

let source_slice source range =
  match Core.Source.slice source range with
  | Ok bytes -> bytes
  | Error error ->
      Alcotest.failf "cannot slice connective fixture: %a" Core.Source.pp_error
        error

let actual_arguments expression =
  match expression.Typedtree.exp_desc with
  | Typedtree.Texp_apply (_, arguments) ->
      List.filter_map
        (fun (_, argument) ->
          match argument with
          | Typedtree.Arg expression -> Some expression
          | Typedtree.Omitted () -> None)
        arguments
  | _ -> []

let operator_name source_text expression =
  match expression.Typedtree.exp_desc with
  | Typedtree.Texp_apply ({ exp_desc = Typedtree.Texp_ident _; exp_loc; _ }, _)
    ->
      String.trim (slice_location source_text exp_loc)
  | _ -> Alcotest.fail "fixture is not a connective application"

let find_connective source_text token expression =
  let found = ref None in
  let expr iterator expression =
    (match expression.Typedtree.exp_desc with
    | Typedtree.Texp_apply ({ exp_desc = Typedtree.Texp_ident _; exp_loc; _ }, _)
      when String.equal (String.trim (slice_location source_text exp_loc)) token
      ->
        found := Some expression
    | _ -> ());
    Tast_iterator.default_iterator.expr iterator expression
  in
  let iterator = { Tast_iterator.default_iterator with expr } in
  iterator.expr iterator expression;
  match !found with
  | Some expression -> expression
  | None -> Alcotest.failf "cannot find connective %S in %S" token source_text

let application_value_reference expression =
  match expression.Typedtree.exp_desc with
  | Typedtree.Texp_apply
      ({ exp_desc = Typedtree.Texp_ident (path, _, description); _ }, _) ->
      (path, description)
  | _ -> Alcotest.fail "fixture is not a value-backed connective application"

let is_resolved_stdlib_connective expression name =
  let path, description = application_value_reference expression in
  Core.Operator.Spec.Typed_evidence.is_resolved_stdlib_value ~path ~description
    ~environment:expression.Typedtree.exp_env ~name

let transition = function
  | "&&" -> ("||", "and-to-or@1")
  | "||" -> ("&&", "or-to-and@1")
  | token -> Alcotest.failf "unsupported connective token %S" token

let replacement_for source_text expression =
  let token = operator_name source_text expression in
  match (token, actual_arguments expression) with
  | "&&", [ left; right ] ->
      Printf.sprintf "(if (%s) then true else (%s))"
        (slice_location source_text left.Typedtree.exp_loc)
        (slice_location source_text right.Typedtree.exp_loc)
  | "||", [ left; right ] ->
      Printf.sprintf "(if (%s) then (%s) else false)"
        (slice_location source_text left.Typedtree.exp_loc)
        (slice_location source_text right.Typedtree.exp_loc)
  | _, arguments ->
      Alcotest.failf "connective fixture has %d actual arguments, expected two"
        (List.length arguments)

let make_legacy_with ?(path = "lib/connective_shadow_fixture.ml") ~source_text
    ~expression ~rule ~replacement () =
  let source = Core.Source.of_string source_text in
  let range = range_of_expression expression in
  let unchecked =
    match Core.Mutant.unchecked ~path ~range ~rule ~replacement with
    | Ok unchecked -> unchecked
    | Error error ->
        Alcotest.failf "cannot construct connective legacy fixture: %a"
          Core.Mutant.pp_validation_error error
  in
  let legacy =
    match Core.Mutant.validate ~source unchecked with
    | Ok mutant -> mutant
    | Error error ->
        Alcotest.failf "cannot validate connective legacy fixture: %a"
          Core.Mutant.pp_validation_error error
  in
  (source, range, legacy)

let make_legacy source_text expression =
  let original = operator_name source_text expression in
  let replacement_operator, _ = transition original in
  let rule =
    get_ok
      (Core.Operator.rule_for_replacement Core.Operator.Boolean_connective
         ~original ~replacement:replacement_operator)
  in
  make_legacy_with ~source_text ~expression ~rule
    ~replacement:(replacement_for source_text expression)
    ()

let candidate source_bytes expression =
  match
    Core.Operator.Spec.evaluate_boolean_connective ~source_bytes expression
  with
  | [ Core.Operator.Spec.Candidate { rule; plan } ] -> (rule, plan)
  | [ Core.Operator.Spec.Rejection { rule; reason } ] ->
      Alcotest.failf "connective fixture %s was rejected: %s"
        (Core.Operator.Rule.stable_name rule)
        (Core.Operator.Spec.rejection_name reason)
  | [] -> Alcotest.fail "connective fixture produced no shadow event"
  | decisions ->
      Alcotest.failf "connective fixture produced %d shadow events"
        (List.length decisions)

let verify source range expression legacy =
  match
    Engine.Ocaml_frontend.For_testing.verify_boolean_connective_shadow_event
      ~source ~path:(Core.Mutant.path legacy) ~range ~expression
      ~legacy:[ legacy ]
  with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "connective parity failed: %a" Engine.Error.pp error

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

let parse_expression source_text =
  let lexbuf = Lexing.from_string source_text in
  Lexing.set_filename lexbuf "<connective-replacement>";
  Parse.expression lexbuf

let normalized expression = Format.asprintf "%a" Pprintast.expression expression

let boolean_constructor expected expression =
  match expression.Parsetree.pexp_desc with
  | Parsetree.Pexp_construct ({ Asttypes.txt = Longident.Lident name; _ }, None)
    ->
      String.equal name expected
  | _ -> false

let check_short_circuit_shape source_text expression plan =
  let left, right =
    match actual_arguments expression with
    | [ left; right ] ->
        ( slice_location source_text left.Typedtree.exp_loc,
          slice_location source_text right.Typedtree.exp_loc )
    | _ -> Alcotest.fail "connective shape fixture lost its operands"
  in
  let replacement =
    Core.Operator.Spec.Replacement_plan.replacement_bytes plan
  in
  match
    (operator_name source_text expression, parse_expression replacement)
  with
  | ( "&&",
      {
        Parsetree.pexp_desc =
          Parsetree.Pexp_ifthenelse (condition, yes_branch, Some no_branch);
        _;
      } ) ->
      Alcotest.(check string)
        "&& condition is the left effect"
        (normalized (parse_expression left))
        (normalized condition);
      Alcotest.(check bool)
        "&& true branch performs no right effect" true
        (boolean_constructor "true" yes_branch);
      Alcotest.(check string)
        "&& false branch performs the right effect"
        (normalized (parse_expression right))
        (normalized no_branch)
  | ( "||",
      {
        Parsetree.pexp_desc =
          Parsetree.Pexp_ifthenelse (condition, yes_branch, Some no_branch);
        _;
      } ) ->
      Alcotest.(check string)
        "|| condition is the left effect"
        (normalized (parse_expression left))
        (normalized condition);
      Alcotest.(check string)
        "|| true branch performs the right effect"
        (normalized (parse_expression right))
        (normalized yes_branch);
      Alcotest.(check bool)
        "|| false branch performs no right effect" true
        (boolean_constructor "false" no_branch)
  | _ ->
      Alcotest.failf "replacement lost short-circuit if shape: %s" replacement

let check_parity label source_text expression =
  let source, range, legacy = make_legacy source_text expression in
  let source_bytes = source_slice source range in
  let rule, plan = candidate source_bytes expression in
  let token = operator_name source_text expression in
  let replacement_operator, stable_rule = transition token in
  Alcotest.(check string)
    (label ^ " rule") stable_rule
    (Core.Operator.Rule.stable_name rule);
  Alcotest.(check string)
    (label ^ " raw source") source_bytes
    (Core.Operator.Spec.Replacement_plan.source_bytes plan);
  Alcotest.(check string)
    (label ^ " short-circuit bytes")
    (replacement_for source_text expression)
    (Core.Operator.Spec.Replacement_plan.replacement_bytes plan);
  Alcotest.(check string)
    (label ^ " semantic key")
    (Printf.sprintf "boolean-connective:%s->%s" token replacement_operator)
    (Core.Operator.Spec.Replacement_plan.semantic_key plan);
  let original_id = Core.Mutant.Id.full (Core.Mutant.id legacy) in
  verify source range expression legacy;
  Alcotest.(check string)
    (label ^ " production ID remains unchanged")
    original_id
    (Core.Mutant.Id.full (Core.Mutant.id legacy));
  plan

let test_registry_and_side_effect_shapes () =
  Compmisc.init_path ();
  let definitions =
    Core.Operator.Spec.For_testing.boolean_connective_specs ()
  in
  Alcotest.(check (list string))
    "ordered connective registry"
    [ "and-to-or@1"; "or-to-and@1" ]
    (List.map
       (fun definition ->
         Core.Operator.Spec.rule definition |> Core.Operator.Rule.stable_name)
       definitions);
  List.iter
    (fun definition ->
      let rule = Core.Operator.Spec.rule definition in
      Alcotest.(check bool)
        (Core.Operator.Rule.stable_name rule ^ " family")
        true
        (Core.Operator.Rule.family rule = Core.Operator.Boolean_connective);
      Alcotest.(check bool)
        (Core.Operator.Rule.stable_name rule ^ " remains Balanced")
        true
        (Core.Operator.Rule.profile rule = Core.Operator.Profile.Balanced))
    definitions;
  let fixtures =
    [
      ( "and",
        "let hits = ref 0 in let observe value = (incr hits; value) in observe \
         false && observe true",
        "&&" );
      ( "or",
        "let hits = ref 0 in let observe value = (incr hits; value) in observe \
         false || observe true",
        "||" );
    ]
  in
  List.iter
    (fun (label, source_text, token) ->
      let root = typed_expression source_text in
      let expression = find_connective source_text token root in
      let plan = check_parity label source_text expression in
      check_short_circuit_shape source_text expression plan)
    fixtures

let test_whitespace_comments_and_bool_alias () =
  Compmisc.init_path ();
  let commented = "((true (* left *))  &&  (false (* right *)))" in
  let expression = typed_expression commented in
  let plan = check_parity "comments" commented expression in
  let replacement =
    Core.Operator.Spec.Replacement_plan.replacement_bytes plan
  in
  let contains needle =
    let rec search offset =
      if offset + String.length needle > String.length replacement then false
      else if
        String.equal
          (String.sub replacement offset (String.length needle))
          needle
      then true
      else search (offset + 1)
    in
    search 0
  in
  Alcotest.(check bool)
    "comments remain within owned operand bytes" true
    (contains "left" && contains "right");
  let alias_environment = environment_after "type truth = bool" in
  let alias_source = "(true : truth) || (false : truth)" in
  let alias_expression =
    typed_expression ~environment:alias_environment alias_source
  in
  ignore (check_parity "manifest bool alias" alias_source alias_expression)

let test_stdlib_uid_and_local_shadow_boundary () =
  Compmisc.init_path ();
  let stdlib_source = "true && false" in
  let stdlib_expression = typed_expression stdlib_source in
  Alcotest.(check bool)
    "unqualified Stdlib connective resolves through its typed environment" true
    (is_resolved_stdlib_connective stdlib_expression "&&");
  ignore (check_parity "Stdlib &&" stdlib_source stdlib_expression);
  let shadowed_source =
    "let ( && ) left right = if left then right else true in true && false"
  in
  let root = typed_expression shadowed_source in
  let expression = find_connective shadowed_source "&&" root in
  let source_bytes =
    slice_location shadowed_source expression.Typedtree.exp_loc
  in
  Alcotest.(check int)
    "local connective UID is not a candidate" 0
    (List.length
       (Core.Operator.Spec.evaluate_boolean_connective ~source_bytes expression));
  let source, range, legacy = make_legacy shadowed_source expression in
  let error =
    expect_invariant "shadowed connective legacy candidate"
      (Engine.Ocaml_frontend.For_testing.verify_boolean_connective_shadow
         ~source ~path:(Core.Mutant.path legacy) ~range ~expression ~legacy)
  in
  Alcotest.(check (option string))
    "hypothetical shadowed legacy emission is fatal" (Some "not-applicable")
    (List.assoc_opt "decision" (Engine.Error.context error));
  let reject_local_export label source_text =
    let root = typed_expression source_text in
    let expression = find_connective source_text "&&" root in
    let source_bytes =
      slice_location source_text expression.Typedtree.exp_loc
    in
    Alcotest.(check int)
      label 0
      (List.length
         (Core.Operator.Spec.evaluate_boolean_connective ~source_bytes
            expression))
  in
  reject_local_export "local Stdlib module alias is conservative"
    "let module Alias = Stdlib in Alias.(true && false)";
  reject_local_export "local connective re-export is not Stdlib proof"
    "let module Reexport = struct let ( && ) = Stdlib.( && ) end in \
     Reexport.(true && false)"

let test_arity_type_source_and_identity_mismatch () =
  Compmisc.init_path ();
  let and_rule =
    get_ok
      (Core.Operator.rule_for_replacement Core.Operator.Boolean_connective
         ~original:"&&" ~replacement:"||")
  in
  let partial_source = "( && ) true" in
  let partial_expression = typed_expression partial_source in
  Alcotest.(check int)
    "partial connective has no candidate" 0
    (List.length
       (Core.Operator.Spec.evaluate_boolean_connective
          ~source_bytes:partial_source partial_expression));
  let partial_core_source, partial_range, partial_legacy =
    make_legacy_with ~source_text:partial_source ~expression:partial_expression
      ~rule:and_rule ~replacement:"false" ()
  in
  ignore
    (expect_invariant "partial connective legacy candidate"
       (Engine.Ocaml_frontend.For_testing.verify_boolean_connective_shadow
          ~source:partial_core_source
          ~path:(Core.Mutant.path partial_legacy)
          ~range:partial_range ~expression:partial_expression
          ~legacy:partial_legacy));
  let typed_source = "true && false" in
  let typed_expression = typed_expression typed_source in
  let core_source, range, legacy = make_legacy typed_source typed_expression in
  let wrong_type_expression =
    { typed_expression with Typedtree.exp_type = Predef.type_int }
  in
  Alcotest.(check int)
    "wrong connective result type has no candidate" 0
    (List.length
       (Core.Operator.Spec.evaluate_boolean_connective
          ~source_bytes:typed_source wrong_type_expression));
  ignore
    (expect_invariant "wrong-type connective legacy candidate"
       (Engine.Ocaml_frontend.For_testing.verify_boolean_connective_shadow
          ~source:core_source ~path:(Core.Mutant.path legacy) ~range
          ~expression:wrong_type_expression ~legacy));
  let stale_source_text = "true || false" in
  let stale_source, stale_range, stale_legacy =
    make_legacy_with ~source_text:stale_source_text ~expression:typed_expression
      ~rule:and_rule ~replacement:"(if (true) then true else (false))" ()
  in
  let stale_error =
    expect_invariant "connective source mismatch"
      (Engine.Ocaml_frontend.For_testing.verify_boolean_connective_shadow
         ~source:stale_source
         ~path:(Core.Mutant.path stale_legacy)
         ~range:stale_range ~expression:typed_expression ~legacy:stale_legacy)
  in
  Alcotest.(check (option string))
    "source mismatch is an explicit rejection"
    (Some "rejection:and-to-or@1:source-bytes-mismatch")
    (List.assoc_opt "decision" (Engine.Error.context stale_error));
  let _, _, wrong_replacement_legacy =
    make_legacy_with ~source_text:typed_source ~expression:typed_expression
      ~rule:and_rule ~replacement:"(if (true) then true else (false)) " ()
  in
  ignore
    (expect_invariant "connective replacement identity mismatch"
       (Engine.Ocaml_frontend.For_testing.verify_boolean_connective_shadow
          ~source:core_source
          ~path:(Core.Mutant.path wrong_replacement_legacy)
          ~range ~expression:typed_expression ~legacy:wrong_replacement_legacy))

let () =
  Alcotest.run "Boolean connective shadow parity contract"
    [
      ( "parity",
        [
          Alcotest.test_case "registry and side-effect shape" `Quick
            test_registry_and_side_effect_shapes;
          Alcotest.test_case "whitespace, comments, and bool alias" `Quick
            test_whitespace_comments_and_bool_alias;
          Alcotest.test_case "Stdlib UID versus local shadow" `Quick
            test_stdlib_uid_and_local_shadow_boundary;
          Alcotest.test_case "arity, type, source, and identity mismatch" `Quick
            test_arity_type_source_and_identity_mismatch;
        ] );
    ]
