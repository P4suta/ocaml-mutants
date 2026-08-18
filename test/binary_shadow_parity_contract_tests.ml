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
  Lexing.set_filename lexbuf "lib/binary_shadow_fixture.ml";
  try Typecore.type_expression environment (Parse.expression lexbuf)
  with exception_ ->
    Alcotest.failf "cannot type binary fixture %S: %s" source_text
      (Printexc.to_string exception_)

let environment_after source_text =
  let lexbuf = Lexing.from_string source_text in
  Lexing.set_filename lexbuf "lib/binary_shadow_types.ml";
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
      Alcotest.failf "cannot slice binary fixture: %a" Core.Source.pp_error
        error

let contains_substring ~needle value =
  let rec search offset =
    if offset + String.length needle > String.length value then false
    else if String.equal (String.sub value offset (String.length needle)) needle
    then true
    else search (offset + 1)
  in
  search 0

let operator_name source_text expression =
  match expression.Typedtree.exp_desc with
  | Typedtree.Texp_apply ({ exp_desc = Typedtree.Texp_ident _; exp_loc; _ }, _)
    ->
      String.trim (slice_location source_text exp_loc)
  | _ -> Alcotest.fail "fixture is not a binary operator application"

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

let transition = function
  | "=" -> (Core.Operator.Comparison, "<>", "eq-to-neq@1")
  | "<>" -> (Core.Operator.Comparison, "=", "neq-to-eq@1")
  | "<" -> (Core.Operator.Comparison, "<=", "lt-to-le@1")
  | "<=" -> (Core.Operator.Comparison, "<", "le-to-lt@1")
  | ">" -> (Core.Operator.Comparison, ">=", "gt-to-ge@1")
  | ">=" -> (Core.Operator.Comparison, ">", "ge-to-gt@1")
  | "+" -> (Core.Operator.Integer_arithmetic, "-", "int-add-to-sub@1")
  | "-" -> (Core.Operator.Integer_arithmetic, "+", "int-sub-to-add@1")
  | "*" -> (Core.Operator.Integer_arithmetic, "/", "int-mul-to-div@1")
  | "/" -> (Core.Operator.Integer_arithmetic, "*", "int-div-to-mul@1")
  | "+." -> (Core.Operator.Float_arithmetic, "-.", "float-add-to-sub@1")
  | "-." -> (Core.Operator.Float_arithmetic, "+.", "float-sub-to-add@1")
  | "*." -> (Core.Operator.Float_arithmetic, "/.", "float-mul-to-div@1")
  | "/." -> (Core.Operator.Float_arithmetic, "*.", "float-div-to-mul@1")
  | token -> Alcotest.failf "unsupported fixture token %S" token

let replacement_for source_text expression replacement_operator =
  match actual_arguments expression with
  | [ left; right ] ->
      Printf.sprintf "(Stdlib.( %s )) (%s) (%s)" replacement_operator
        (slice_location source_text left.Typedtree.exp_loc)
        (slice_location source_text right.Typedtree.exp_loc)
  | arguments ->
      Alcotest.failf "fixture has %d actual arguments, expected two"
        (List.length arguments)

let make_legacy_with ?(path = "lib/binary_shadow_fixture.ml") ~source_text
    ~expression ~rule ~replacement () =
  let source = Core.Source.of_string source_text in
  let range = range_of_expression expression in
  let unchecked =
    match Core.Mutant.unchecked ~path ~range ~rule ~replacement with
    | Ok unchecked -> unchecked
    | Error error ->
        Alcotest.failf "cannot construct binary legacy fixture: %a"
          Core.Mutant.pp_validation_error error
  in
  let legacy =
    match Core.Mutant.validate ~source unchecked with
    | Ok mutant -> mutant
    | Error error ->
        Alcotest.failf "cannot validate binary legacy fixture: %a"
          Core.Mutant.pp_validation_error error
  in
  (source, range, legacy)

let make_legacy ?path source_text expression =
  let original = operator_name source_text expression in
  let family, replacement_operator, _ = transition original in
  let rule =
    get_ok
      (Core.Operator.rule_for_replacement family ~original
         ~replacement:replacement_operator)
  in
  let replacement =
    replacement_for source_text expression replacement_operator
  in
  make_legacy_with ?path ~source_text ~expression ~rule ~replacement ()

let candidate source_bytes expression =
  match
    Core.Operator.Spec.evaluate_binary_operator ~source_bytes expression
  with
  | [ Core.Operator.Spec.Candidate { rule; plan } ] -> (rule, plan)
  | [ Core.Operator.Spec.Rejection { rule; reason } ] ->
      Alcotest.failf "binary fixture %s was rejected: %s"
        (Core.Operator.Rule.stable_name rule)
        (Core.Operator.Spec.rejection_name reason)
  | [] -> Alcotest.fail "binary fixture produced no shadow event"
  | decisions ->
      Alcotest.failf "binary fixture produced %d shadow events"
        (List.length decisions)

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

let verify source range expression legacy =
  match
    Engine.Ocaml_frontend.For_testing.verify_binary_shadow_event ~source
      ~path:(Core.Mutant.path legacy) ~range ~expression ~legacy:[ legacy ]
  with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "binary parity failed: %a" Engine.Error.pp error

let check_parity ?environment label source_text =
  let expression = typed_expression ?environment source_text in
  let source, range, legacy = make_legacy source_text expression in
  let source_bytes = source_slice source range in
  let rule, plan = candidate source_bytes expression in
  let original = operator_name source_text expression in
  let family, replacement_operator, stable_rule = transition original in
  Alcotest.(check string)
    (label ^ " stable rule") stable_rule
    (Core.Operator.Rule.stable_name rule);
  Alcotest.(check bool)
    (label ^ " family") true
    (Core.Operator.Rule.family rule = family);
  Alcotest.(check string)
    (label ^ " raw source") source_bytes
    (Core.Operator.Spec.Replacement_plan.source_bytes plan);
  Alcotest.(check string)
    (label ^ " replacement")
    (replacement_for source_text expression replacement_operator)
    (Core.Operator.Spec.Replacement_plan.replacement_bytes plan);
  Alcotest.(check string)
    (label ^ " semantic key")
    (Printf.sprintf "%s:%s->%s"
       (Core.Operator.Family.name family)
       original replacement_operator)
    (Core.Operator.Spec.Replacement_plan.semantic_key plan);
  let original_id = Core.Mutant.Id.full (Core.Mutant.id legacy) in
  verify source range expression legacy;
  Alcotest.(check string)
    (label ^ " production ID remains unchanged")
    original_id
    (Core.Mutant.Id.full (Core.Mutant.id legacy))

let test_registry_is_exactly_the_uniform_fourteen () =
  let expected =
    [
      "eq-to-neq@1";
      "neq-to-eq@1";
      "lt-to-le@1";
      "le-to-lt@1";
      "gt-to-ge@1";
      "ge-to-gt@1";
      "int-add-to-sub@1";
      "int-sub-to-add@1";
      "int-mul-to-div@1";
      "int-div-to-mul@1";
      "float-add-to-sub@1";
      "float-sub-to-add@1";
      "float-mul-to-div@1";
      "float-div-to-mul@1";
    ]
  in
  let definitions = Core.Operator.Spec.For_testing.binary_operator_specs () in
  let actual =
    List.map
      (fun definition ->
        Core.Operator.Spec.rule definition |> Core.Operator.Rule.stable_name)
      definitions
  in
  Alcotest.(check (list string)) "ordered registry" expected actual;
  List.iter
    (fun definition ->
      let rule = Core.Operator.Spec.rule definition in
      Alcotest.(check bool)
        (Core.Operator.Rule.stable_name rule ^ " remains Balanced")
        true
        (Core.Operator.Rule.profile rule = Core.Operator.Profile.Balanced);
      Alcotest.(check bool)
        (Core.Operator.Rule.stable_name rule ^ " excludes Boolean connective")
        true
        (Core.Operator.Rule.family rule <> Core.Operator.Boolean_connective))
    definitions

let test_all_fourteen_rules_have_exact_parity () =
  Compmisc.init_path ();
  [
    ("eq", "1 = 2");
    ("neq", "1 <> 2");
    ("lt", "1 < 2");
    ("le", "1 <= 2");
    ("gt", "1 > 2");
    ("ge", "1 >= 2");
    ("int add", "1 + 2");
    ("int sub", "1 - 2");
    ("int mul", "1 * 2");
    ("int div", "1 / 2");
    ("float add", "1.0 +. 2.0");
    ("float sub", "1.0 -. 2.0");
    ("float mul", "1.0 *. 2.0");
    ("float div", "1.0 /. 2.0");
  ]
  |> List.iter (fun (label, source_text) -> check_parity label source_text)

let test_whitespace_comments_and_aliases () =
  Compmisc.init_path ();
  let commented = "((1 (* left *))  +  (2 (* right *)))" in
  let expression = typed_expression commented in
  let source, range, legacy = make_legacy commented expression in
  let source_bytes = source_slice source range in
  let _, plan = candidate source_bytes expression in
  Alcotest.(check bool)
    "comments are retained in operand slices" true
    (let replacement =
       Core.Operator.Spec.Replacement_plan.replacement_bytes plan
     in
     contains_substring ~needle:"left" replacement
     && contains_substring ~needle:"right" replacement);
  verify source range expression legacy;
  let alias_environment = environment_after "type count = int" in
  check_parity ~environment:alias_environment "manifest int alias"
    "(1 : count) + (2 : count)"

let rec find_application source_text token expression =
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
  | None -> Alcotest.failf "cannot find operator %S in %S" token source_text

let application_value_reference expression =
  match expression.Typedtree.exp_desc with
  | Typedtree.Texp_apply
      ({ exp_desc = Typedtree.Texp_ident (path, _, description); _ }, _) ->
      (path, description)
  | _ -> Alcotest.fail "fixture is not a value-backed application"

let is_resolved_stdlib_operator expression name =
  let path, description = application_value_reference expression in
  Core.Operator.Spec.Typed_evidence.is_resolved_stdlib_value ~path ~description
    ~environment:expression.Typedtree.exp_env ~name

let test_stdlib_uid_and_shadowed_value_boundary () =
  Compmisc.init_path ();
  let stdlib_source = "Stdlib.(1 + 2)" in
  let stdlib_root = typed_expression stdlib_source in
  let stdlib_expression = find_application stdlib_source "+" stdlib_root in
  Alcotest.(check bool)
    "qualified Stdlib operator resolves through its typed environment" true
    (is_resolved_stdlib_operator stdlib_expression "+");
  let source, range, legacy = make_legacy stdlib_source stdlib_expression in
  verify source range stdlib_expression legacy;
  let shadowed_source = "let ( + ) left right = left -. right in 1.0 + 2.0" in
  let shadowed_root = typed_expression shadowed_source in
  let shadowed_expression =
    find_application shadowed_source "+" shadowed_root
  in
  let source_bytes =
    slice_location shadowed_source shadowed_expression.Typedtree.exp_loc
  in
  Alcotest.(check int)
    "local operator UID is not a Spec candidate" 0
    (List.length
       (Core.Operator.Spec.evaluate_binary_operator ~source_bytes
          shadowed_expression));
  let shadow_source, shadow_range, shadow_legacy =
    make_legacy shadowed_source shadowed_expression
  in
  let error =
    expect_invariant "shadowed legacy candidate"
      (Engine.Ocaml_frontend.For_testing.verify_binary_shadow
         ~source:shadow_source
         ~path:(Core.Mutant.path shadow_legacy)
         ~range:shadow_range ~expression:shadowed_expression
         ~legacy:shadow_legacy)
  in
  Alcotest.(check (option string))
    "shadowed operator is fatal after a hypothetical legacy emission"
    (Some "not-applicable")
    (List.assoc_opt "decision" (Engine.Error.context error));
  let reject_local_export label source_text =
    let root = typed_expression source_text in
    let expression = find_application source_text "+" root in
    let source_bytes =
      slice_location source_text expression.Typedtree.exp_loc
    in
    Alcotest.(check int)
      label 0
      (List.length
         (Core.Operator.Spec.evaluate_binary_operator ~source_bytes expression));
    expression
  in
  ignore
    (reject_local_export "local module alias is conservative"
       "let module Alias = Stdlib in Alias.(1 + 2)");
  ignore
    (reject_local_export "fixture-style local Stdlib lookalike is rejected"
       "let module Local_stdlib = struct let ( + ) left right = left - right \
        end in Local_stdlib.(1 + 2)");
  let reexported_expression =
    reject_local_export "local re-export is not Stdlib proof"
      "let module Reexport = struct let ( + ) = Stdlib.( + ) end in \
       Reexport.(1 + 2)"
  in
  let stdlib_path, stdlib_description =
    application_value_reference stdlib_expression
  in
  let _, reexported_description =
    application_value_reference reexported_expression
  in
  Alcotest.(check bool)
    "forged Typedtree/environment UID mismatch is rejected" false
    (Core.Operator.Spec.Typed_evidence.is_resolved_stdlib_value
       ~path:stdlib_path ~description:reexported_description
       ~environment:stdlib_expression.Typedtree.exp_env ~name:"+");
  Alcotest.(check bool)
    "unresolvable same-name path is rejected" false
    (Core.Operator.Spec.Typed_evidence.is_resolved_stdlib_value
       ~path:(Path.Pident (Ident.create_local "+"))
       ~description:stdlib_description
       ~environment:stdlib_expression.Typedtree.exp_env ~name:"+")

let test_arity_type_and_source_mismatch_are_not_candidates () =
  Compmisc.init_path ();
  let add_rule =
    get_ok
      (Core.Operator.rule_for_replacement Core.Operator.Integer_arithmetic
         ~original:"+" ~replacement:"-")
  in
  let partial_source = "( + ) 1" in
  let partial_expression = typed_expression partial_source in
  Alcotest.(check int)
    "partial application has no candidate" 0
    (List.length
       (Core.Operator.Spec.evaluate_binary_operator ~source_bytes:partial_source
          partial_expression));
  let partial_core_source, partial_range, partial_legacy =
    make_legacy_with ~source_text:partial_source ~expression:partial_expression
      ~rule:add_rule ~replacement:"0" ()
  in
  ignore
    (expect_invariant "partial legacy candidate"
       (Engine.Ocaml_frontend.For_testing.verify_binary_shadow
          ~source:partial_core_source
          ~path:(Core.Mutant.path partial_legacy)
          ~range:partial_range ~expression:partial_expression
          ~legacy:partial_legacy));
  let typed_source = "1 + 2" in
  let typed_expression = typed_expression typed_source in
  let core_source, range, legacy = make_legacy typed_source typed_expression in
  let wrong_type_expression =
    { typed_expression with Typedtree.exp_type = Predef.type_bool }
  in
  Alcotest.(check int)
    "wrong result type has no candidate" 0
    (List.length
       (Core.Operator.Spec.evaluate_binary_operator ~source_bytes:typed_source
          wrong_type_expression));
  ignore
    (expect_invariant "wrong-type legacy candidate"
       (Engine.Ocaml_frontend.For_testing.verify_binary_shadow
          ~source:core_source ~path:(Core.Mutant.path legacy) ~range
          ~expression:wrong_type_expression ~legacy));
  let stale_source_text = "1 - 2" in
  let stale_source, stale_range, stale_legacy =
    make_legacy_with ~source_text:stale_source_text ~expression:typed_expression
      ~rule:add_rule ~replacement:"(Stdlib.( - )) (1) (2)" ()
  in
  let stale_error =
    expect_invariant "source mismatch"
      (Engine.Ocaml_frontend.For_testing.verify_binary_shadow
         ~source:stale_source
         ~path:(Core.Mutant.path stale_legacy)
         ~range:stale_range ~expression:typed_expression ~legacy:stale_legacy)
  in
  Alcotest.(check (option string))
    "source mismatch is an explicit rejection"
    (Some "rejection:int-add-to-sub@1:source-bytes-mismatch")
    (List.assoc_opt "decision" (Engine.Error.context stale_error))

let () =
  Alcotest.run "Binary operator shadow parity contract"
    [
      ( "parity",
        [
          Alcotest.test_case "exact fourteen-rule registry" `Quick
            test_registry_is_exactly_the_uniform_fourteen;
          Alcotest.test_case "all fourteen rules" `Quick
            test_all_fourteen_rules_have_exact_parity;
          Alcotest.test_case "whitespace, comments, and aliases" `Quick
            test_whitespace_comments_and_aliases;
          Alcotest.test_case "Stdlib UID versus shadowed value" `Quick
            test_stdlib_uid_and_shadowed_value_boundary;
          Alcotest.test_case "arity, type, and source mismatch" `Quick
            test_arity_type_and_source_mismatch_are_not_candidates;
        ] );
    ]
