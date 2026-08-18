module Core = Ocaml_mutants_core
module Engine = Ocaml_mutants_engine

let get_ok = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let typed_expression source_text =
  let lexbuf = Lexing.from_string source_text in
  Lexing.set_filename lexbuf "lib/if_branch_shadow_fixture.ml";
  try
    Typecore.type_expression (Compmisc.initial_env ()) (Parse.expression lexbuf)
  with exception_ ->
    Alcotest.failf "cannot type if-branch fixture %S: %s" source_text
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

let slice_location source_text location =
  let start_byte = location.Location.loc_start.Lexing.pos_cnum in
  let end_byte = location.loc_end.pos_cnum in
  if
    start_byte < 0
    || end_byte > String.length source_text
    || start_byte >= end_byte
  then
    Alcotest.failf "invalid branch location %d..%d for %S" start_byte end_byte
      source_text
  else String.sub source_text start_byte (end_byte - start_byte)

let find_if expression =
  match expression.Typedtree.exp_desc with
  | Typedtree.Texp_ifthenelse (_, _, Some _) -> expression
  | _ -> (
      let found = ref None in
      let expr iterator expression =
        (match expression.Typedtree.exp_desc with
        | Typedtree.Texp_ifthenelse (_, _, Some _) when !found = None ->
            found := Some expression
        | _ -> ());
        Tast_iterator.default_iterator.expr iterator expression
      in
      let iterator = { Tast_iterator.default_iterator with expr } in
      iterator.expr iterator expression;
      match !found with
      | Some conditional -> conditional
      | None -> Alcotest.fail "fixture has no full if")

let branches conditional =
  match conditional.Typedtree.exp_desc with
  | Typedtree.Texp_ifthenelse (_, then_branch, Some else_branch) ->
      (then_branch, else_branch)
  | _ -> Alcotest.fail "fixture is not a full if"

let rule stable_name = get_ok (Core.Operator.Rule.of_stable_name stable_name)

let make_legacy ?(path = "lib/if_branch_shadow_fixture.ml") ~source_text
    ~expression ~rule ~replacement () =
  let source = Core.Source.of_string source_text in
  let range = range_of_expression expression in
  let unchecked =
    match Core.Mutant.unchecked ~path ~range ~rule ~replacement with
    | Ok unchecked -> unchecked
    | Error error ->
        Alcotest.failf "cannot construct if-branch legacy fixture: %a"
          Core.Mutant.pp_validation_error error
  in
  let legacy =
    match Core.Mutant.validate ~source unchecked with
    | Ok mutant -> mutant
    | Error error ->
        Alcotest.failf "cannot validate if-branch legacy fixture: %a"
          Core.Mutant.pp_validation_error error
  in
  (source, range, legacy)

let candidate ~source_text ~target conditional =
  match
    Core.Operator.Spec.evaluate_if_branch ~source_bytes:source_text ~target
      conditional
  with
  | [ Core.Operator.Spec.Candidate { rule; plan } ] -> (rule, plan)
  | [ Core.Operator.Spec.Rejection { rule; reason } ] ->
      Alcotest.failf "if-branch fixture %s was rejected: %s"
        (Core.Operator.Rule.stable_name rule)
        (Core.Operator.Spec.rejection_name reason)
  | [] -> Alcotest.fail "if-branch fixture produced no shadow event"
  | decisions ->
      Alcotest.failf "if-branch fixture produced %d shadow events"
        (List.length decisions)

let verify source range conditional target legacy =
  match
    Engine.Ocaml_frontend.For_testing.verify_if_branch_shadow_event ~source
      ~path:(Core.Mutant.path legacy) ~range ~conditional ~target
      ~legacy:[ legacy ]
  with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "if-branch parity failed: %a" Engine.Error.pp error

let expect_invariant label = function
  | Ok () -> Alcotest.failf "%s unexpectedly preserved parity" label
  | Error error ->
      Alcotest.(check string)
        (label ^ " cause") "invariant-violation"
        (Engine.Error.cause_name (Engine.Error.cause error));
      error

let check_target label source_text conditional target =
  let then_branch, else_branch = branches conditional in
  let target_expression, retained_expression, stable_rule =
    match target with
    | Core.Operator.Spec.Then_branch ->
        (then_branch, else_branch, "select-else-branch@1")
    | Core.Operator.Spec.Else_branch ->
        (else_branch, then_branch, "select-then-branch@1")
  in
  let original = slice_location source_text target_expression.exp_loc in
  let replacement = slice_location source_text retained_expression.exp_loc in
  let expected_rule = rule stable_rule in
  let actual_rule, plan = candidate ~source_text ~target conditional in
  Alcotest.(check bool)
    (label ^ " rule") true
    (Core.Operator.Rule.equal expected_rule actual_rule);
  Alcotest.(check string)
    (label ^ " raw target") original
    (Core.Operator.Spec.Replacement_plan.source_bytes plan);
  Alcotest.(check string)
    (label ^ " opposite branch bytes")
    replacement
    (Core.Operator.Spec.Replacement_plan.replacement_bytes plan);
  Alcotest.(check string)
    (label ^ " target-shape key")
    "replacement:opposite-if-branch"
    (Core.Operator.Spec.Replacement_plan.semantic_key plan);
  let source, range, legacy =
    make_legacy ~source_text ~expression:target_expression ~rule:expected_rule
      ~replacement ()
  in
  let full_id = Core.Mutant.Id.full (Core.Mutant.id legacy) in
  verify source range conditional target legacy;
  Alcotest.(check string)
    (label ^ " production ID") full_id
    (Core.Mutant.Id.full (Core.Mutant.id legacy))

let test_registry_and_effectful_branches () =
  Compmisc.init_path ();
  Alcotest.(check (list string))
    "ordered registry"
    [ "select-then-branch@1"; "select-else-branch@1" ]
    (Core.Operator.Spec.For_testing.if_branch_specs ()
    |> List.map (fun definition ->
        Core.Operator.Spec.rule definition |> Core.Operator.Rule.stable_name));
  let source_text =
    "let hits = ref 0 in let touch x = (incr hits; x) in if true then ((ignore \
     (touch 1); 2 (* then *)) [@warning \"-32\"]) else (ignore (touch 3); 4 (* \
     else *))"
  in
  let conditional = typed_expression source_text |> find_if in
  check_target "replace then with else" source_text conditional
    Core.Operator.Spec.Then_branch;
  check_target "replace else with then" source_text conditional
    Core.Operator.Spec.Else_branch

let test_not_applicable_and_source_mismatch () =
  Compmisc.init_path ();
  let partial_source = "if true then ()" in
  let partial = typed_expression partial_source in
  List.iter
    (fun target ->
      Alcotest.(check int)
        "if without else is silent" 0
        (List.length
           (Core.Operator.Spec.evaluate_if_branch ~source_bytes:partial_source
              ~target partial)))
    [ Core.Operator.Spec.Then_branch; Core.Operator.Spec.Else_branch ];
  let source_text = "if true then 1 else 2" in
  let conditional = typed_expression source_text in
  let blank_source = String.make (String.length source_text) ' ' in
  match
    Core.Operator.Spec.evaluate_if_branch ~source_bytes:blank_source
      ~target:Core.Operator.Spec.Then_branch conditional
  with
  | [ Core.Operator.Spec.Rejection { reason; _ } ] ->
      Alcotest.(check string)
        "unowned branch bytes" "source-bytes-mismatch"
        (Core.Operator.Spec.rejection_name reason)
  | _ -> Alcotest.fail "invalid branch source was not rejected once"

let test_range_and_replacement_mismatch_are_fatal () =
  Compmisc.init_path ();
  let source_text = "if true then 1 else 2" in
  let conditional = typed_expression source_text in
  let then_branch, else_branch = branches conditional in
  let then_bytes = slice_location source_text then_branch.exp_loc in
  let else_bytes = slice_location source_text else_branch.exp_loc in
  let select_else = rule "select-else-branch@1" in
  let source, wrong_range, wrong_legacy =
    make_legacy ~source_text ~expression:else_branch ~rule:select_else
      ~replacement:then_bytes ()
  in
  ignore
    (expect_invariant "range mismatch"
       (Engine.Ocaml_frontend.For_testing.verify_if_branch_shadow ~source
          ~path:(Core.Mutant.path wrong_legacy)
          ~range:wrong_range ~conditional ~target:Core.Operator.Spec.Then_branch
          ~legacy:wrong_legacy));
  let source, range, wrong_replacement =
    make_legacy ~source_text ~expression:then_branch ~rule:select_else
      ~replacement:(else_bytes ^ " ") ()
  in
  ignore
    (expect_invariant "legacy rendering mismatch"
       (Engine.Ocaml_frontend.For_testing.verify_if_branch_shadow ~source
          ~path:(Core.Mutant.path wrong_replacement)
          ~range ~conditional ~target:Core.Operator.Spec.Then_branch
          ~legacy:wrong_replacement))

let () =
  Alcotest.run "If-branch shadow parity contract"
    [
      ( "if-branch",
        [
          Alcotest.test_case "registry and effectful raw branches" `Quick
            test_registry_and_effectful_branches;
          Alcotest.test_case "Not_applicable and source mismatch" `Quick
            test_not_applicable_and_source_mismatch;
          Alcotest.test_case "range and rendering mismatch" `Quick
            test_range_and_replacement_mismatch_are_fatal;
        ] );
    ]
