module Core = Ocaml_mutants_core
module Engine = Ocaml_mutants_engine

let get_ok = function
  | Ok value -> value
  | Error message -> Alcotest.fail message

let remove_if_present path = try Sys.remove path with Sys_error _ -> ()

let with_serialized_cmt ~source use =
  let directory =
    Filename.temp_dir ~perms:0o700 "ocaml-mutants-return-refutation-" ".tmp"
  in
  let source_path = Filename.concat directory "return_refutation_fixture.ml" in
  let cmi_path = Filename.concat directory "return_refutation_fixture.cmi" in
  let cmo_path = Filename.concat directory "return_refutation_fixture.cmo" in
  let cmt_path = Filename.concat directory "return_refutation_fixture.cmt" in
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
      Alcotest.(check int) "fixture compiler exit" 0 (Sys.command command);
      Alcotest.(check bool)
        "fixture emitted a serialized CMT" true (Sys.file_exists cmt_path);
      use ~root:directory ~cmt_path)

let unreachable_expression cmt_path =
  let _, information = Cmt_format.read cmt_path in
  let structure =
    match information with
    | Some { Cmt_format.cmt_annots = Implementation structure; _ } -> structure
    | _ -> Alcotest.fail "refutation fixture CMT is not an implementation"
  in
  let unreachable = ref [] in
  let expr iterator expression =
    (match expression.Typedtree.exp_desc with
    | Typedtree.Texp_unreachable -> unreachable := expression :: !unreachable
    | _ -> ());
    Tast_iterator.default_iterator.expr iterator expression
  in
  let iterator = { Tast_iterator.default_iterator with expr } in
  iterator.structure iterator structure;
  match !unreachable with
  | [ expression ] -> expression
  | expressions ->
      Alcotest.failf "expected one refutation RHS, found %d"
        (List.length expressions)

let typed_expression ?environment source_text =
  let environment =
    Option.value environment ~default:(Compmisc.initial_env ())
  in
  let lexbuf = Lexing.from_string source_text in
  Lexing.set_filename lexbuf "lib/return_shadow_fixture.ml";
  try Typecore.type_expression environment (Parse.expression lexbuf)
  with exception_ ->
    Alcotest.failf "cannot type return fixture %S: %s" source_text
      (Printexc.to_string exception_)

let environment_after source_text =
  let lexbuf = Lexing.from_string source_text in
  Lexing.set_filename lexbuf "lib/return_shadow_types.ml";
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
      Alcotest.failf "cannot slice return fixture: %a" Core.Source.pp_error
        error

let direct_function_body expression =
  match expression.Typedtree.exp_desc with
  | Typedtree.Texp_function (_, Typedtree.Tfunction_body body) -> body
  | Typedtree.Texp_function
      (_, Typedtree.Tfunction_cases { cases = case :: _; _ }) ->
      case.Typedtree.c_rhs
  | _ -> Alcotest.fail "fixture is not a function"

let find_function_body expression =
  match expression.Typedtree.exp_desc with
  | Typedtree.Texp_function _ -> direct_function_body expression
  | _ -> (
      let found = ref None in
      let expr iterator expression =
        (match expression.Typedtree.exp_desc with
        | Typedtree.Texp_function _ when !found = None ->
            found := Some (direct_function_body expression)
        | _ -> ());
        Tast_iterator.default_iterator.expr iterator expression
      in
      let iterator = { Tast_iterator.default_iterator with expr } in
      iterator.expr iterator expression;
      match !found with
      | Some body -> body
      | None -> Alcotest.fail "fixture has no function body")

let make_legacy ?(path = "lib/return_shadow_fixture.ml") ~source_text
    ~expression ~rule ~replacement () =
  let source = Core.Source.of_string source_text in
  let range = range_of_expression expression in
  let unchecked =
    match Core.Mutant.unchecked ~path ~range ~rule ~replacement with
    | Ok unchecked -> unchecked
    | Error error ->
        Alcotest.failf "cannot construct return legacy fixture: %a"
          Core.Mutant.pp_validation_error error
  in
  let legacy =
    match Core.Mutant.validate ~source unchecked with
    | Ok mutant -> mutant
    | Error error ->
        Alcotest.failf "cannot validate return legacy fixture: %a"
          Core.Mutant.pp_validation_error error
  in
  (source, range, legacy)

let candidates source_bytes expression =
  Core.Operator.Spec.evaluate_return_replacement ~source_bytes expression
  |> List.map (function
    | Core.Operator.Spec.Candidate { rule; plan } -> (rule, plan)
    | Core.Operator.Spec.Rejection { rule; reason } ->
        Alcotest.failf "return fixture %s was rejected: %s"
          (Core.Operator.Rule.stable_name rule)
          (Core.Operator.Spec.rejection_name reason))

let verify_event source range expression legacy =
  let path =
    match legacy with
    | first :: _ -> Core.Mutant.path first
    | [] -> Alcotest.fail "return event fixture has no legacy candidates"
  in
  match
    Engine.Ocaml_frontend.For_testing.verify_return_shadow_event ~source ~path
      ~range ~expression ~legacy
  with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "return parity failed: %a" Engine.Error.pp error

let expect_invariant label = function
  | Ok () -> Alcotest.failf "%s unexpectedly preserved parity" label
  | Error error ->
      Alcotest.(check string)
        (label ^ " cause") "invariant-violation"
        (Engine.Error.cause_name (Engine.Error.cause error));
      error

let check_fixture ?environment label source_text expected =
  let root = typed_expression ?environment source_text in
  let body = find_function_body root in
  let source = Core.Source.of_string source_text in
  let range = range_of_expression body in
  let source_bytes = source_slice source range in
  let actual = candidates source_bytes body in
  Alcotest.(check (list string))
    (label ^ " rules") (List.map fst expected)
    (List.map (fun (rule, _) -> Core.Operator.Rule.stable_name rule) actual);
  let legacy =
    List.map2
      (fun (stable_rule, replacement) (rule, plan) ->
        Alcotest.(check string)
          (label ^ " raw source") source_bytes
          (Core.Operator.Spec.Replacement_plan.source_bytes plan);
        Alcotest.(check string)
          (label ^ " replacement bytes")
          replacement
          (Core.Operator.Spec.Replacement_plan.replacement_bytes plan);
        Alcotest.(check string)
          (label ^ " replacement key")
          ("replacement:" ^ replacement)
          (Core.Operator.Spec.Replacement_plan.semantic_key plan);
        let expected_rule =
          get_ok (Core.Operator.Rule.of_stable_name stable_rule)
        in
        Alcotest.(check bool)
          (label ^ " rule identity") true
          (Core.Operator.Rule.equal expected_rule rule);
        let source, range, legacy =
          make_legacy ~source_text ~expression:body ~rule ~replacement ()
        in
        let full_id = Core.Mutant.Id.full (Core.Mutant.id legacy) in
        Alcotest.(check string)
          (label ^ " production ID") full_id
          (Core.Mutant.Id.full (Core.Mutant.id legacy));
        legacy)
      expected actual
  in
  verify_event source range body legacy

let test_registry_and_all_eight_rules () =
  Compmisc.init_path ();
  Alcotest.(check (list string))
    "ordered registry"
    [
      "return-unit@1";
      "return-false@1";
      "return-true@1";
      "return-zero@1";
      "return-float-zero@1";
      "return-empty-string@1";
      "return-empty-list@1";
      "return-none@1";
    ]
    (Core.Operator.Spec.For_testing.return_replacement_specs ()
    |> List.map (fun definition ->
        Core.Operator.Spec.rule definition |> Core.Operator.Rule.stable_name));
  [
    ( "unit effect",
      "fun () -> print_endline \"effect\"",
      [ ("return-unit@1", "()") ] );
    ( "Boolean expression",
      "fun () -> 1 = 2",
      [ ("return-false@1", "false"); ("return-true@1", "true") ] );
    ( "Boolean identifier",
      "fun (value : bool) -> value",
      [ ("return-false@1", "false"); ("return-true@1", "true") ] );
    ("Boolean true literal", "fun () -> true", [ ("return-false@1", "false") ]);
    ("Boolean false literal", "fun () -> false", [ ("return-true@1", "true") ]);
    ("integer", "fun () -> 41 + 1", [ ("return-zero@1", "0") ]);
    ("float", "fun () -> 1.5", [ ("return-float-zero@1", "0.0") ]);
    ("string", "fun () -> \"value\"", [ ("return-empty-string@1", "\"\"") ]);
    ("list", "fun () -> [ 1 ]", [ ("return-empty-list@1", "[]") ]);
    ("option", "fun () -> Some 1", [ ("return-none@1", "None") ]);
  ]
  |> List.iter (fun (label, source_text, expected) ->
      check_fixture label source_text expected)

module Stable_name_map = Map.Make (String)

let stable_name_counts names =
  List.fold_left
    (fun counts name ->
      let count =
        Option.value (Stable_name_map.find_opt name counts) ~default:0
      in
      Stable_name_map.add name (count + 1) counts)
    Stable_name_map.empty names

let definition_coverage_errors ~registered_names ~definition_names =
  let registered_counts = stable_name_counts registered_names in
  let definition_counts = stable_name_counts definition_names in
  let errors = ref [] in
  Stable_name_map.iter
    (fun name registered_count ->
      if registered_count <> 1 then
        errors :=
          Printf.sprintf "registered stable rule %s occurs %d times" name
            registered_count
          :: !errors;
      let definition_count =
        Option.value
          (Stable_name_map.find_opt name definition_counts)
          ~default:0
      in
      if definition_count <> 1 then
        errors :=
          Printf.sprintf "stable rule %s has %d definitions" name
            definition_count
          :: !errors)
    registered_counts;
  Stable_name_map.iter
    (fun name definition_count ->
      if not (Stable_name_map.mem name registered_counts) then
        errors :=
          Printf.sprintf "unregistered stable rule %s has %d definitions" name
            definition_count
          :: !errors)
    definition_counts;
  List.sort String.compare !errors

let test_all_thirty_registered_rules_have_a_shadow_definition () =
  let definitions = Core.Operator.Spec.For_testing.all_specs () in
  let registered_names =
    List.map Core.Operator.Rule.stable_name Core.Operator.Rule.all
  in
  let definition_names =
    definitions
    |> List.map (fun definition ->
        Core.Operator.Spec.rule definition |> Core.Operator.Rule.stable_name)
  in
  Alcotest.(check int)
    "production registry size" 30
    (List.length registered_names);
  Alcotest.(check int) "Spec registry size" 30 (List.length definition_names);
  Alcotest.(check (list string))
    "every registered stable rule has exactly one definition" []
    (definition_coverage_errors ~registered_names ~definition_names);
  Alcotest.(check (list string))
    "Spec registry is the ordered production registry" registered_names
    definition_names

let test_registry_owned_compatibility_projections () =
  Compmisc.init_path ();
  let binary_transitions =
    [
      ("&&", "||", "and-to-or@1");
      ("||", "&&", "or-to-and@1");
      ("=", "<>", "eq-to-neq@1");
      ("<>", "=", "neq-to-eq@1");
      ("<", "<=", "lt-to-le@1");
      ("<=", "<", "le-to-lt@1");
      (">", ">=", "gt-to-ge@1");
      (">=", ">", "ge-to-gt@1");
      ("+", "-", "int-add-to-sub@1");
      ("-", "+", "int-sub-to-add@1");
      ("*", "/", "int-mul-to-div@1");
      ("/", "*", "int-div-to-mul@1");
      ("+.", "-.", "float-add-to-sub@1");
      ("-.", "+.", "float-sub-to-add@1");
      ("*.", "/.", "float-mul-to-div@1");
      ("/.", "*.", "float-div-to-mul@1");
    ]
  in
  List.iter
    (fun (original, replacement, stable_name) ->
      let transition =
        match Core.Operator.Spec.find_binary_transition ~original with
        | Some transition -> transition
        | None -> Alcotest.failf "missing binary projection for %S" original
      in
      Alcotest.(check string)
        (original ^ " replacement operator")
        replacement
        (Core.Operator.Spec.binary_transition_replacement transition);
      Alcotest.(check string)
        (original ^ " rule") stable_name
        (Core.Operator.Spec.binary_transition_rule transition
        |> Core.Operator.Rule.stable_name);
      let expected_rendering =
        match original with
        | "&&" -> "(if (left ()) then true else (right ()))"
        | "||" -> "(if (left ()) then (right ()) else false)"
        | _ -> Printf.sprintf "(Stdlib.( %s )) (left ()) (right ())" replacement
      in
      Alcotest.(check string)
        (original ^ " rendering") expected_rendering
        (Core.Operator.Spec.render_binary_transition transition ~left:"left ()"
           ~right:"right ()"))
    binary_transitions;
  Alcotest.(check bool)
    "unknown token has no projection" true
    (Option.is_none
       (Core.Operator.Spec.find_binary_transition ~original:"custom"));
  Alcotest.(check (list string))
    "binary projection rejects a duplicate original"
    [ "duplicate binary original operator \"&&\"" ]
    (Core.Operator.Spec.For_testing.binary_original_invariant_errors
       [ "&&"; "||"; "&&" ]);
  Alcotest.(check (list string))
    "registered binary originals are unique" []
    (Core.Operator.Spec.For_testing.binary_original_invariant_errors
       (List.map (fun (original, _, _) -> original) binary_transitions));
  Alcotest.(check string)
    "then target selects the else branch" "select-else-branch@1"
    (Core.Operator.Spec.if_branch_rule Core.Operator.Spec.Then_branch
    |> Core.Operator.Rule.stable_name);
  Alcotest.(check string)
    "else target selects the then branch" "select-then-branch@1"
    (Core.Operator.Spec.if_branch_rule Core.Operator.Spec.Else_branch
    |> Core.Operator.Rule.stable_name);
  Alcotest.(check (list string))
    "branch projection rejects a missing target"
    [ "if-branch target else has 0 registry entries" ]
    (Core.Operator.Spec.For_testing.if_branch_target_invariant_errors
       [ Core.Operator.Spec.Then_branch ]);
  Alcotest.(check (list string))
    "branch projection rejects a duplicate target"
    [ "if-branch target then has 2 registry entries" ]
    (Core.Operator.Spec.For_testing.if_branch_target_invariant_errors
       [
         Core.Operator.Spec.Then_branch;
         Core.Operator.Spec.Then_branch;
         Core.Operator.Spec.Else_branch;
       ]);
  Alcotest.(check (list string))
    "registered branch targets are total and unique" []
    (Core.Operator.Spec.For_testing.if_branch_target_invariant_errors
       [ Core.Operator.Spec.Then_branch; Core.Operator.Spec.Else_branch ]);
  [
    ("true", [ "false"; "true" ]);
    ("0", [ "0" ]);
    ("0.0", [ "0.0" ]);
    ("\"value\"", [ "\"\"" ]);
    ("()", [ "()" ]);
    ("[ 1 ]", [ "[]" ]);
    ("Some 1", [ "None" ]);
  ]
  |> List.iter (fun (source_text, expected) ->
      let expression = typed_expression source_text in
      Alcotest.(check (list string))
        (source_text ^ " neutral projection")
        expected
        (Core.Operator.Spec.neutral_return_replacements expression.exp_type);
      Alcotest.(check (list string))
        (source_text ^ " typedtree adapter projection")
        expected
        (Engine.Typedtree_compat.neutral_replacements expression.exp_type));
  let precedence =
    List.sort Core.Operator.Spec.compare_deduplication_precedence
      Core.Operator.Family.all
    |> List.map Core.Operator.Family.name
  in
  Alcotest.(check (list string))
    "exact-transformation precedence"
    [
      "boolean-literal";
      "boolean-connective";
      "comparison";
      "integer-arithmetic";
      "float-arithmetic";
      "condition-negation";
      "if-branch";
      "sequence-deletion";
      "return-replacement";
    ]
    precedence

let test_definition_coverage_rejects_duplicate_and_missing () =
  let registered_names = [ "first@1"; "second@1" ] in
  Alcotest.(check (list string))
    "missing definition is reported"
    [ "stable rule second@1 has 0 definitions" ]
    (definition_coverage_errors ~registered_names
       ~definition_names:[ "first@1" ]);
  Alcotest.(check (list string))
    "duplicate definition is reported"
    [ "stable rule first@1 has 2 definitions" ]
    (definition_coverage_errors ~registered_names
       ~definition_names:[ "first@1"; "first@1"; "second@1" ])

let test_alias_comments_attributes_and_abstract_boundary () =
  Compmisc.init_path ();
  let environment = environment_after "type count = int" in
  check_fixture ~environment "manifest int alias"
    "fun () -> (((41 + 1) (* keep *)) [@warning \"-32\"] : count)"
    [ ("return-zero@1", "0") ];
  let abstract_source =
    "let module M : sig type t val value : t end = struct type t = int let \
     value = 1 end in ignore ((fun () -> M.value) ())"
  in
  let body = typed_expression abstract_source |> find_function_body in
  let source = Core.Source.of_string abstract_source in
  let source_bytes = source_slice source (range_of_expression body) in
  Alcotest.(check int)
    "abstract head is silent" 0
    (List.length
       (Core.Operator.Spec.evaluate_return_replacement ~source_bytes body));
  let neutral = typed_expression "fun () -> 0" |> find_function_body in
  Alcotest.(check int)
    "byte-identical neutral return is silent" 0
    (List.length
       (Core.Operator.Spec.evaluate_return_replacement ~source_bytes:"0" neutral))

let test_source_range_and_render_mismatch () =
  Compmisc.init_path ();
  let source_text = "fun () -> 42" in
  let root = typed_expression source_text in
  let body = direct_function_body root in
  (match
     Core.Operator.Spec.evaluate_return_replacement ~source_bytes:"(" body
   with
  | [ Core.Operator.Spec.Rejection { reason; _ } ] ->
      Alcotest.(check string)
        "malformed source rejection" "source-bytes-mismatch"
        (Core.Operator.Spec.rejection_name reason)
  | _ -> Alcotest.fail "malformed return source was not rejected once");
  let zero_rule = get_ok (Core.Operator.Rule.of_stable_name "return-zero@1") in
  let source, wrong_range, wrong_legacy =
    make_legacy ~source_text ~expression:root ~rule:zero_rule ~replacement:"0"
      ()
  in
  ignore
    (expect_invariant "range mismatch"
       (Engine.Ocaml_frontend.For_testing.verify_return_shadow ~source
          ~path:(Core.Mutant.path wrong_legacy)
          ~range:wrong_range ~expression:body ~legacy:wrong_legacy));
  let source, range, wrong_render =
    make_legacy ~source_text ~expression:body ~rule:zero_rule ~replacement:"(0)"
      ()
  in
  ignore
    (expect_invariant "legacy rendering mismatch"
       (Engine.Ocaml_frontend.For_testing.verify_return_shadow ~source
          ~path:(Core.Mutant.path wrong_render)
          ~range ~expression:body ~legacy:wrong_render))

let test_event_set_is_exact_ordered_and_rejection_free () =
  Compmisc.init_path ();
  let source_text = "fun (value : bool) -> value" in
  let body = typed_expression source_text |> direct_function_body in
  let source = Core.Source.of_string source_text in
  let range = range_of_expression body in
  let make stable_name replacement =
    let rule = get_ok (Core.Operator.Rule.of_stable_name stable_name) in
    let _, _, legacy =
      make_legacy ~source_text ~expression:body ~rule ~replacement ()
    in
    legacy
  in
  let return_false = make "return-false@1" "false" in
  let return_true = make "return-true@1" "true" in
  let return_zero = make "return-zero@1" "0" in
  let path = Core.Mutant.path return_false in
  let verify source legacy =
    Engine.Ocaml_frontend.For_testing.verify_return_shadow_event ~source ~path
      ~range ~expression:body ~legacy
  in
  (match verify source [ return_false; return_true ] with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "complete ordered event failed: %a" Engine.Error.pp error);
  let check_decision label expected result =
    let error = expect_invariant label result in
    Alcotest.(check (option string))
      (label ^ " decision") (Some expected)
      (List.assoc_opt "decision" (Engine.Error.context error))
  in
  check_decision "missing legacy candidate" "spec-only"
    (verify source [ return_false ]);
  check_decision "extra legacy candidate" "legacy-only"
    (verify source [ return_false; return_true; return_zero ]);
  check_decision "legacy candidate order" "order-mismatch"
    (verify source [ return_true; return_false ]);
  check_decision "duplicate legacy candidate" "duplicate-legacy"
    (verify source [ return_false; return_false; return_true ]);
  let malformed = Bytes.make (String.length source_text) ' ' in
  Bytes.set malformed (Core.Source_range.start_byte range) '(';
  check_decision "Spec rejection is fatal"
    "rejection:return-false@1:source-bytes-mismatch"
    (verify (Core.Source.of_string (Bytes.unsafe_to_string malformed)) [])

let test_spec_writer_provenance_and_atomicity () =
  Compmisc.init_path ();
  let source_text = "fun (value : bool) -> value" in
  let body = typed_expression source_text |> direct_function_body in
  let source = Core.Source.of_string source_text in
  let range = range_of_expression body in
  let make stable_name replacement =
    let rule = get_ok (Core.Operator.Rule.of_stable_name stable_name) in
    let _, _, legacy =
      make_legacy ~source_text ~expression:body ~rule ~replacement ()
    in
    legacy
  in
  let legacy = [ make "return-false@1" "false"; make "return-true@1" "true" ] in
  let path = Core.Mutant.path (List.hd legacy) in
  let committed = ref None in
  (match
     Engine.Ocaml_frontend.For_testing.commit_return_shadow_event ~source ~path
       ~range ~expression:body ~legacy ~commit:(fun shadow ->
         committed := Some shadow)
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "Spec writer rejected exact legacy oracle: %a"
        Engine.Error.pp error);
  let shadow =
    match !committed with
    | Some shadow -> shadow
    | None -> Alcotest.fail "Spec writer did not commit"
  in
  Alcotest.(check (list string))
    "committed IDs retain legacy catalog order"
    (List.map
       (fun mutant -> Core.Mutant.Id.full (Core.Mutant.id mutant))
       legacy)
    (List.map
       (fun mutant -> Core.Mutant.Id.full (Core.Mutant.id mutant))
       shadow);
  Alcotest.(check bool)
    "writer commits newly materialized Spec objects, not legacy oracle objects"
    true
    (List.for_all2 (fun legacy shadow -> legacy != shadow) legacy shadow);
  let commit_calls = ref 0 in
  ignore
    (expect_invariant "parity failure does not commit"
       (Engine.Ocaml_frontend.For_testing.commit_return_shadow_event ~source
          ~path ~range ~expression:body
          ~legacy:[ List.hd legacy ]
          ~commit:(fun _ -> incr commit_calls)));
  Alcotest.(check int)
    "failed event has no partial writer callback" 0 !commit_calls

let test_refutation_rhs_is_not_a_return_candidate () =
  let source =
    String.concat "\n"
      [
        "type empty = |";
        "";
        "let eliminate : empty -> int = function _ -> .";
        "let normal = function value -> value + 1";
        "";
      ]
  in
  with_serialized_cmt ~source (fun ~root ~cmt_path ->
      let unreachable = unreachable_expression cmt_path in
      Alcotest.(check int)
        "Spec treats typed refutation as not applicable" 0
        (List.length
           (Core.Operator.Spec.evaluate_return_replacement ~source_bytes:"."
              unreachable));
      let discovery =
        match
          Engine.Ocaml_frontend.discover ~root ~cmt_files:[ cmt_path ]
            ~selected_source:(fun _ -> true)
            ~operators:[ Core.Operator.Return_replacement ]
        with
        | Ok discovery -> discovery
        | Error error ->
            Alcotest.failf "cannot discover serialized refutation fixture: %a"
              Engine.Error.pp error
      in
      match Core.Catalog.to_list discovery.catalog with
      | [ mutant ] ->
          Alcotest.(check int)
            "only the normal case remains" 4
            (Core.Mutant.range mutant |> Core.Source_range.start_line);
          Alcotest.(check string)
            "normal case keeps its rule ID" "return-zero@1"
            (Core.Mutant.rule mutant |> Core.Operator.Rule.stable_name);
          Alcotest.(check string)
            "normal case owns its exact source" "value + 1"
            (Core.Mutant.original mutant);
          Alcotest.(check string)
            "normal case keeps its replacement" "0"
            (Core.Mutant.replacement mutant)
      | mutants ->
          Alcotest.failf "expected one normal return candidate, found %d"
            (List.length mutants))

let () =
  Alcotest.run "Return shadow parity contract"
    [
      ( "return-replacement",
        [
          Alcotest.test_case "registry and all eight rules" `Quick
            test_registry_and_all_eight_rules;
          Alcotest.test_case "all 30 rules have shadow definitions" `Quick
            test_all_thirty_registered_rules_have_a_shadow_definition;
          Alcotest.test_case "registry compatibility projections" `Quick
            test_registry_owned_compatibility_projections;
          Alcotest.test_case "coverage rejects duplicate and missing" `Quick
            test_definition_coverage_rejects_duplicate_and_missing;
          Alcotest.test_case "aliases, raw forms, abstract boundary" `Quick
            test_alias_comments_attributes_and_abstract_boundary;
          Alcotest.test_case "source, range, and rendering mismatch" `Quick
            test_source_range_and_render_mismatch;
          Alcotest.test_case "event set exactness" `Quick
            test_event_set_is_exact_ordered_and_rejection_free;
          Alcotest.test_case "Spec writer provenance and atomicity" `Quick
            test_spec_writer_provenance_and_atomicity;
          Alcotest.test_case "refutation RHS is not a return candidate" `Quick
            test_refutation_rhs_is_not_a_return_candidate;
        ] );
    ]
