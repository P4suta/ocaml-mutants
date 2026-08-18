module Engine = Ocaml_mutants_engine
module Core = Ocaml_mutants_core

(* Engine-level discovery contracts: real serialized CMTs flow through the
   public [Ocaml_frontend.discover] and the resulting catalog is checked
   directly. These semantic guarantees previously lived inside the shadow parity
   suites that compared the production writer against a second legacy
   implementation; the [Operator.Spec] registry is now the single writer, so the
   guarantees are stated against observable discovery output instead. *)

let remove_if_present path = try Sys.remove path with Sys_error _ -> ()

let with_serialized_cmt ~source use =
  let directory =
    Filename.temp_dir ~perms:0o700 "ocaml-mutants-frontend-discovery-" ".tmp"
  in
  let source_path = Filename.concat directory "discovery_fixture.ml" in
  let cmi_path = Filename.concat directory "discovery_fixture.cmi" in
  let cmo_path = Filename.concat directory "discovery_fixture.cmo" in
  let cmt_path = Filename.concat directory "discovery_fixture.cmt" in
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

let discover ~operators ~root ~cmt_path =
  match
    Engine.Ocaml_frontend.discover ~root ~cmt_files:[ cmt_path ]
      ~selected_source:(fun _ -> true)
      ~operators
  with
  | Ok discovery -> discovery
  | Error error ->
      Alcotest.failf "cannot discover serialized fixture: %a" Engine.Error.pp
        error

let rule_names mutants =
  List.map
    (fun mutant -> Core.Mutant.rule mutant |> Core.Operator.Rule.stable_name)
    mutants

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
        discover ~operators:[ Core.Operator.Boolean_literal ] ~root ~cmt_path
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
        (rule_names mutants);
      Alcotest.(check (list string))
        "remaining replacements preserve the normal Boolean rule"
        [ "true"; "true" ]
        (List.map Core.Mutant.replacement mutants))

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
      let discovery =
        discover ~operators:[ Core.Operator.Return_replacement ] ~root ~cmt_path
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

let test_shadowed_comparison_is_not_mutated () =
  let source =
    String.concat "\n"
      [
        "let genuine left right = left = right";
        "";
        "module Local = struct";
        "  let ( = ) (_ : int) (_ : int) = false";
        "";
        "  let shadowed left right = left = right";
        "end";
        "";
      ]
  in
  with_serialized_cmt ~source (fun ~root ~cmt_path ->
      let discovery =
        discover ~operators:[ Core.Operator.Comparison ] ~root ~cmt_path
      in
      let mutants = Core.Catalog.to_list discovery.catalog in
      Alcotest.(check (list string))
        "only the resolved Stdlib comparison is mutated" [ "eq-to-neq@1" ]
        (rule_names mutants);
      Alcotest.(check (list int))
        "the shadowed application is excluded by typed evidence" [ 1 ]
        (List.map
           (fun mutant ->
             Core.Mutant.range mutant |> Core.Source_range.start_line)
           mutants))

let test_match_and_try_guards_are_negated () =
  let source =
    String.concat "\n"
      [
        "let classify value =";
        "  match value with";
        "  | candidate when candidate > 0 -> \"positive\"";
        "  | _ -> \"other\"";
        "";
        "let recover thunk =";
        "  try thunk ()";
        "  with Failure message when String.length message > 0 -> message";
        "";
      ]
  in
  with_serialized_cmt ~source (fun ~root ~cmt_path ->
      let discovery =
        discover ~operators:[ Core.Operator.Condition_negation ] ~root ~cmt_path
      in
      let mutants = Core.Catalog.to_list discovery.catalog in
      Alcotest.(check (list string))
        "match and try guards are both negated"
        [ "negate-condition@1"; "negate-condition@1" ]
        (rule_names mutants);
      Alcotest.(check (list string))
        "negation renders a self-contained Stdlib.not application"
        [
          "Stdlib.not (candidate > 0)"; "Stdlib.not (String.length message > 0)";
        ]
        (List.map Core.Mutant.replacement mutants))

let test_match_arms_take_neutral_bodies () =
  let source =
    String.concat "\n"
      [
        "let label value =";
        "  match value with 0 -> \"zero\" | _ -> \"other\"";
        "";
        "let tally value = match value with None -> 0 | Some count -> count";
        "";
      ]
  in
  with_serialized_cmt ~source (fun ~root ~cmt_path ->
      let discovery =
        discover ~operators:[ Core.Operator.Match_arm ] ~root ~cmt_path
      in
      let mutants = Core.Catalog.to_list discovery.catalog in
      Alcotest.(check (list string))
        "string arms are replaced and the identity zero arm is excluded"
        [
          "match-arm-empty-string@1";
          "match-arm-empty-string@1";
          "match-arm-zero@1";
        ]
        (rule_names mutants);
      Alcotest.(check (list string))
        "replacements are the neutral literals of the arm result type"
        [ "\"\""; "\"\""; "0" ]
        (List.map Core.Mutant.replacement mutants))

let test_user_defined_some_is_not_swapped () =
  let source =
    String.concat "\n"
      [
        "let genuine value = [ Some value ]";
        "";
        "module Local = struct";
        "  type 'value shadow = Some of 'value | Empty";
        "";
        "  let masked value = [ Some value; Empty ]";
        "end";
        "";
      ]
  in
  with_serialized_cmt ~source (fun ~root ~cmt_path ->
      let discovery =
        discover
          ~operators:[ Core.Operator.Constructor_replacement ]
          ~root ~cmt_path
      in
      let mutants = Core.Catalog.to_list discovery.catalog in
      let some_swaps =
        List.filter
          (fun mutant ->
            Core.Mutant.rule mutant |> Core.Operator.Rule.stable_name
            = "some-to-none@1")
          mutants
      in
      Alcotest.(check (list int))
        "only the Stdlib option constructor is swapped" [ 1 ]
        (List.map
           (fun mutant ->
             Core.Mutant.range mutant |> Core.Source_range.start_line)
           some_swaps))

let () =
  Alcotest.run "Frontend discovery contract"
    [
      ( "discovery",
        [
          Alcotest.test_case "direct assert false is filtered from CMT" `Quick
            test_direct_assert_false_is_filtered_from_serialized_cmt;
          Alcotest.test_case "refutation RHS is not a return candidate" `Quick
            test_refutation_rhs_is_not_a_return_candidate;
          Alcotest.test_case "shadowed comparison is not mutated" `Quick
            test_shadowed_comparison_is_not_mutated;
          Alcotest.test_case "match and try guards are negated" `Quick
            test_match_and_try_guards_are_negated;
          Alcotest.test_case "match arms take neutral bodies" `Quick
            test_match_arms_take_neutral_bodies;
          Alcotest.test_case "user-defined Some is not swapped" `Quick
            test_user_defined_some_is_not_swapped;
        ] );
    ]
