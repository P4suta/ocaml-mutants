module Engine = Ocaml_mutants_engine
module C = Engine.Cache_fs

let name value =
  match C.Name.of_string value with
  | Ok name -> name
  | Error error ->
      Alcotest.failf "invalid test name %S: %a" value C.Name.pp_error error

let relative values =
  match C.Relative.of_strings values with
  | Ok relative -> relative
  | Error error ->
      Alcotest.failf "invalid test component %S: %a" error.component
        C.Name.pp_error error.reason

let test_portable_names () =
  let accepted =
    [
      ".ocaml-mutants-cache-v2";
      "latest";
      "20260817T010203Z-token.json";
      String.make 64 'a';
    ]
  in
  List.iter
    (fun value ->
      Alcotest.(check string) "round trip" value (C.Name.to_string (name value)))
    accepted;
  let rejected =
    [
      ("", C.Name.Empty);
      (".", C.Name.Dot_component);
      ("..", C.Name.Dot_component);
      ("report.", C.Name.Trailing_dot);
      ("report ", C.Name.Trailing_space);
      ("CON", C.Name.Reserved_device);
      ("con.json", C.Name.Reserved_device);
      ("PrN.txt", C.Name.Reserved_device);
      ("AUX", C.Name.Reserved_device);
      ("nul.cache", C.Name.Reserved_device);
      ("COM1", C.Name.Reserved_device);
      ("com9.json", C.Name.Reserved_device);
      ("LPT1", C.Name.Reserved_device);
      ("lpt9.cache", C.Name.Reserved_device);
      ("dir/file", C.Name.Unsafe_character '/');
      ("dir\\file", C.Name.Unsafe_character '\\');
      ("stream:name", C.Name.Unsafe_character ':');
    ]
  in
  List.iter
    (fun (value, expected) ->
      match C.Name.of_string value with
      | Ok _ -> Alcotest.failf "unsafe component %S was accepted" value
      | Error actual ->
          Alcotest.(check bool)
            ("rejection for " ^ value) true (actual = expected))
    rejected

let test_relative_is_component_only () =
  let value = relative [ "runs"; "run-1"; "report.json" ] in
  Alcotest.(check int)
    "component count" 3
    (List.length (C.Relative.components value));
  Alcotest.(check bool)
    "structural equality" true
    (C.Relative.equal value
       ( C.Relative.child
           (C.Relative.child C.Relative.root (name "runs"))
           (name "run-1")
       |> fun parent -> C.Relative.child parent (name "report.json") ));
  match C.Relative.of_strings [ "runs"; ".."; "report.json" ] with
  | Error { component = ".."; reason = C.Name.Dot_component } -> ()
  | Error _ | Ok _ -> Alcotest.fail "dot-dot entered a relative capability path"

let expect_budget_error expected = function
  | Error actual -> Alcotest.(check bool) "budget error" true (actual = expected)
  | Ok _ -> Alcotest.fail "invalid traversal budget was accepted"

let test_explicit_traversal_budget () =
  expect_budget_error (C.Traversal_budget.Negative_max_depth (-1L))
    (C.Traversal_budget.create ~max_depth:(-1L) ~max_entries:0L
       ~max_native_name_bytes:0L);
  expect_budget_error (C.Traversal_budget.Negative_max_entries (-1L))
    (C.Traversal_budget.create ~max_depth:0L ~max_entries:(-1L)
       ~max_native_name_bytes:0L);
  expect_budget_error (C.Traversal_budget.Negative_max_native_name_bytes (-1L))
    (C.Traversal_budget.create ~max_depth:0L ~max_entries:0L
       ~max_native_name_bytes:(-1L));
  let budget =
    match
      C.Traversal_budget.create ~max_depth:7L ~max_entries:101L
        ~max_native_name_bytes:4096L
    with
    | Ok budget -> budget
    | Error _ -> Alcotest.fail "valid traversal budget was rejected"
  in
  Alcotest.(check int64) "depth" 7L (C.Traversal_budget.max_depth budget);
  Alcotest.(check int64) "entries" 101L (C.Traversal_budget.max_entries budget);
  Alcotest.(check int64)
    "native bytes" 4096L
    (C.Traversal_budget.max_native_name_bytes budget)

let test_diagnostic_layers_are_separate () =
  let contract =
    C.make_error ~operation:C.Remove_tree ~class_:C.Identity_changed
      ~native_domain:C.Contract ~native_code:"binding-changed" ()
  in
  Alcotest.(check bool)
    "contract error has no primitive" true
    (contract.primitive_operation = None);
  let backend =
    C.make_error ~operation:C.Remove_tree
      ~primitive_operation:Engine.Dir_cap.Conditional_unlink
      ~class_:C.Identity_changed ~native_domain:C.In_memory
      ~native_code:"identity-changed" ()
  in
  Alcotest.(check bool)
    "backend primitive retained" true
    (backend.primitive_operation = Some Engine.Dir_cap.Conditional_unlink);
  Alcotest.(check string)
    "cache operation" "remove-tree"
    (C.operation_name backend.operation);
  Alcotest.(check string)
    "backend operation" "conditional-unlink"
    (Engine.Dir_cap.operation_name (Option.get backend.primitive_operation))

let test_cleanup_problem_keeps_retry_state () =
  let error =
    C.make_error ~operation:C.Close_listing
      ~primitive_operation:Engine.Dir_cap.Close_directory ~class_:C.Busy
      ~native_domain:C.In_memory ~native_code:"close-busy" ()
  in
  let problem =
    ({
       C.error;
       local_handle = C.Handle_still_open "exact-capability";
       namespace_released = false;
     }
      : string C.cleanup_problem)
  in
  let issue = C.Cleanup_error { primary = problem; suppressed = [] } in
  Alcotest.(check string)
    "issue error" "close-busy" (C.issue_error issue).native_code;
  match issue with
  | C.Cleanup_error
      {
        primary = { local_handle = C.Handle_still_open retry; _ };
        suppressed = [];
      } ->
      Alcotest.(check string) "retry token" "exact-capability" retry
  | C.Operation_error _ | C.Cleanup_error _ ->
      Alcotest.fail "cleanup progress was flattened"

let () =
  Alcotest.run "cache-fs-contracts"
    [
      ( "portable-contract",
        [
          Alcotest.test_case "portable names" `Quick test_portable_names;
          Alcotest.test_case "relative components" `Quick
            test_relative_is_component_only;
          Alcotest.test_case "explicit traversal budget" `Quick
            test_explicit_traversal_budget;
          Alcotest.test_case "diagnostic layers" `Quick
            test_diagnostic_layers_are_separate;
          Alcotest.test_case "cleanup retry state" `Quick
            test_cleanup_problem_keeps_retry_state;
        ] );
    ]
