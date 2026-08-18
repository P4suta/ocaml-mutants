module Engine = Ocaml_mutants_engine

(* The include/exclude glob surface documented in docs/configuration.md: `/` is
   the portable separator, `*` stays inside one path component, `?` matches one
   non-separator byte, and `**` crosses directory boundaries. *)

let check_match ~pattern path expected =
  Alcotest.(check bool)
    (Printf.sprintf "%S vs %S" pattern path)
    expected
    (Engine.Glob.matches ~pattern path)

let test_star_stays_in_component () =
  check_match ~pattern:"lib/*.ml" "lib/a.ml" true;
  check_match ~pattern:"lib/*.ml" "lib/sub/a.ml" false;
  check_match ~pattern:"*.ml" "a.ml" true;
  check_match ~pattern:"*.ml" "lib/a.ml" false;
  check_match ~pattern:"lib/*" "lib/a.ml" true;
  check_match ~pattern:"lib/*" "lib" false

let test_question_mark_is_one_byte () =
  check_match ~pattern:"a?c.ml" "abc.ml" true;
  check_match ~pattern:"a?c.ml" "ac.ml" false;
  check_match ~pattern:"a?c.ml" "abbc.ml" false;
  check_match ~pattern:"a?c" "a/c" false

let test_double_star_crosses_directories () =
  check_match ~pattern:"**/*.ml" "a.ml" true;
  check_match ~pattern:"**/*.ml" "lib/deep/nested/a.ml" true;
  check_match ~pattern:"lib/**/*.ml" "lib/a.ml" true;
  check_match ~pattern:"lib/**/*.ml" "lib/deep/a.ml" true;
  check_match ~pattern:"lib/**/*.ml" "bin/a.ml" false;
  check_match ~pattern:"lib/**" "lib/deep/a.ml" true

let test_separators_normalize () =
  check_match ~pattern:"lib/**/*.ml" "lib\\deep\\a.ml" true;
  check_match ~pattern:"lib/*.ml" "./lib/a.ml" true;
  if Sys.win32 then check_match ~pattern:"lib/*.ml" "LIB/A.ML" true

(* The built-in defaults: every .ml file, minus test and build trees. A
   component must literally be `test`; a name that merely contains the substring
   stays selected. *)
let test_default_selection () =
  let selected =
    Engine.Glob.selected ~include_:[ "**/*.ml" ]
      ~exclude:[ "**/test/**"; "**/tests/**"; "**/_build/**" ]
  in
  Alcotest.(check bool) "library source" true (selected "lib/engine/glob.ml");
  Alcotest.(check bool) "root source" true (selected "main.ml");
  Alcotest.(check bool) "test tree" false (selected "test/unit_tests.ml");
  Alcotest.(check bool)
    "nested tests tree" false
    (selected "src/tests/helper.ml");
  Alcotest.(check bool) "build tree" false (selected "_build/default/lib/a.ml");
  Alcotest.(check bool)
    "substring is not a component" true
    (selected "lib/attestation.ml");
  Alcotest.(check bool) "non-ml file" false (selected "lib/engine/glob.mli");
  Alcotest.(check bool)
    "empty include selects nothing" false
    (Engine.Glob.selected ~include_:[] ~exclude:[] "lib/a.ml")

let () =
  Alcotest.run "glob contracts"
    [
      ( "glob",
        [
          Alcotest.test_case "star stays in one component" `Quick
            test_star_stays_in_component;
          Alcotest.test_case "question mark is one byte" `Quick
            test_question_mark_is_one_byte;
          Alcotest.test_case "double star crosses directories" `Quick
            test_double_star_crosses_directories;
          Alcotest.test_case "separators normalize" `Quick
            test_separators_normalize;
          Alcotest.test_case "default selection" `Quick test_default_selection;
        ] );
    ]
