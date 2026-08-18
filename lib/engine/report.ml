module Core = Ocaml_mutants_core

type tone = Success | Failure | Warning | Diagnostic | Neutral

let ansi_code = function
  | Success -> "32"
  | Failure -> "31"
  | Warning -> "33"
  | Diagnostic -> "35"
  | Neutral -> "36"

let colorize enabled tone value =
  if enabled then Printf.sprintf "\027[%sm%s\027[0m" (ansi_code tone) value
  else value

let first_line value =
  match String.index_opt value '\n' with
  | None -> value
  | Some index -> String.sub value 0 index

let focused_diff formatter result =
  let mutant = result.Run_store.mutant in
  Format.fprintf formatter "    - %s@.    + %s@."
    (first_line (Core.Mutant.original mutant))
    (first_line (Core.Mutant.replacement mutant))

let outcome_rank = function
  | Core.Outcome.Survived -> 0
  | Core.Outcome.Error _ -> 1
  | Core.Outcome.Inconclusive _ -> 2
  | Core.Outcome.Timeout -> 3
  | Core.Outcome.Killed -> 4

let result_display result =
  match (result.Run_store.expected_reason, result.outcome) with
  | Some _, Core.Outcome.Survived -> ("EXPECTED SURVIVOR", Success)
  | Some _, Core.Outcome.Killed -> ("UNFULFILLED EXPECTATION", Failure)
  | Some _, Core.Outcome.Timeout when result.timeout_confirmed ->
      ("UNFULFILLED EXPECTATION", Failure)
  | _, Core.Outcome.Survived -> ("SURVIVED", Failure)
  | _, Core.Outcome.Killed -> ("KILLED", Success)
  | _, Core.Outcome.Timeout -> ("TIMEOUT", Warning)
  | _, Core.Outcome.Inconclusive _ -> ("INCONCLUSIVE", Diagnostic)
  | _, Core.Outcome.Error _ -> ("ERROR", Diagnostic)

let print_result ~color formatter result =
  let mutant = result.Run_store.mutant in
  let label, tone = result_display result in
  Format.fprintf formatter "%s %s:%a  %s  %.3fs%s%s@."
    (colorize color tone label)
    (Core.Mutant.path mutant) Core.Source_range.pp (Core.Mutant.range mutant)
    (Core.Operator.Rule.stable_name (Core.Mutant.rule mutant))
    (Core.Duration.to_seconds result.duration)
    (if result.cached then "  cached" else "")
    (match result.expected_reason with
    | None -> ""
    | Some reason -> "  expected: " ^ reason);
  if result.outcome = Core.Outcome.Survived then focused_diff formatter result

let expectation_tone = function
  | Run_store.Expectation_fulfilled -> Success
  | Run_store.Expectation_not_evaluated -> Neutral
  | Run_store.Expectation_unfulfilled_killed
  | Run_store.Expectation_unfulfilled_confirmed_timeout
  | Run_store.Expectation_inconclusive _ | Run_store.Expectation_error _
  | Run_store.Expectation_stale ->
      Failure

let print_expectations ~color formatter expectations =
  if expectations <> [] then (
    Format.fprintf formatter "Expectations:@.";
    List.iter
      (fun (expectation : Run_store.expectation_evaluation) ->
        let status =
          Run_store.expectation_status_name expectation.status
          |> String.uppercase_ascii
        in
        Format.fprintf formatter "  %s %s — %s@."
          (colorize color (expectation_tone expectation.status) status)
          expectation.mutant_id expectation.reason)
      expectations)

let print_warnings ~color formatter warnings =
  if warnings <> [] then (
    Format.fprintf formatter "Warnings:@.";
    List.iter
      (fun (warning : Run_store.warning) ->
        Format.fprintf formatter "  %s %s@."
          (colorize color Warning (String.uppercase_ascii warning.code))
          warning.message)
      warnings)

let print_skipped formatter skipped =
  if skipped <> [] then (
    Format.fprintf formatter "Skipped:@.";
    List.iter
      (fun (skip : Run_store.skip_summary) ->
        Format.fprintf formatter "  %d %s@." skip.count skip.reason;
        List.iter
          (fun example -> Format.fprintf formatter "    - %s@." example)
          skip.examples)
      skipped)

let print_run ?(color = false) formatter run =
  let results =
    List.sort
      (fun left right ->
        match
          Int.compare
            (outcome_rank left.Run_store.outcome)
            (outcome_rank right.outcome)
        with
        | 0 -> Core.Mutant.compare left.mutant right.mutant
        | value -> value)
      run.Run_store.results
  in
  List.iter (print_result ~color formatter) results;
  let ( killed,
        unexpected_survivors,
        expected_survivors,
        timeout,
        inconclusive,
        error ) =
    List.fold_left
      (fun ( killed,
             unexpected_survivors,
             expected_survivors,
             timeout,
             inconclusive,
             error ) result ->
        match (result.Run_store.outcome, result.expected_reason) with
        | Core.Outcome.Killed, _ ->
            ( killed + 1,
              unexpected_survivors,
              expected_survivors,
              timeout,
              inconclusive,
              error )
        | Core.Outcome.Survived, Some _ ->
            ( killed,
              unexpected_survivors,
              expected_survivors + 1,
              timeout,
              inconclusive,
              error )
        | Core.Outcome.Survived, None ->
            ( killed,
              unexpected_survivors + 1,
              expected_survivors,
              timeout,
              inconclusive,
              error )
        | Core.Outcome.Timeout, _ ->
            ( killed,
              unexpected_survivors,
              expected_survivors,
              timeout + 1,
              inconclusive,
              error )
        | Core.Outcome.Inconclusive _, _ ->
            ( killed,
              unexpected_survivors,
              expected_survivors,
              timeout,
              inconclusive + 1,
              error )
        | Core.Outcome.Error _, _ ->
            ( killed,
              unexpected_survivors,
              expected_survivors,
              timeout,
              inconclusive,
              error + 1 ))
      (0, 0, 0, 0, 0, 0) results
  in
  let not_run = Run_store.not_run run in
  let total = List.length results + List.length not_run in
  Format.fprintf formatter
    "@.Run %s (%s): %d mutants — %s, %s, %s, %s, %s, %s, %d not run@."
    (Core.Run_id.to_string run.metadata.id)
    (Run_store.status_name run.status)
    total
    (colorize color Success (Printf.sprintf "%d killed" killed))
    (colorize color Failure
       (Printf.sprintf "%d unexpected survivor" unexpected_survivors))
    (colorize color Success
       (Printf.sprintf "%d expected survivor" expected_survivors))
    (colorize color Warning (Printf.sprintf "%d timeout" timeout))
    (colorize color Diagnostic (Printf.sprintf "%d inconclusive" inconclusive))
    (colorize color Diagnostic (Printf.sprintf "%d error" error))
    (List.length not_run);
  print_expectations ~color formatter run.expectations;
  print_warnings ~color formatter run.warnings;
  print_skipped formatter run.skipped

let print_catalog formatter catalog =
  let mutants = Core.Catalog.to_list catalog in
  List.iter
    (fun mutant ->
      Format.fprintf formatter "%s  %s:%a  %s@,"
        (Core.Mutant.Id.short (Core.Mutant.id mutant))
        (Core.Mutant.path mutant) Core.Source_range.pp
        (Core.Mutant.range mutant)
        (Core.Operator.Rule.stable_name (Core.Mutant.rule mutant)))
    mutants;
  Format.fprintf formatter "%d mutants@." (List.length mutants)
