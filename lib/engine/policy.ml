type finding_kind = Policy_violation | Evidence_refusal

type finding = {
  code : string;
  kind : finding_kind;
  message : string;
  actual : string option;
  required : string option;
}

type verdict = Passed | Violated | Refused

type evaluation = {
  run_id : string;
  verdict : verdict;
  exit_code : int;
  findings : finding list;
  summary : Run_store.summary;
  evidence_level : Run_store.evidence_level;
  complete : bool;
  policy : Config.policy;
  reference_score : float option;
}

let finding ?actual ?required kind code message =
  { code; kind; message; actual; required }

let verdict_name = function
  | Passed -> "passed"
  | Violated -> "policy-violation"
  | Refused -> "refused"

let finding_kind_name = function
  | Policy_violation -> "policy-violation"
  | Evidence_refusal -> "evidence-refusal"

let expectation_findings evaluations =
  List.filter_map
    (fun (evaluation : Run_store.expectation_evaluation) ->
      let actual = Some (Run_store.expectation_status_name evaluation.status) in
      match evaluation.status with
      | Run_store.Expectation_fulfilled -> None
      | Run_store.Expectation_unfulfilled_killed
      | Run_store.Expectation_unfulfilled_confirmed_timeout ->
          Some
            (finding ?actual ~required:"fulfilled" Policy_violation
               "POLICY_EXPECTATION_UNFULFILLED"
               (Printf.sprintf "expected survivor %s is no longer surviving"
                  evaluation.mutant_id))
      | Run_store.Expectation_inconclusive _ | Run_store.Expectation_error _ ->
          Some
            (finding ?actual Evidence_refusal
               "EVIDENCE_EXPECTATION_INCONCLUSIVE"
               (Printf.sprintf
                  "expectation %s does not have conclusive execution evidence"
                  evaluation.mutant_id))
      | Run_store.Expectation_stale ->
          Some
            (finding ?actual Evidence_refusal "EVIDENCE_STALE_EXPECTATION"
               (Printf.sprintf
                  "expectation %s no longer identifies a catalog mutant"
                  evaluation.mutant_id))
      | Run_store.Expectation_not_evaluated ->
          Some
            (finding ?actual Evidence_refusal
               "EVIDENCE_EXPECTATION_NOT_EVALUATED"
               (Printf.sprintf "expectation %s was not evaluated"
                  evaluation.mutant_id)))
    evaluations

let evidence_findings policy (run : Run_store.run) (summary : Run_store.summary)
    evidence_level =
  let findings = ref [] in
  let refuse ?actual ?required code message =
    findings :=
      finding ?actual ?required Evidence_refusal code message :: !findings
  in
  (match run.status with
  | Run_store.Completed -> ()
  | Run_store.Interrupted ->
      refuse "EVIDENCE_RUN_INTERRUPTED" "the run was interrupted"
  | Run_store.Failed error ->
      refuse ~actual:(Error.code error) "EVIDENCE_RUN_FAILED"
        (Error.message error));
  if
    policy.Config.require_complete && not (run.completeness = Run_store.Complete)
  then
    refuse ~actual:summary.kind ~required:"complete" "EVIDENCE_INCOMPLETE"
      "policy requires a complete mutation result";
  if summary.inconclusive > 0 then
    refuse
      ~actual:(string_of_int summary.inconclusive)
      ~required:"0" "EVIDENCE_INCONCLUSIVE"
      "one or more mutant outcomes are inconclusive";
  if summary.error > 0 then
    refuse
      ~actual:(string_of_int summary.error)
      ~required:"0" "EVIDENCE_ERROR"
      "one or more mutant executions ended in an error";
  if summary.unconfirmed_timeouts > 0 then
    refuse
      ~actual:(string_of_int summary.unconfirmed_timeouts)
      ~required:"0" "EVIDENCE_TIMEOUT_UNCONFIRMED"
      "one or more timeout outcomes lack a serial confirmation";
  if evidence_level = Run_store.Estimated && not policy.Config.allow_estimated
  then
    refuse ~actual:"estimated" ~required:"executed-or-exact-cache"
      "EVIDENCE_ESTIMATED_NOT_ALLOWED"
      "policy does not permit estimated evidence";
  List.rev !findings

let policy_findings policy (summary : Run_store.summary) reference_score =
  let findings = ref [] in
  let violate ?actual ?required code message =
    findings :=
      finding ?actual ?required Policy_violation code message :: !findings
  in
  if
    summary.Run_store.unexpected_survivors
    > policy.Config.max_unexpected_survivors
  then
    violate
      ~actual:(string_of_int summary.unexpected_survivors)
      ~required:(Printf.sprintf "<= %d" policy.max_unexpected_survivors)
      "POLICY_UNEXPECTED_SURVIVORS"
      "unexpected survivor count exceeds the configured maximum";
  Option.iter
    (fun minimum ->
      match summary.score with
      | Some score when score >= minimum -> ()
      | Some score ->
          violate
            ~actual:(Printf.sprintf "%.3f" score)
            ~required:(Printf.sprintf ">= %.3f" minimum)
            "POLICY_MINIMUM_SCORE"
            "mutation score is below the configured minimum"
      | None ->
          violate ~actual:"n/a"
            ~required:(Printf.sprintf ">= %.3f" minimum)
            "POLICY_MINIMUM_SCORE"
            "mutation score is unavailable but a minimum is configured")
    policy.minimum_score;
  (match policy.maximum_score_drop with
  | None -> ()
  | Some maximum_drop -> (
      match (reference_score, summary.score) with
      | Some reference, Some score when reference -. score <= maximum_drop -> ()
      | Some reference, Some score ->
          violate
            ~actual:(Printf.sprintf "%.3f" (reference -. score))
            ~required:(Printf.sprintf "<= %.3f" maximum_drop)
            "POLICY_MAXIMUM_SCORE_DROP"
            "mutation score drop exceeds the configured maximum"
      | None, _ | _, None -> ()));
  List.rev !findings

let missing_reference policy reference_score (summary : Run_store.summary) =
  match
    (policy.Config.maximum_score_drop, reference_score, summary.Run_store.score)
  with
  | Some _, None, _ ->
      [
        finding Evidence_refusal "EVIDENCE_REFERENCE_REQUIRED"
          "maximum_score_drop requires a comparison report";
      ]
  | Some _, Some _, None ->
      [
        finding Evidence_refusal "EVIDENCE_SCORE_UNAVAILABLE"
          "maximum_score_drop cannot be evaluated because this run has no score";
      ]
  | _ -> []

let evaluate ~policy ?reference_score (run : Run_store.run) =
  let summary = Run_store.summary run in
  let evidence_level = Run_store.run_evidence_level run in
  let evidence =
    evidence_findings policy run summary evidence_level
    @ missing_reference policy reference_score summary
    @ expectation_findings run.expectations
  in
  let violations = policy_findings policy summary reference_score in
  let findings = evidence @ violations in
  let has_refusal =
    List.exists (fun finding -> finding.kind = Evidence_refusal) findings
  in
  let has_violation =
    List.exists (fun finding -> finding.kind = Policy_violation) findings
  in
  let verdict, exit_code =
    if has_refusal then (Refused, 2)
    else if has_violation then (Violated, 1)
    else (Passed, 0)
  in
  {
    run_id = Ocaml_mutants_core.Run_id.to_string run.metadata.id;
    verdict;
    exit_code;
    findings;
    summary;
    evidence_level;
    complete = run.completeness = Run_store.Complete;
    policy;
    reference_score;
  }

let option_number = function None -> `Null | Some value -> `Float value

let to_yojson evaluation =
  `Assoc
    [
      ("document_type", `String "ocaml-mutants.check-report-v1");
      ("schema_version", `Int 1);
      ("run_id", `String evaluation.run_id);
      ("verdict", `String (verdict_name evaluation.verdict));
      ("exit_code", `Int evaluation.exit_code);
      ( "policy",
        `Assoc
          [
            ("require_complete", `Bool evaluation.policy.require_complete);
            ( "max_unexpected_survivors",
              `Int evaluation.policy.max_unexpected_survivors );
            ("minimum_score", option_number evaluation.policy.minimum_score);
            ( "maximum_score_drop",
              option_number evaluation.policy.maximum_score_drop );
            ("allow_estimated", `Bool evaluation.policy.allow_estimated);
          ] );
      ( "evidence",
        `Assoc
          [
            ( "level",
              `String (Run_store.evidence_level_name evaluation.evidence_level)
            );
            ("complete", `Bool evaluation.complete);
            ("reference_score", option_number evaluation.reference_score);
          ] );
      ( "summary",
        `Assoc
          [
            ("total", `Int evaluation.summary.total);
            ("executed", `Int evaluation.summary.executed);
            ( "unexpected_survivors",
              `Int evaluation.summary.unexpected_survivors );
            ("inconclusive", `Int evaluation.summary.inconclusive);
            ("error", `Int evaluation.summary.error);
            ("score", option_number evaluation.summary.score);
          ] );
      ( "findings",
        `List
          (List.map
             (fun finding ->
               `Assoc
                 [
                   ("code", `String finding.code);
                   ("kind", `String (finding_kind_name finding.kind));
                   ("message", `String finding.message);
                   ( "actual",
                     match finding.actual with
                     | None -> `Null
                     | Some value -> `String value );
                   ( "required",
                     match finding.required with
                     | None -> `Null
                     | Some value -> `String value );
                 ])
             evaluation.findings) );
    ]

let to_string evaluation =
  Yojson.Safe.pretty_to_string ~std:true (to_yojson evaluation) ^ "\n"

let pp formatter evaluation =
  Format.fprintf formatter "Check %s: %s@." evaluation.run_id
    (String.uppercase_ascii (verdict_name evaluation.verdict));
  List.iter
    (fun finding ->
      Format.fprintf formatter "  %s: %s" finding.code finding.message;
      Option.iter
        (fun actual -> Format.fprintf formatter " (actual %s)" actual)
        finding.actual;
      Option.iter
        (fun required -> Format.fprintf formatter " (required %s)" required)
        finding.required;
      Format.fprintf formatter "@.")
    evaluation.findings
