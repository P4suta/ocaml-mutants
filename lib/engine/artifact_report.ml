module Core = Ocaml_mutants_core

type format = Terminal | Native_json | Html | Markdown | Sarif | Stryker

let of_string = function
  | "terminal" -> Ok Terminal
  | "json" | "native-json" -> Ok Native_json
  | "html" -> Ok Html
  | "markdown" | "md" -> Ok Markdown
  | "sarif" -> Ok Sarif
  | "stryker" | "stryker-json" -> Ok Stryker
  | value ->
      Error
        (Printf.sprintf
           "unknown report format %S (expected terminal, json, html, markdown, \
            sarif, or stryker)"
           value)

let name = function
  | Terminal -> "terminal"
  | Native_json -> "json"
  | Html -> "html"
  | Markdown -> "markdown"
  | Sarif -> "sarif"
  | Stryker -> "stryker"

let extension = function
  | Terminal -> "txt"
  | Native_json | Sarif | Stryker -> "json"
  | Html -> "html"
  | Markdown -> "md"

let sanitize_controls value =
  String.map
    (fun character ->
      match character with
      | '\n' | '\r' | '\t' -> character
      | '\000' .. '\008' | '\011' | '\012' | '\014' .. '\031' | '\127' -> ' '
      | character -> character)
    value

let html_escape value =
  let buffer = Buffer.create (String.length value + 16) in
  String.iter
    (function
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | '"' -> Buffer.add_string buffer "&quot;"
      | '\'' -> Buffer.add_string buffer "&#39;"
      | character -> Buffer.add_char buffer character)
    (sanitize_controls value);
  Buffer.contents buffer

let script_safe_json json =
  let value = Yojson.Safe.to_string ~std:true json in
  let buffer = Buffer.create (String.length value + 32) in
  String.iter
    (function
      | '<' -> Buffer.add_string buffer "\\u003c"
      | '>' -> Buffer.add_string buffer "\\u003e"
      | '&' -> Buffer.add_string buffer "\\u0026"
      | character -> Buffer.add_char buffer character)
    value;
  Buffer.contents buffer

type source_embedding = Embed_context | Embed_all | Embed_none

let source_embedding (run : Run_store.run) =
  try
    let open Yojson.Safe.Util in
    match
      run.metadata.resolved_config |> member "privacy"
      |> member "source_embedding" |> to_string
    with
    | "all" -> Embed_all
    | "none" -> Embed_none
    | "context" | _ -> Embed_context
  with Yojson.Safe.Util.Type_error _ -> Embed_context

let result_json ~embed_context ~coverage (result : Run_store.mutant_result) =
  let mutant = result.mutant in
  `Assoc
    [
      ("id", `String (Core.Mutant.Id.full (Core.Mutant.id mutant)));
      ("short_id", `String (Core.Mutant.Id.short (Core.Mutant.id mutant)));
      ("lineage_id", `String (Core.Mutant.lineage_id mutant));
      ("path", `String (sanitize_controls (Core.Mutant.path mutant)));
      ("line", `Int (Core.Source_range.start_line (Core.Mutant.range mutant)));
      ( "rule",
        `String (Core.Operator.Rule.stable_name (Core.Mutant.rule mutant)) );
      ("outcome", `String (Core.Outcome.name result.outcome));
      ("coverage", `String coverage);
      ( "detail",
        `String
          (match result.outcome with
          | Core.Outcome.Error value | Core.Outcome.Inconclusive value ->
              sanitize_controls value
          | Core.Outcome.Killed | Core.Outcome.Survived | Core.Outcome.Timeout
            ->
              "") );
      ( "original",
        `String
          (if embed_context then sanitize_controls (Core.Mutant.original mutant)
           else "") );
      ( "replacement",
        `String
          (if embed_context then
             sanitize_controls (Core.Mutant.replacement mutant)
           else "") );
      ( "evidence",
        `String
          (Run_store.result_evidence_level result
          |> Run_store.evidence_level_name) );
      ( "expected_reason",
        match result.expected_reason with
        | None -> `Null
        | Some reason -> `String (sanitize_controls reason) );
      ("stdout", `String (sanitize_controls result.stdout.contents));
      ("stderr", `String (sanitize_controls result.stderr.contents));
    ]

let full_sources ~root (run : Run_store.run) =
  let expected = Hashtbl.create 32 in
  let rec collect = function
    | [] -> Ok ()
    | (result : Run_store.mutant_result) :: rest -> (
        let mutant = result.mutant in
        let path = Core.Mutant.path mutant in
        let digest = Core.Mutant.source_digest mutant in
        match Hashtbl.find_opt expected path with
        | Some prior when not (String.equal prior digest) ->
            Error
              (Error.create ~phase:Error.Reporting
                 ~cause:Error.Invariant_violation
                 ~context:[ ("path", path) ]
                 "cannot embed source with contradictory recorded digests")
        | Some _ -> collect rest
        | None ->
            Hashtbl.add expected path digest;
            collect rest)
  in
  match collect run.results with
  | Error _ as error -> error
  | Ok () ->
      let paths =
        Hashtbl.to_seq_keys expected |> List.of_seq |> List.sort String.compare
      in
      let rec read sources = function
        | [] -> Ok (`Assoc (List.rev sources))
        | path :: rest -> (
            match Util.read_file (Filename.concat root path) with
            | Error message ->
                Error
                  (Error.create ~phase:Error.Reporting ~cause:Error.Io_failure
                     ~context:[ ("path", path) ]
                     "cannot embed source: %s" message)
            | Ok contents ->
                let digest = Core.Source.(of_string contents |> digest) in
                if not (String.equal digest (Hashtbl.find expected path)) then
                  Error
                    (Error.create ~phase:Error.Reporting
                       ~cause:Error.Workspace_violation
                       ~context:[ ("path", path) ]
                       "cannot embed source because its digest changed")
                else
                  read
                    ((path, `String (sanitize_controls contents)) :: sources)
                    rest)
      in
      read [] paths

let html ~root run =
  let summary = Run_store.summary run in
  let embedding = source_embedding run in
  let sources =
    match embedding with
    | Embed_all -> full_sources ~root run
    | Embed_context | Embed_none -> Ok (`Assoc [])
  in
  Result.map
    (fun sources ->
      let data =
        `Assoc
          [
            ("run_id", `String (Core.Run_id.to_string run.Run_store.metadata.id));
            ("status", `String (Run_store.status_name run.status));
            ("total", `Int summary.total);
            ("killed", `Int summary.killed);
            ("unexpected_survivors", `Int summary.unexpected_survivors);
            ("inconclusive", `Int summary.inconclusive);
            ("error", `Int summary.error);
            ( "score",
              match summary.score with
              | None -> `Null
              | Some score -> `Float score );
            ( "results",
              `List
                (List.map
                   (fun result ->
                     result_json ~embed_context:(embedding <> Embed_none)
                       ~coverage:(Run_store.result_coverage run result)
                       result)
                   run.results) );
            ("sources", sources);
          ]
        |> script_safe_json
      in
      Printf.sprintf
        {|<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src data:; style-src 'unsafe-inline'; script-src 'nonce-ocaml-mutants-v1'; base-uri 'none'; form-action 'none'">
<title>ocaml-mutants %s</title>
<style>
:root{color-scheme:light dark;font:15px/1.45 system-ui,sans-serif}body{margin:0;background:#101418;color:#eef2f5}header{position:sticky;top:0;padding:1rem 1.25rem;background:#182028;border-bottom:1px solid #3b4650}main{padding:1rem 1.25rem}.metrics{display:flex;gap:1rem;flex-wrap:wrap}.metric{padding:.5rem .75rem;background:#222c35;border-radius:.4rem}button,input{font:inherit;padding:.45rem .65rem;margin:.25rem;border:1px solid #607080;border-radius:.35rem;background:#202a33;color:inherit}button[aria-pressed=true]{outline:2px solid #5bc0eb}.item{margin:.7rem 0;padding:.8rem;border-left:4px solid #778;background:#182028}.survived{border-color:#ff6b6b}.inconclusive,.error,.timeout{border-color:#ffd166}.killed{border-color:#4ecb71}code,pre{white-space:pre-wrap;overflow-wrap:anywhere;background:#0d1115;padding:.25rem}.diff-del{color:#ff8b8b}.diff-add{color:#7fe09b}.muted{color:#a9b4bd}.hidden{display:none}@media (prefers-color-scheme:light){body{background:#fff;color:#17212a}header,.item{background:#f2f5f7}.metric{background:#e8edf0}code,pre{background:#f4f4f4}}
</style>
</head>
<body>
<header><h1>ocaml-mutants run %s</h1><div id="metrics" class="metrics"></div><div><button data-filter="actionable" aria-pressed="true">Actionable</button><button data-filter="all" aria-pressed="false">All</button><button data-filter="killed" aria-pressed="false">Killed</button><input id="search" aria-label="Filter mutants" placeholder="path, rule, or id"></div></header>
<main><div id="notice" class="muted"></div><div id="results"></div></main>
<script id="report-data" type="application/json">%s</script>
<script nonce="ocaml-mutants-v1">
(()=>{'use strict';const d=JSON.parse(document.getElementById('report-data').textContent);const esc=s=>String(s??'').replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));const metrics=document.getElementById('metrics');metrics.innerHTML=[['status',d.status],['total',d.total],['killed',d.killed],['unexpected survivors',d.unexpected_survivors],['inconclusive',d.inconclusive],['errors',d.error],['score',d.score===null?'n/a':d.score.toFixed(1)+'%%']].map(([k,v])=>`<div class="metric"><strong>${esc(v)}</strong> ${esc(k)}</div>`).join('');let filter='actionable';const search=document.getElementById('search');function actionable(r){return r.outcome!=='killed'}function render(){const q=search.value.toLowerCase();const rows=d.results.filter(r=>(filter==='all'||filter==='killed'&&r.outcome==='killed'||filter==='actionable'&&actionable(r))&&(!q||(r.path+' '+r.rule+' '+r.id).toLowerCase().includes(q)));const shown=rows.slice(0,500);document.getElementById('notice').textContent=rows.length>shown.length?`Showing ${shown.length} of ${rows.length}; refine the filter.`:`${rows.length} result(s)`;document.getElementById('results').innerHTML=shown.map(r=>`<article class="item ${esc(r.outcome)}"><h2>${esc(r.outcome.toUpperCase())} <code>${esc(r.short_id)}</code></h2><div>${esc(r.path)}:${r.line} · ${esc(r.rule)} · evidence ${esc(r.evidence)}</div><pre class="diff-del">- ${esc(r.original)}</pre><pre class="diff-add">+ ${esc(r.replacement)}</pre>${r.expected_reason?`<p>Expected: ${esc(r.expected_reason)}</p>`:''}${r.detail?`<pre>${esc(r.detail)}</pre>`:''}<details><summary>Captured output</summary><h3>stdout</h3><pre>${esc(r.stdout)}</pre><h3>stderr</h3><pre>${esc(r.stderr)}</pre></details></article>`).join('')}document.querySelectorAll('[data-filter]').forEach(b=>b.addEventListener('click',()=>{filter=b.dataset.filter;document.querySelectorAll('[data-filter]').forEach(x=>x.setAttribute('aria-pressed',String(x===b)));render()}));search.addEventListener('input',render);render()})();
</script>
</body>
</html>
|}
        (html_escape (Core.Run_id.to_string run.metadata.id))
        (html_escape (Core.Run_id.to_string run.metadata.id))
        data)
    sources

let markdown_escape value =
  sanitize_controls value |> String.split_on_char '\n' |> String.concat " "
  |> String.split_on_char '|' |> String.concat "\\|"

let markdown run =
  let summary = Run_store.summary run in
  let buffer = Buffer.create 4096 in
  Buffer.add_string buffer
    (Printf.sprintf "# ocaml-mutants run `%s`\n\n"
       (Core.Run_id.to_string run.Run_store.metadata.id));
  Buffer.add_string buffer
    (Printf.sprintf
       "Status: **%s** · %d total · %d killed · %d unexpected survivors · \
        evidence **%s**\n\n"
       (Run_store.status_name run.status)
       summary.total summary.killed summary.unexpected_survivors
       (Run_store.run_evidence_level run |> Run_store.evidence_level_name));
  Buffer.add_string buffer
    "| Outcome | Mutant | Location | Rule | Change |\n|---|---|---|---|---|\n";
  run.results
  |> List.filter (fun result -> result.Run_store.outcome <> Core.Outcome.Killed)
  |> List.iter (fun (result : Run_store.mutant_result) ->
      let mutant = result.mutant in
      Buffer.add_string buffer
        (Printf.sprintf "| %s | `%s` | `%s:%d` | `%s` | `%s` → `%s` |\n"
           (Core.Outcome.name result.outcome)
           (Core.Mutant.Id.full (Core.Mutant.id mutant))
           (markdown_escape (Core.Mutant.path mutant))
           (Core.Source_range.start_line (Core.Mutant.range mutant))
           (Core.Operator.Rule.stable_name (Core.Mutant.rule mutant))
           (markdown_escape (Core.Mutant.original mutant))
           (markdown_escape (Core.Mutant.replacement mutant))));
  Buffer.contents buffer

let sarif_level = function
  | Core.Outcome.Survived -> "warning"
  | Core.Outcome.Error _ | Core.Outcome.Inconclusive _ -> "error"
  | Core.Outcome.Timeout -> "warning"
  | Core.Outcome.Killed -> "none"

let sarif run =
  let results =
    run.Run_store.results
    |> List.filter (fun result ->
        result.Run_store.outcome <> Core.Outcome.Killed)
    |> List.map (fun (result : Run_store.mutant_result) ->
        let mutant = result.mutant in
        let range = Core.Mutant.range mutant in
        `Assoc
          [
            ( "ruleId",
              `String (Core.Operator.Rule.stable_name (Core.Mutant.rule mutant))
            );
            ("level", `String (sarif_level result.outcome));
            ( "message",
              `Assoc
                [
                  ( "text",
                    `String
                      (Printf.sprintf "%s mutant: %s -> %s"
                         (Core.Outcome.name result.outcome)
                         (sanitize_controls (Core.Mutant.original mutant))
                         (sanitize_controls (Core.Mutant.replacement mutant)))
                  );
                ] );
            ( "partialFingerprints",
              `Assoc
                [
                  ( "ocamlMutantsFullId",
                    `String (Core.Mutant.Id.full (Core.Mutant.id mutant)) );
                ] );
            ( "locations",
              `List
                [
                  `Assoc
                    [
                      ( "physicalLocation",
                        `Assoc
                          [
                            ( "artifactLocation",
                              `Assoc
                                [ ("uri", `String (Core.Mutant.path mutant)) ]
                            );
                            ( "region",
                              `Assoc
                                [
                                  ( "startLine",
                                    `Int (Core.Source_range.start_line range) );
                                  ( "startColumn",
                                    `Int
                                      (Core.Source_range.start_column range + 1)
                                  );
                                  ( "endLine",
                                    `Int (Core.Source_range.end_line range) );
                                  ( "endColumn",
                                    `Int (Core.Source_range.end_column range + 1)
                                  );
                                ] );
                          ] );
                    ];
                ] );
          ])
  in
  `Assoc
    [
      ("$schema", `String "https://json.schemastore.org/sarif-2.1.0.json");
      ("version", `String "2.1.0");
      ( "runs",
        `List
          [
            `Assoc
              [
                ( "tool",
                  `Assoc
                    [
                      ( "driver",
                        `Assoc
                          [
                            ("name", `String "ocaml-mutants");
                            ("version", `String "1.0.0");
                            ( "informationUri",
                              `String "https://github.com/P4suta/ocaml-mutants"
                            );
                            ("rules", `List []);
                          ] );
                    ] );
                ("results", `List results);
              ];
          ] );
    ]
  |> Yojson.Safe.pretty_to_string ~std:true
  |> fun value -> value ^ "\n"

let projection_error error =
  Error.create ~phase:Error.Reporting ~cause:Error.Invariant_violation
    "cannot render Stryker projection: %a" Stryker_report.pp_error error

let render ~root ~color ?stryker_thresholds format run =
  match format with
  | Terminal -> Ok (Format.asprintf "%a" (Report.print_run ~color) run)
  | Native_json -> Ok (Run_store.run_to_string run)
  | Html -> html ~root run
  | Markdown -> Ok (markdown run)
  | Sarif -> Ok (sarif run)
  | Stryker -> (
      match stryker_thresholds with
      | None ->
          Error
            (Error.create ~phase:Error.Reporting ~cause:Error.Invalid_input
               "Stryker output requires explicit high and low thresholds")
      | Some thresholds ->
          Stryker_report.to_string ~thresholds
            ~read_source:(fun ~path ->
              Util.read_file (Filename.concat root path))
            run
          |> Result.map_error projection_error)
