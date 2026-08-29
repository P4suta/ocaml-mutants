type progress = {
  phase : string;
  completed : int;
  total : int;
  workers : int;
  cache_hits : int;
  resume_hits : int;
  elapsed_seconds : float;
  eta_seconds : float option;
}

type event =
  | Run_started of { run_id : string }
  | Phase_started of { phase : string; total : int option }
  | Progress of progress
  | Mutant_settled of {
      result : Run_store.mutant_result;
      coverage : string;
    }
  | Warning of { code : string; message : string }
  | Run_finished of { exit_code : int }

type plain_state = { channel : out_channel; mutable last_progress_at : float }

type sink =
  | Silent
  | Plain of plain_state
  | Jsonl of out_channel
  | Callback of (event -> unit)

let plain channel = Plain { channel; last_progress_at = neg_infinity }
let jsonl channel = Jsonl channel
let silent = Silent
let callback consumer = Callback consumer
let mutex = Mutex.create ()
let current = ref (plain stderr)
let sequence = ref 0

let with_sink sink action =
  Mutex.lock mutex;
  let previous = !current in
  current := sink;
  sequence := 0;
  Mutex.unlock mutex;
  Fun.protect action ~finally:(fun () ->
      Mutex.lock mutex;
      current := previous;
      Mutex.unlock mutex)

let option_float = function None -> `Null | Some value -> `Float value

let outcome_name (result : Run_store.mutant_result) =
  match result.outcome with
  | Ocaml_mutants_core.Outcome.Killed -> "killed"
  | Ocaml_mutants_core.Outcome.Survived -> "survived"
  | Ocaml_mutants_core.Outcome.Timeout ->
      if result.timeout_confirmed then "timeout" else "timeout-unconfirmed"
  | Ocaml_mutants_core.Outcome.Inconclusive _ -> "inconclusive"
  | Ocaml_mutants_core.Outcome.Error _ -> "error"

let payload = function
  | Run_started { run_id } ->
      ("run-started", `Assoc [ ("run_id", `String run_id) ])
  | Phase_started { phase; total } ->
      ( "phase-started",
        `Assoc
          [
            ("phase", `String phase);
            ( "total",
              match total with None -> `Null | Some value -> `Int value );
          ] )
  | Progress progress ->
      ( "progress",
        `Assoc
          [
            ("phase", `String progress.phase);
            ("completed", `Int progress.completed);
            ("total", `Int progress.total);
            ("workers", `Int progress.workers);
            ("cache_hits", `Int progress.cache_hits);
            ("resume_hits", `Int progress.resume_hits);
            ("elapsed_seconds", `Float progress.elapsed_seconds);
            ("eta_seconds", option_float progress.eta_seconds);
          ] )
  | Mutant_settled settled ->
      let result = settled.result in
      ( "mutant-settled",
        `Assoc
          [
            ( "mutant_id",
              `String
                (Ocaml_mutants_core.Mutant.Id.full
                   (Ocaml_mutants_core.Mutant.id result.mutant)) );
            ("outcome", `String (outcome_name result));
            ("cached", `Bool result.cached);
            ( "duration_seconds",
              `Float
                (Ocaml_mutants_core.Duration.to_seconds result.duration) );
          ] )
  | Warning warning ->
      ( "warning",
        `Assoc
          [
            ("code", `String warning.code); ("message", `String warning.message);
          ] )
  | Run_finished { exit_code } ->
      ("run-finished", `Assoc [ ("exit_code", `Int exit_code) ])

let to_yojson ~sequence ~timestamp event =
  let kind, payload = payload event in
  `Assoc
    [
      ("document_type", `String "ocaml-mutants.event-v1");
      ("schema_version", `Int 1);
      ("sequence", `Int sequence);
      ("timestamp", `String timestamp);
      ("type", `String kind);
      ("payload", payload);
    ]

let print_plain state = function
  | Run_started { run_id } ->
      Printf.fprintf state.channel "ocaml-mutants: run %s started\n%!" run_id
  | Phase_started { phase; total = None } ->
      Printf.fprintf state.channel "ocaml-mutants: %s\n%!" phase
  | Phase_started { phase; total = Some total } ->
      Printf.fprintf state.channel "ocaml-mutants: %s (%d)\n%!" phase total
  | Progress progress ->
      let now = Unix.gettimeofday () in
      if
        progress.completed = progress.total
        || now -. state.last_progress_at >= 1.
      then (
        state.last_progress_at <- now;
        Printf.fprintf state.channel
          "ocaml-mutants: [%d/%d] %s; workers=%d cache=%d resume=%d \
           elapsed=%.1fs%s\n\
           %!"
          progress.completed progress.total progress.phase progress.workers
          progress.cache_hits progress.resume_hits progress.elapsed_seconds
          (match progress.eta_seconds with
          | None -> ""
          | Some eta -> Printf.sprintf " eta=%.1fs" eta))
  | Mutant_settled _ -> ()
  | Warning warning ->
      Printf.fprintf state.channel "ocaml-mutants: warning[%s]: %s\n%!"
        warning.code warning.message
  | Run_finished { exit_code } ->
      Printf.fprintf state.channel "ocaml-mutants: finished (exit %d)\n%!"
        exit_code

let emit event =
  Mutex.protect mutex (fun () ->
      let sink = !current in
      match sink with
      | Silent -> ()
      | Callback consumer -> consumer event
      | Plain state -> print_plain state event
      | Jsonl channel ->
          let current_sequence = !sequence in
          incr sequence;
          Yojson.Safe.to_channel channel
            (to_yojson ~sequence:current_sequence ~timestamp:(Util.timestamp ())
               event);
          output_char channel '\n';
          flush channel)
