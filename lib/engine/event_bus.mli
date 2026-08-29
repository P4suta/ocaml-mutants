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

type sink

val plain : out_channel -> sink
val jsonl : out_channel -> sink
val silent : sink
val callback : (event -> unit) -> sink
val with_sink : sink -> (unit -> 'a) -> 'a
val emit : event -> unit
val to_yojson : sequence:int -> timestamp:string -> event -> Yojson.Safe.t
(** Callback sinks receive the complete, already bounded/redacted settled
    result so an interactive consumer can render evidence immediately. The
    public JSONL projection remains deliberately compact and contains only the
    stable ID, outcome, cache flag, and duration. *)
