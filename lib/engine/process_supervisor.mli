type status =
  | Exited of int
  | Signaled of int
  | Timed_out
  | Cancelled
  | Spawn_error of string

type result = {
  status : status;
  stdout : string;
  stderr : string;
  stdout_truncated : bool;
  stderr_truncated : bool;
  stdout_bytes : int;
  stderr_bytes : int;
  duration : float;
}

module type CLOCK = sig
  val now : unit -> Mtime.t
  val elapsed_seconds : Mtime.t -> float
  val sleep : float -> unit
end

module type BACKEND = sig
  val run :
    now:(unit -> Mtime.t) ->
    elapsed_seconds:(Mtime.t -> float) ->
    sleep:(float -> unit) ->
    ?timeout:float ->
    cancelled:(unit -> bool) ->
    cwd:string ->
    env:(string * string option) list ->
    string list ->
    result
end

module Make (Backend : BACKEND) (Clock : CLOCK) : sig
  val run :
    ?timeout:float ->
    ?cancelled:(unit -> bool) ->
    cwd:string ->
    env:(string * string option) list ->
    string list ->
    result
end

val run :
  ?timeout:float ->
  ?cancelled:(unit -> bool) ->
  ?cancel:Cancel.t ->
  cwd:string ->
  env:(string * string option) list ->
  string list ->
  result
(** [run] releases the target only after its process tree is owned by the
    platform supervisor. [timeout] is the post-release execution deadline;
    [result.duration] also includes supervised startup and cleanup. Normal
    completion, timeout, cancellation, and spawn failure all reclaim the owned
    process tree. *)

val status_string : status -> string
val succeeded : result -> bool

val capture_capacity_bytes : int
(** Maximum retained bytes for each of stdout and stderr. Once exceeded, the
    capacity is divided equally between the beginning and end of the stream;
    [stdout_bytes] and [stderr_bytes] still report the full drained totals. *)

val configure_helper_executable : string -> unit
val helper_requested : string array -> bool
val run_helper : string array -> int
val process_id : int -> int
val process_is_alive : int -> bool

type liveness_witness
(** Exact liveness authority for one observed process. On Windows the witness
    retains a handle opened while the process was provably alive, so a recycled
    PID can never masquerade as the witnessed process, and a PID whose handle
    cannot be opened is reported as already dead. On POSIX the witness answers
    with PID liveness. *)

val open_liveness_witness : int -> liveness_witness
val witness_pid : liveness_witness -> int
val witness_is_alive : liveness_witness -> bool
val close_liveness_witness : liveness_witness -> unit
