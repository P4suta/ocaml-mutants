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

module Make (Backend : BACKEND) (Clock : CLOCK) = struct
  let run ?timeout ?(cancelled = fun () -> false) ~cwd ~env command =
    Backend.run ~now:Clock.now ~elapsed_seconds:Clock.elapsed_seconds
      ~sleep:Clock.sleep ?timeout ~cancelled ~cwd ~env command
end

external job_create : unit -> nativeint = "ocaml_mutants_job_create"
external job_assign : nativeint -> int -> bool = "ocaml_mutants_job_assign"
external job_terminate : nativeint -> bool = "ocaml_mutants_job_terminate"
external job_close : nativeint -> unit = "ocaml_mutants_job_close"
external windows_process_id : int -> int = "ocaml_mutants_windows_process_id"
external process_is_alive : int -> bool = "ocaml_mutants_process_is_alive"

external raw_open_witness : int -> nativeint
  = "ocaml_mutants_process_open_witness"

external raw_witness_is_alive : nativeint -> bool
  = "ocaml_mutants_process_witness_is_alive"

external raw_witness_close : nativeint -> unit
  = "ocaml_mutants_process_witness_close"

type liveness_witness = { witness_pid : int; witness_handle : nativeint }

let open_liveness_witness pid =
  {
    witness_pid = pid;
    witness_handle = (if Sys.win32 then raw_open_witness pid else 0n);
  }

let witness_pid witness = witness.witness_pid

let witness_is_alive witness =
  if Sys.win32 then
    witness.witness_handle <> 0n && raw_witness_is_alive witness.witness_handle
  else process_is_alive witness.witness_pid

let close_liveness_witness witness =
  if Sys.win32 then raw_witness_close witness.witness_handle

external posix_spawn_process :
  string
  * string array
  * string array
  * Unix.file_descr
  * Unix.file_descr
  * Unix.file_descr ->
  int = "ocaml_mutants_posix_spawn"

external windows_spawn_process_group :
  string
  * string array
  * string array
  * Unix.file_descr
  * Unix.file_descr
  * Unix.file_descr ->
  int = "ocaml_mutants_windows_spawn_process_group"

let process_id = windows_process_id
let helper_flag = "--ocaml-mutants-internal-process-helper"
let helper_environment = "OCAML_MUTANTS_HELPER_EXECUTABLE"

module Positive_seconds = struct
  type t = Seconds of float

  let make_exn ~name value =
    match classify_float value with
    | (FP_normal | FP_subnormal) when value > 0. -> Seconds value
    | FP_zero | FP_nan | FP_infinite | FP_normal | FP_subnormal ->
        invalid_arg (name ^ " must be finite and positive")

  let to_float (Seconds value) = value
end

module Positive_bytes = struct
  type t = Bytes of int

  let make_exn ~name value =
    if value > 0 then Bytes value else invalid_arg (name ^ " must be positive")

  let to_int (Bytes value) = value
end

type supervision_policy = {
  handshake_deadline : Positive_seconds.t;
  observation_interval : Positive_seconds.t;
  retained_output_per_stream : Positive_bytes.t;
  output_read_chunk : Positive_bytes.t;
}

let make_supervision_policy ~handshake_deadline_seconds
    ~observation_interval_seconds ~retained_output_per_stream_bytes
    ~output_read_chunk_bytes =
  let handshake_deadline =
    Positive_seconds.make_exn ~name:"handshake deadline"
      handshake_deadline_seconds
  in
  let observation_interval =
    Positive_seconds.make_exn ~name:"observation interval"
      observation_interval_seconds
  in
  let retained_output_per_stream =
    Positive_bytes.make_exn ~name:"retained output per stream"
      retained_output_per_stream_bytes
  in
  let output_read_chunk =
    Positive_bytes.make_exn ~name:"output read chunk" output_read_chunk_bytes
  in
  if
    Positive_seconds.to_float observation_interval
    >= Positive_seconds.to_float handshake_deadline
  then
    invalid_arg "observation interval must be shorter than handshake deadline";
  if Positive_bytes.to_int retained_output_per_stream mod 2 <> 0 then
    invalid_arg "retained output must split evenly between head and tail";
  if
    Positive_bytes.to_int output_read_chunk
    > Positive_bytes.to_int retained_output_per_stream / 2
  then invalid_arg "output read chunk must fit within one capture half";
  {
    handshake_deadline;
    observation_interval;
    retained_output_per_stream;
    output_read_chunk;
  }

(* This is one protocol contract shared by the supervisor and its helper. The
   handshake deadline is a startup-liveness bound, independent of a test's
   execution timeout. The output bound applies separately to stdout/stderr and
   is split evenly so both the first and last evidence are retained. *)
let production_policy =
  make_supervision_policy ~handshake_deadline_seconds:10.
    ~observation_interval_seconds:0.01
    ~retained_output_per_stream_bytes:(4 * 1024 * 1024)
    ~output_read_chunk_bytes:(64 * 1024)

type handshake_paths = { ready : string; go : string }

let handshake_paths seed = { ready = seed ^ ".ready"; go = seed ^ ".go" }

type helper_request = {
  handshake : string;
  cwd : string;
  target : string array;
}

let parse_helper_request argv =
  match Array.to_list argv with
  | _executable :: flag :: handshake :: cwd :: (_ :: _ as target)
    when String.equal flag helper_flag ->
      Some { handshake; cwd; target = Array.of_list target }
  | _ -> None

let configure_helper_executable executable =
  if String.trim executable = "" then
    invalid_arg "process helper executable must not be empty";
  Unix.putenv helper_environment executable

let helper_requested argv =
  match Array.to_list argv with
  | _executable :: flag :: _ -> String.equal flag helper_flag
  | _ -> false

let run_helper argv =
  let helper_failure_exit_code = 127 in
  let signal_exit_code_base = 128 in
  match parse_helper_request argv with
  | None -> helper_failure_exit_code
  | Some request -> (
      let paths = handshake_paths request.handshake in
      try
        if not Sys.win32 then ignore (Unix.setsid ());
        (match Util.atomic_write paths.ready "ready\n" with
        | Ok () -> ()
        | Error message -> failwith message);
        let started = Mtime_clock.now () in
        let elapsed () =
          Mtime.span started (Mtime_clock.now ()) |> Mtime.Span.to_float_ns
          |> fun nanoseconds -> nanoseconds *. 1e-9
        in
        while
          (not (Sys.file_exists paths.go))
          && elapsed ()
             < Positive_seconds.to_float production_policy.handshake_deadline
        do
          Unix.sleepf
            (Positive_seconds.to_float production_policy.observation_interval)
        done;
        if not (Sys.file_exists paths.go) then helper_failure_exit_code
        else (
          Sys.chdir request.cwd;
          if Sys.win32 then
            let pid =
              Unix.create_process_env request.target.(0) request.target
                (Unix.environment ()) Unix.stdin Unix.stdout Unix.stderr
            in
            match snd (Unix.waitpid [] pid) with
            | Unix.WEXITED code -> code
            | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
                signal_exit_code_base + signal
          else
            Unix.execvpe request.target.(0) request.target (Unix.environment ()))
      with exn ->
        prerr_endline ("ocaml-mutants process helper: " ^ Printexc.to_string exn);
        helper_failure_exit_code)

let updated_environment changes =
  let inherited = Unix.environment () in
  let table = Hashtbl.create (Array.length inherited + List.length changes) in
  inherited
  |> Array.iter (fun binding ->
      match String.index_opt binding '=' with
      | None -> ()
      | Some index ->
          Hashtbl.replace table
            (String.sub binding 0 index)
            (String.sub binding (index + 1) (String.length binding - index - 1)));
  List.iter
    (fun (name, value) ->
      match value with
      | Some value -> Hashtbl.replace table name value
      | None -> Hashtbl.remove table name)
    changes;
  Hashtbl.fold
    (fun name value values -> (name ^ "=" ^ value) :: values)
    table []
  |> Array.of_list

let capture_capacity_bytes =
  Positive_bytes.to_int production_policy.retained_output_per_stream

let capture_half_capacity_bytes = capture_capacity_bytes / 2

let output_read_chunk_bytes =
  Positive_bytes.to_int production_policy.output_read_chunk

type split_capture = {
  head : string;
  tail : bytes;
  mutable tail_start : int;
  mutable tail_length : int;
}

type capture_state = Full of Buffer.t | Split of split_capture
type capture = { mutable state : capture_state; mutable total : int }

let capture_create () =
  { state = Full (Buffer.create output_read_chunk_bytes); total = 0 }

let split_feed split bytes offset length =
  let capacity = Bytes.length split.tail in
  if length >= capacity then (
    Bytes.blit bytes (offset + length - capacity) split.tail 0 capacity;
    split.tail_start <- 0;
    split.tail_length <- capacity)
  else
    let rec copy offset remaining =
      if remaining > 0 then (
        let write_at = (split.tail_start + split.tail_length) mod capacity in
        let available = min remaining (capacity - write_at) in
        Bytes.blit bytes offset split.tail write_at available;
        if split.tail_length < capacity then
          split.tail_length <- split.tail_length + available
        else split.tail_start <- (split.tail_start + available) mod capacity;
        copy (offset + available) (remaining - available))
    in
    copy offset length

let capture_feed capture bytes offset length =
  capture.total <- capture.total + length;
  match capture.state with
  | Split split -> split_feed split bytes offset length
  | Full buffer ->
      if Buffer.length buffer + length <= capture_capacity_bytes then
        Buffer.add_subbytes buffer bytes offset length
      else
        let combined =
          Buffer.contents buffer ^ Bytes.sub_string bytes offset length
        in
        let split =
          {
            head = String.sub combined 0 capture_half_capacity_bytes;
            tail = Bytes.create capture_half_capacity_bytes;
            tail_start = 0;
            tail_length = 0;
          }
        in
        let tail_offset =
          max capture_half_capacity_bytes
            (String.length combined - capture_half_capacity_bytes)
        in
        let tail_bytes = Bytes.unsafe_of_string combined in
        split_feed split tail_bytes tail_offset
          (String.length combined - tail_offset);
        capture.state <- Split split

let capture_contents capture =
  match capture.state with
  | Full buffer -> (Buffer.contents buffer, false, capture.total)
  | Split split ->
      let tail = Bytes.create split.tail_length in
      let first =
        min split.tail_length (Bytes.length split.tail - split.tail_start)
      in
      Bytes.blit split.tail split.tail_start tail 0 first;
      if first < split.tail_length then
        Bytes.blit split.tail 0 tail first (split.tail_length - first);
      (split.head ^ Bytes.unsafe_to_string tail, true, capture.total)

let drain descriptor =
  let capture = capture_create () in
  let chunk = Bytes.create output_read_chunk_bytes in
  Fun.protect
    ~finally:(fun () ->
      try Unix.close descriptor with Unix.Unix_error _ -> ())
    (fun () ->
      let rec loop () =
        match Unix.read descriptor chunk 0 (Bytes.length chunk) with
        | 0 -> ()
        | count ->
            capture_feed capture chunk 0 count;
            loop ()
      in
      (try loop () with Unix.Unix_error _ -> ());
      capture_contents capture)

let monotonic_elapsed started =
  Mtime.span started (Mtime_clock.now ()) |> Mtime.Span.to_float_ns
  |> fun nanoseconds -> nanoseconds *. 1e-9

type helper_readiness = Helper_ready | Helper_cancelled | Helper_failed

let wait_for_helper ~cancelled paths =
  let started = Mtime_clock.now () in
  let rec wait () =
    if Sys.file_exists paths.ready then Helper_ready
    else if cancelled () then Helper_cancelled
    else if
      monotonic_elapsed started
      >= Positive_seconds.to_float production_policy.handshake_deadline
    then Helper_failed
    else (
      Unix.sleepf
        (Positive_seconds.to_float production_policy.observation_interval);
      wait ())
  in
  wait ()

let cleanup_handshake handshake =
  let paths = handshake_paths handshake in
  List.iter
    (fun path -> try Sys.remove path with Sys_error _ -> ())
    [ handshake; paths.ready; paths.go ]

let terminate_helper pid =
  try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()

let terminate ~pid ~job ~job_assigned =
  if Sys.win32 then
    (* Before Job assignment the helper is still behind the go handshake and
       therefore has no child. After assignment the Job is the process-tree
       owner; closing it remains the final kill-on-close backstop. *)
    if job_assigned then (if not (job_terminate job) then terminate_helper pid)
    else terminate_helper pid
  else
    try Unix.kill (-pid) Sys.sigkill
    with Unix.Unix_error _ -> terminate_helper pid

let terminate_descendants ~pid ~job ~job_assigned =
  if Sys.win32 then (if job_assigned then ignore (job_terminate job))
  else try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> ()

let wait ~pid ~job ~job_assigned ~timeout ~cancelled =
  let started = Mtime_clock.now () in
  let rec poll () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        let elapsed = monotonic_elapsed started in
        if cancelled () then (
          terminate ~pid ~job ~job_assigned;

          ignore (Unix.waitpid [] pid);
          Cancelled)
        else if
          Option.fold ~none:false ~some:(fun limit -> elapsed >= limit) timeout
        then (
          terminate ~pid ~job ~job_assigned;

          ignore (Unix.waitpid [] pid);
          Timed_out)
        else (
          Unix.sleepf
            (Positive_seconds.to_float production_policy.observation_interval);
          poll ())
    | _, Unix.WEXITED code -> Exited code
    | _, Unix.WSIGNALED signal -> Signaled signal
    | _, Unix.WSTOPPED signal -> Signaled signal
  in
  poll ()

let run ?timeout ?(cancelled = fun () -> false) ?cancel ~cwd ~env command =
  let started = Mtime_clock.now () in
  let cancelled () =
    cancelled () || Option.fold ~none:false ~some:Cancel.is_requested cancel
  in
  if cancelled () then
    {
      status = Cancelled;
      stdout = "";
      stderr = "";
      stdout_truncated = false;
      stderr_truncated = false;
      stdout_bytes = 0;
      stderr_bytes = 0;
      duration = monotonic_elapsed started;
    }
  else
    match command with
    | [] ->
        {
          status = Spawn_error "empty command";
          stdout = "";
          stderr = "";
          stdout_truncated = false;
          stderr_truncated = false;
          stdout_bytes = 0;
          stderr_bytes = 0;
          duration = monotonic_elapsed started;
        }
    | _ :: _ -> (
        let stdout_read, stdout_fd = Unix.pipe ~cloexec:true () in
        let stderr_read, stderr_fd = Unix.pipe ~cloexec:true () in
        let environment =
          updated_environment (env @ [ (helper_environment, None) ])
        in
        let job = if Sys.win32 then job_create () else Nativeint.zero in
        let stdout_capture = ref None in
        let stderr_capture = ref None in
        let stdout_thread = ref None in
        let stderr_thread = ref None in
        let spawned_pid = ref None in
        let assigned_to_job = ref false in
        let handshake_path = ref None in
        let close_fds () =
          Unix.close stdout_fd;
          Unix.close stderr_fd
        in
        let start_drains () =
          close_fds ();
          stdout_thread :=
            Some
              (Thread.create
                 (fun () -> stdout_capture := Some (drain stdout_read))
                 ());
          stderr_thread :=
            Some
              (Thread.create
                 (fun () -> stderr_capture := Some (drain stderr_read))
                 ())
        in
        let finish status =
          Option.iter Thread.join !stdout_thread;
          Option.iter Thread.join !stderr_thread;
          let stdout, stdout_truncated, stdout_bytes =
            Option.value !stdout_capture ~default:("", false, 0)
          in
          let stderr, stderr_truncated, stderr_bytes =
            Option.value !stderr_capture ~default:("", false, 0)
          in
          {
            status;
            stdout;
            stderr;
            stdout_truncated;
            stderr_truncated;
            stdout_bytes;
            stderr_bytes;
            duration = monotonic_elapsed started;
          }
        in
        try
          let helper =
            match Sys.getenv_opt helper_environment with
            | Some helper when String.trim helper <> "" -> helper
            | _ ->
                failwith
                  "process helper is not configured; initialize the process \
                   supervisor before running commands"
          in
          let handshake =
            Filename.temp_file "ocaml-mutants-handshake-" ".tmp"
          in
          let paths = handshake_paths handshake in
          handshake_path := Some handshake;
          Sys.remove handshake;
          let helper_argv =
            Array.of_list ([ helper; helper_flag; handshake; cwd ] @ command)
          in
          let pid =
            let spawn =
              if Sys.win32 then windows_spawn_process_group
              else posix_spawn_process
            in
            let pid =
              spawn
                ( helper,
                  helper_argv,
                  environment,
                  Unix.stdin,
                  stdout_fd,
                  stderr_fd )
            in
            if pid < 0 then
              raise
                (Unix.Unix_error
                   ( Unix.EUNKNOWNERR (-pid),
                     (if Sys.win32 then "CreateProcessW" else "posix_spawnp"),
                     helper ))
            else pid
          in
          spawned_pid := Some pid;
          start_drains ();
          let job_assigned, status =
            match wait_for_helper ~cancelled paths with
            | Helper_cancelled ->
                terminate ~pid ~job ~job_assigned:false;
                ignore (Unix.waitpid [] pid);
                (false, Cancelled)
            | Helper_failed ->
                terminate ~pid ~job ~job_assigned:false;
                ignore (Unix.waitpid [] pid);
                (false, Spawn_error "process helper handshake failed")
            | Helper_ready -> (
                let assigned = Sys.win32 && job_assign job pid in
                assigned_to_job := assigned;
                if Sys.win32 && not assigned then (
                  terminate ~pid ~job ~job_assigned:false;
                  ignore (Unix.waitpid [] pid);
                  (false, Spawn_error "could not assign process helper to Job"))
                else
                  match Util.atomic_write paths.go "go\n" with
                  | Error message ->
                      terminate ~pid ~job ~job_assigned:assigned;
                      ignore (Unix.waitpid [] pid);
                      (assigned, Spawn_error message)
                  | Ok () ->
                      ( assigned,
                        wait ~pid ~job ~job_assigned:assigned ~timeout
                          ~cancelled ))
          in
          cleanup_handshake handshake;
          terminate_descendants ~pid ~job ~job_assigned;
          if Sys.win32 then job_close job;
          finish status
        with exn ->
          Option.iter cleanup_handshake !handshake_path;
          Option.iter
            (fun pid ->
              terminate ~pid ~job ~job_assigned:!assigned_to_job;
              try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ())
            !spawned_pid;
          (try close_fds () with Unix.Unix_error _ -> ());
          (try Unix.close stdout_read with Unix.Unix_error _ -> ());
          (try Unix.close stderr_read with Unix.Unix_error _ -> ());
          if Sys.win32 then job_close job;
          finish (Spawn_error (Printexc.to_string exn)))

let status_string = function
  | Exited code -> Printf.sprintf "exit %d" code
  | Signaled signal -> Printf.sprintf "signal %d" signal
  | Timed_out -> "timeout"
  | Cancelled -> "cancelled"
  | Spawn_error message -> "spawn error: " ^ message

let succeeded result = match result.status with Exited 0 -> true | _ -> false
