module Core = Ocaml_mutants_core
module Engine = Ocaml_mutants_engine

exception Contract_failure of string
exception Unsupported_environment of string

let fail format =
  Printf.ksprintf (fun message -> raise (Contract_failure message)) format

let get_ok label = function
  | Ok value -> value
  | Error message -> fail "%s: %s" label message

module Positive_seconds = struct
  type t = Seconds of float

  let make_exn ~name value =
    match classify_float value with
    | (FP_normal | FP_subnormal) when value > 0. -> Seconds value
    | FP_zero | FP_nan | FP_infinite | FP_normal | FP_subnormal ->
        invalid_arg (name ^ " must be finite and positive")

  let to_float (Seconds value) = value

  let to_milliseconds value =
    let milliseconds = ceil (to_float value *. 1_000.) in
    if milliseconds > float_of_int max_int then
      invalid_arg "deadline does not fit the native millisecond boundary";
    int_of_float milliseconds
end

module Positive_bytes = struct
  type t = Bytes of int

  let make_exn ~name value =
    if value > 0 then Bytes value else invalid_arg (name ^ " must be positive")

  let to_int (Bytes value) = value
end

type contract_policy = {
  readiness_deadline : Positive_seconds.t;
  shutdown_deadline : Positive_seconds.t;
  cleanup_deadline : Positive_seconds.t;
  observation_interval : Positive_seconds.t;
  stage_timeout : Positive_seconds.t;
  protocol_limit : Positive_bytes.t;
  diagnostic_limit : Positive_bytes.t;
}

let policy =
  {
    readiness_deadline =
      Positive_seconds.make_exn ~name:"readiness deadline" 240.;
    shutdown_deadline = Positive_seconds.make_exn ~name:"shutdown deadline" 60.;
    (* Sixty seconds, aligned with the shutdown deadline: loaded CI runners take
       longer than 15 seconds to tear down the interrupted Job tree. The passing
       path polls every 10ms and exits as soon as the tree is gone, so this
       budget only lengthens genuine failures, never green runs. *)
    cleanup_deadline = Positive_seconds.make_exn ~name:"cleanup deadline" 60.;
    observation_interval =
      Positive_seconds.make_exn ~name:"observation interval" 0.01;
    stage_timeout = Positive_seconds.make_exn ~name:"stage timeout" 120.;
    protocol_limit =
      Positive_bytes.make_exn ~name:"readiness protocol limit" (16 * 1024);
    diagnostic_limit =
      Positive_bytes.make_exn ~name:"CLI diagnostic limit" (64 * 1024);
  }

module Deadline = struct
  type t = { started : Mtime.t; limit : Positive_seconds.t }

  let start limit = { started = Mtime_clock.now (); limit }

  let elapsed deadline =
    Mtime.span deadline.started (Mtime_clock.now ()) |> Mtime.Span.to_float_ns
    |> fun nanoseconds -> nanoseconds *. 1e-9

  let remaining deadline =
    max 0. (Positive_seconds.to_float deadline.limit -. elapsed deadline)

  let expired deadline = remaining deadline <= 0.
end

module Native_launcher = struct
  type process
  type start_result = Started of process | Unsupported of int | Failed of int

  external start_raw : string array * string * string -> start_result
    = "ocaml_mutants_test_interrupt_start"

  external pid_raw : process -> int = "ocaml_mutants_test_interrupt_pid"
  external send_raw : process -> int * int = "ocaml_mutants_test_interrupt_send"

  external wait_raw : process -> int -> int -> int * int
    = "ocaml_mutants_test_interrupt_wait"

  external terminate_raw : process -> int * int
    = "ocaml_mutants_test_interrupt_terminate"

  external close_raw : process -> unit = "ocaml_mutants_test_interrupt_close"

  type wait_result =
    | Exited of int
    | Signaled of int
    | Timed_out
    | Wait_failed of int

  type owned = {
    process : process;
    stdout_path : string;
    stderr_path : string;
    mutable reaped : bool;
    mutable interrupt_delivered : bool;
  }

  let platform_error ~operation code =
    Printf.sprintf "%s failed on %s with native error %d" operation Sys.os_type
      code

  let read_bounded path =
    try
      let channel = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () ->
          let length = in_channel_length channel in
          let limit = Positive_bytes.to_int policy.diagnostic_limit in
          if length <= limit then really_input_string channel length
          else
            let half = limit / 2 in
            let head = really_input_string channel half in
            seek_in channel (length - half);
            let tail = really_input_string channel half in
            Printf.sprintf "%s\n...[retained %d of %d bytes]...\n%s" head limit
              length tail)
    with Sys_error message -> "<unavailable: " ^ message ^ ">"

  let diagnostics_paths ~stdout_path ~stderr_path =
    Printf.sprintf "stdout:\n%s\nstderr:\n%s" (read_bounded stdout_path)
      (read_bounded stderr_path)

  let diagnostics owned =
    diagnostics_paths ~stdout_path:owned.stdout_path
      ~stderr_path:owned.stderr_path

  let start ~stdout_path ~stderr_path arguments =
    match start_raw (arguments, stdout_path, stderr_path) with
    | Started process ->
        Ok
          {
            process;
            stdout_path;
            stderr_path;
            reaped = false;
            interrupt_delivered = false;
          }
    | Unsupported code ->
        raise
          (Unsupported_environment
             (platform_error ~operation:"isolated CLI launch" code))
    | Failed code ->
        fail "%s\n%s"
          (platform_error ~operation:"CLI launch" code)
          (diagnostics_paths ~stdout_path ~stderr_path)

  let send_interrupt owned =
    if owned.interrupt_delivered then
      fail "the one-shot OS interrupt capability was consumed twice";
    owned.interrupt_delivered <- true;
    match send_raw owned.process with
    | 0, 0 -> ()
    | 1, code ->
        raise
          (Unsupported_environment
             (platform_error ~operation:"targeted console interrupt" code))
    | _, code ->
        fail "%s" (platform_error ~operation:"OS interrupt delivery" code)

  let pid owned = pid_raw owned.process

  let decode_wait owned = function
    | 0, code ->
        owned.reaped <- true;
        Exited code
    | 1, signal ->
        owned.reaped <- true;
        Signaled signal
    | 2, _ -> Timed_out
    | _, code -> Wait_failed code

  let wait owned deadline =
    let observation =
      Positive_seconds.to_milliseconds policy.observation_interval
    in
    wait_raw owned.process
      (Positive_seconds.to_milliseconds deadline)
      observation
    |> decode_wait owned

  let poll owned =
    let quantum =
      Positive_seconds.to_milliseconds policy.observation_interval
    in
    match wait_raw owned.process quantum quantum |> decode_wait owned with
    | Timed_out -> None
    | result -> Some result

  let cleanup owned =
    let problem = ref None in
    let record message = if !problem = None then problem := Some message in
    (if not owned.reaped then
       match terminate_raw owned.process with
       | 0, _ -> ()
       | _, code ->
           record
             (platform_error ~operation:"owned CLI cleanup termination" code));
    (if not owned.reaped then
       match wait owned policy.cleanup_deadline with
       | Exited _ | Signaled _ -> ()
       | Timed_out -> record "owned CLI cleanup exceeded its typed deadline"
       | Wait_failed code ->
           record (platform_error ~operation:"owned CLI cleanup wait" code));
    close_raw owned.process;
    Option.iter (fun message -> fail "%s" message) !problem

  let with_process ~stdout_path ~stderr_path arguments action =
    let owned =
      get_ok "cannot own launched CLI"
        (start ~stdout_path ~stderr_path arguments)
    in
    Fun.protect ~finally:(fun () -> cleanup owned) (fun () -> action owned)
end

let write path contents =
  get_ok ("cannot write " ^ path) (Engine.Util.atomic_write path contents)

let normalize path =
  let normalized = Core.Mutant.normalize_path path in
  if Sys.win32 then String.lowercase_ascii normalized else normalized

let within ~parent child =
  let parent = normalize parent in
  let child = normalize child in
  String.equal parent child || String.starts_with ~prefix:(parent ^ "/") child

type layout = {
  parent : string;
  workspace : string;
  cache : string;
  control : string;
  owner_marker : string;
  owner_contents : string;
  nonce : string;
}

let owner_marker = ".cli-interrupt-contract-owner"

let create_layout () =
  let parent =
    (* The embedded spaces exercise both native Windows argv encoders rather
       than letting this contract pass only for shell-friendly paths. *)
    Filename.temp_dir ~perms:0o700 "ocaml mutants cli interrupt-" ".tmp"
    |> Unix.realpath
  in
  let workspace = Filename.concat parent "workspace" in
  let cache = Filename.concat parent "cache" in
  let control = Filename.concat parent "control" in
  Unix.mkdir workspace 0o700;
  Unix.mkdir cache 0o700;
  Unix.mkdir control 0o700;
  let nonce =
    Engine.Util.sha256
      (Printf.sprintf "%s\000%d\000%.17g" parent (Unix.getpid ())
         (Unix.gettimeofday ()))
  in
  let owner_contents =
    Printf.sprintf "owner=cli-interrupt-contract\nnonce=%s\n" nonce
  in
  List.iter
    (fun directory ->
      write (Filename.concat directory owner_marker) owner_contents)
    [ parent; workspace; control ];
  { parent; workspace; cache; control; owner_marker; owner_contents; nonce }

let cleanup_layout layout =
  let temp_root = Unix.realpath (Filename.get_temp_dir_name ()) in
  let parent = Unix.realpath layout.parent in
  if String.equal (normalize temp_root) (normalize parent) then
    fail "refusing to remove the temporary root itself";
  if not (within ~parent:temp_root parent) then
    fail "owned interrupt-contract directory escaped the temporary root: %s"
      parent;
  let check_owner directory =
    let path = Filename.concat directory layout.owner_marker in
    let actual =
      get_ok
        ("cannot read ownership marker " ^ path)
        (Engine.Util.read_file path)
    in
    if not (String.equal actual layout.owner_contents) then
      fail "ownership marker changed: %s" path
  in
  List.iter check_owner [ parent; layout.workspace; layout.control ];
  get_ok "cannot remove owned interrupt-contract directory"
    (Engine.Util.remove_tree parent);
  if Sys.file_exists parent then
    fail "owned interrupt-contract directory remains"

let source_digest layout =
  let skip relative =
    Engine.Workspace_snapshot.default_skip relative
    || String.equal relative layout.owner_marker
  in
  get_ok "cannot digest source workspace"
    (Engine.Util.digest_tree ~skip layout.workspace)

let hex_encode value =
  let result = Bytes.create (String.length value * 2) in
  let digits = "0123456789abcdef" in
  String.iteri
    (fun index character ->
      let code = Char.code character in
      Bytes.set result (index * 2) digits.[code lsr 4];
      Bytes.set result ((index * 2) + 1) digits.[code land 0x0f])
    value;
  Bytes.unsafe_to_string result

let hex_value = function
  | '0' .. '9' as value -> Char.code value - Char.code '0'
  | 'a' .. 'f' as value -> Char.code value - Char.code 'a' + 10
  | 'A' .. 'F' as value -> Char.code value - Char.code 'A' + 10
  | value -> fail "invalid hexadecimal protocol byte %C" value

let hex_decode value =
  if String.length value mod 2 <> 0 then fail "odd hexadecimal protocol field";
  String.init
    (String.length value / 2)
    (fun index ->
      Char.chr
        ((hex_value value.[index * 2] lsl 4)
        lor hex_value value.[(index * 2) + 1]))

let write_all descriptor value =
  let rec write_from offset =
    if offset < String.length value then (
      let written =
        Unix.write_substring descriptor value offset
          (String.length value - offset)
      in
      if written = 0 then fail "readiness socket accepted a zero-byte write";
      write_from (offset + written))
  in
  write_from 0

let marker_contents ~nonce ~role ~pid ~job ~cwd =
  Printf.sprintf
    "owner=cli-interrupt-contract\nnonce=%s\nrole=%s\npid=%d\njob=%s\ncwd=%s\n"
    nonce role pid job (hex_encode cwd)

(* Diagnostic evidence for the supervision investigation (issue #3): whether the
   tree member sits inside any Windows Job at readiness time. CI runners may
   wrap steps in their own Job, so "yes" is inconclusive while "no" is a
   definitive escape from the supervisor's Job as well. *)
let job_membership_string () =
  match Engine.Process_supervisor.current_job_membership () with
  | Engine.Process_supervisor.In_some_job -> "yes"
  | Engine.Process_supervisor.In_no_job -> "no"
  | Engine.Process_supervisor.Membership_unknown -> "unknown"

let notify_and_block ~host ~port ~nonce ~control ~role =
  let pid = Unix.getpid () in
  let job = job_membership_string () in
  let cwd = Unix.realpath (Sys.getcwd ()) in
  let marker = Filename.concat control (role ^ ".ready") in
  write marker (marker_contents ~nonce ~role ~pid ~job ~cwd);
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.set_close_on_exec socket;
  Fun.protect
    ~finally:(fun () -> try Unix.close socket with Unix.Unix_error _ -> ())
    (fun () ->
      Unix.connect socket
        (Unix.ADDR_INET (Unix.inet_addr_of_string host, int_of_string port));
      write_all socket
        (Printf.sprintf "%s\t%s\t%d\t%s\t%s\n" nonce role pid job
           (hex_encode cwd));
      let byte = Bytes.create 1 in
      let rec await_owner_close () =
        match Unix.read socket byte 0 1 with
        | 0 -> ()
        | _ -> await_owner_close ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> await_owner_close ()
      in
      await_owner_close ())

let stage_flag = "--cli-interrupt-stage"
let child_flag = "--cli-interrupt-child"
let grandchild_flag = "--cli-interrupt-grandchild"

(* Windows reports an unhandled console-control event as this signed NTSTATUS.
   Seeing it here proves that CTRL_BREAK bypassed Application's cancellation
   lifecycle, even though the operating system delivered the event to the
   intended process group. *)
let windows_status_control_c_exit = -1_073_741_510

let argv_probe =
  "spaces; doubled-slash=\\\\; slash-before-quote=\\\"survive\\\""

let helper_arguments () =
  if Array.length Sys.argv <> 7 then
    fail
      "interrupt helper expected host, port, nonce, control directory, and \
       argv probe";
  if not (String.equal Sys.argv.(6) argv_probe) then
    fail "native process launch changed the argv quoting probe";
  (Sys.argv.(2), Sys.argv.(3), Sys.argv.(4), Sys.argv.(5))

let spawn_helper flag host port nonce control =
  let executable = Unix.realpath Sys.executable_name in
  let arguments =
    [| executable; flag; host; port; nonce; control; argv_probe |]
  in
  ignore
    (Unix.create_process executable arguments Unix.stdin Unix.stdout Unix.stderr)

let run_stage () =
  match Sys.getenv_opt "OCAML_MUTANTS_ACTIVE" with
  | None | Some "" -> ()
  | Some _ ->
      let host, port, nonce, control = helper_arguments () in
      spawn_helper child_flag host port nonce control;
      Printf.printf "interrupt-stage-stdout:%s\n%!" nonce;
      Printf.eprintf "interrupt-stage-stderr:%s\n%!" nonce;
      notify_and_block ~host ~port ~nonce ~control ~role:"stage"

let run_child () =
  let host, port, nonce, control = helper_arguments () in
  spawn_helper grandchild_flag host port nonce control;
  notify_and_block ~host ~port ~nonce ~control ~role:"child"

let run_grandchild () =
  let host, port, nonce, control = helper_arguments () in
  notify_and_block ~host ~port ~nonce ~control ~role:"grandchild"

type readiness = {
  role : string;
  pid : int;
  in_job : string;
  snapshot : string;
}

let read_protocol_line deadline descriptor =
  Unix.set_nonblock descriptor;
  let limit = Positive_bytes.to_int policy.protocol_limit in
  let buffer = Buffer.create 256 in
  let chunk = Bytes.create 256 in
  let rec read () =
    if Buffer.length buffer > limit then
      fail "readiness protocol exceeded its typed byte limit";
    match String.index_opt (Buffer.contents buffer) '\n' with
    | Some index -> String.sub (Buffer.contents buffer) 0 index
    | None -> (
        let remaining = Deadline.remaining deadline in
        if remaining <= 0. then fail "readiness protocol exceeded its deadline";
        let readable, _, _ = Unix.select [ descriptor ] [] [] remaining in
        if readable = [] then fail "readiness protocol exceeded its deadline";
        match Unix.read descriptor chunk 0 (Bytes.length chunk) with
        | 0 -> fail "readiness peer closed before its complete record"
        | count ->
            Buffer.add_subbytes buffer chunk 0 count;
            read ()
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
            read ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> read ())
  in
  read ()

let parse_readiness ~nonce line =
  match String.split_on_char '\t' line with
  | [ actual_nonce; role; pid; in_job; snapshot ] ->
      if not (String.equal actual_nonce nonce) then
        fail "readiness nonce did not authenticate the owned process";
      if not (List.mem role [ "stage"; "child"; "grandchild" ]) then
        fail "unknown readiness role %S" role;
      let pid =
        match int_of_string_opt pid with
        | Some pid when pid > 0 -> pid
        | Some _ | None -> fail "readiness role %s supplied an invalid PID" role
      in
      if not (List.mem in_job [ "yes"; "no"; "unknown" ]) then
        fail "readiness role %s supplied an invalid job membership %S" role
          in_job;
      { role; pid; in_job; snapshot = hex_decode snapshot }
  | _ -> fail "malformed readiness record: %S" line

let create_listener () =
  let descriptor = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.set_close_on_exec descriptor;
  Unix.setsockopt descriptor Unix.SO_REUSEADDR true;
  Unix.bind descriptor (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen descriptor 8;
  let port =
    match Unix.getsockname descriptor with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> fail "loopback readiness listener was not INET"
  in
  (descriptor, port)

let await_readiness layout listener connections owned =
  let deadline = Deadline.start policy.readiness_deadline in
  let seen = Hashtbl.create 3 in
  while Hashtbl.length seen < 3 do
    let remaining = Deadline.remaining deadline in
    if remaining <= 0. then fail "process-tree readiness exceeded its deadline";
    let interval =
      min remaining (Positive_seconds.to_float policy.observation_interval)
    in
    let readable, _, _ = Unix.select [ listener ] [] [] interval in
    match readable with
    | [] -> (
        match Native_launcher.poll owned with
        | None -> ()
        | Some result ->
            let termination =
              match result with
              | Native_launcher.Exited code -> Printf.sprintf "exit %d" code
              | Native_launcher.Signaled signal ->
                  Printf.sprintf "signal %d" signal
              | Native_launcher.Timed_out -> "poll timeout"
              | Native_launcher.Wait_failed code ->
                  Printf.sprintf "native wait failure %d" code
            in
            fail "CLI terminated before process-tree readiness (%s)\n%s"
              termination
              (Native_launcher.diagnostics owned))
    | _ ->
        let descriptor, _ = Unix.accept listener in
        Unix.set_close_on_exec descriptor;
        connections := descriptor :: !connections;
        let ready =
          read_protocol_line deadline descriptor
          |> parse_readiness ~nonce:layout.nonce
        in
        if Hashtbl.mem seen ready.role then
          fail "readiness role %s connected more than once" ready.role;
        let marker = Filename.concat layout.control (ready.role ^ ".ready") in
        let actual =
          get_ok ("cannot read " ^ marker) (Engine.Util.read_file marker)
        in
        let expected =
          marker_contents ~nonce:layout.nonce ~role:ready.role ~pid:ready.pid
            ~job:ready.in_job ~cwd:ready.snapshot
        in
        if not (String.equal actual expected) then
          fail "readiness marker for %s did not match its authenticated record"
            ready.role;
        Hashtbl.add seen ready.role ready
  done;
  [ "stage"; "child"; "grandchild" ]
  |> List.map (fun role -> Hashtbl.find seen role)

let write_fixture layout executable port =
  write
    (Filename.concat layout.workspace "dune-project")
    "(lang dune 3.21)\n(name cli_interrupt_contract)\n";
  write
    (Filename.concat layout.workspace "dune")
    "(library\n (name subject)\n (modules subject))\n";
  write
    (Filename.concat layout.workspace "subject.ml")
    "let first = true\n\
     let second = false\n\
     let choose value = if value then first else second\n";
  let timeout = Positive_seconds.to_float policy.stage_timeout in
  let config =
    Printf.sprintf
      {|version = 1

[mutation]
include = ["subject.ml"]
profile = "balanced"

[test]
timeout = %.1f
baseline_runs = 1
parallel_safe = false

[[test.stages]]
name = "interrupt-ready-tree"
command = [%S, %S, "127.0.0.1", %S, %S, %S, %S]

[execution]
jobs = 1

[cache]
mode = "off"
directory = %S
|}
      timeout executable stage_flag (string_of_int port) layout.nonce
      layout.control argv_probe layout.cache
  in
  write (Filename.concat layout.workspace ".ocaml-mutants.toml") config

let validate_snapshot_path snapshot =
  let temporary = Unix.realpath (Filename.get_temp_dir_name ()) in
  let canonical = Unix.realpath snapshot in
  if not (within ~parent:temporary canonical) then
    fail "instrumented stage escaped the temporary root: %s" canonical;
  if
    not
      (String.equal
         (normalize (Filename.dirname canonical))
         (normalize temporary))
  then fail "snapshot was not an immediate temporary-root child: %s" canonical;
  if
    not
      (String.starts_with ~prefix:"ocaml-mutants-snapshot-"
         (Filename.basename canonical))
  then fail "instrumented stage did not run in an owned snapshot: %s" canonical;
  canonical

type readiness_disconnect = Graceful_eof | Windows_connection_reset

let wait_for_connection_shutdown connections =
  let deadline = Deadline.start policy.shutdown_deadline in
  let pending = ref !connections in
  let saw_windows_reset = ref false in
  let byte = Bytes.create 1 in
  let close_terminal descriptor =
    Unix.close descriptor;
    pending := List.filter (( <> ) descriptor) !pending;
    connections := List.filter (( <> ) descriptor) !connections
  in
  while !pending <> [] do
    let remaining = Deadline.remaining deadline in
    if remaining <= 0. then
      fail "%d process-tree readiness sockets remained open"
        (List.length !pending);
    let readable, _, _ = Unix.select !pending [] [] remaining in
    if readable = [] then
      fail "%d process-tree readiness sockets remained open"
        (List.length !pending);
    List.iter
      (fun descriptor ->
        match Unix.read descriptor byte 0 1 with
        | 0 -> close_terminal descriptor
        | _ -> fail "readiness socket emitted bytes after cancellation"
        | exception Unix.Unix_error (Unix.ECONNRESET, _, _) when Sys.win32 ->
            (* Closing a Windows Job can reset a loopback connection instead of
               returning a graceful zero-byte read. This is only a candidate
               peer closure here: [prove_process_tree_stopped] accepts it after
               every authenticated PID is proved dead. *)
            saw_windows_reset := true;
            close_terminal descriptor
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
            ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> ())
      readable
  done;
  if !saw_windows_reset then Windows_connection_reset else Graceful_eof

let wait_for_witnesses_to_exit witnesses =
  let deadline = Deadline.start policy.cleanup_deadline in
  let rec wait () =
    let alive =
      List.filter
        (fun (_, witness) -> Engine.Process_supervisor.witness_is_alive witness)
        witnesses
    in
    match alive with
    | [] -> ()
    | _ when Deadline.expired deadline ->
        fail "owned descendants remained alive: %s"
          (String.concat ","
             (List.map
                (fun (role, witness) ->
                  Printf.sprintf "%s=%d" role
                    (Engine.Process_supervisor.witness_pid witness))
                alive))
    | _ ->
        let pause =
          min
            (Deadline.remaining deadline)
            (Positive_seconds.to_float policy.observation_interval)
        in
        ignore (Unix.select [] [] [] pause);
        wait ()
  in
  wait ()

let prove_process_tree_stopped connections witnesses =
  let disconnect = wait_for_connection_shutdown connections in
  Fun.protect
    (fun () -> wait_for_witnesses_to_exit witnesses)
    ~finally:(fun () ->
      List.iter
        (fun (_, witness) ->
          Engine.Process_supervisor.close_liveness_witness witness)
        witnesses);
  (* In particular, [Windows_connection_reset] is not accepted as proof on its
     own. Exact witnessed liveness above, backed by both native Job owners, is
     the proof that the reset represented terminal peer shutdown. *)
  match disconnect with
  | Graceful_eof | Windows_connection_reset -> ()

let expect_int label expected = function
  | `Int actual when actual = expected -> ()
  | actual ->
      fail "%s was %s (expected %d)" label
        (Yojson.Safe.to_string actual)
        expected

let expect_string label expected = function
  | `String actual when String.equal actual expected -> ()
  | actual ->
      fail "%s was %s (expected %S)" label
        (Yojson.Safe.to_string actual)
        expected

let contains ~needle value =
  let rec search index =
    if index + String.length needle > String.length value then false
    else if String.equal (String.sub value index (String.length needle)) needle
    then true
    else search (index + 1)
  in
  search 0

let check_report layout =
  let store =
    match
      Engine.Run_store.create ~workspace:layout.workspace
        ~directory:layout.cache ()
    with
    | Ok store -> store
    | Error error ->
        fail "cannot reopen native report store: %s"
          (Format.asprintf "%a" Engine.Error.pp error)
  in
  let run =
    match Engine.Run_store.load_run store "latest" with
    | Ok run -> run
    | Error error ->
        fail "cannot load interrupted native report: %s"
          (Format.asprintf "%a" Engine.Error.pp error)
  in
  let encoded = Engine.Run_store.run_to_yojson run in
  let decoded =
    get_ok "interrupted report did not pass the native lossless decoder"
      (Engine.Run_store.run_of_json encoded)
  in
  let reencoded = Engine.Run_store.run_to_yojson decoded in
  if
    not
      (String.equal
         (Yojson.Safe.to_string encoded)
         (Yojson.Safe.to_string reencoded))
  then fail "interrupted report failed decode(encode) canonical round-trip";
  (match decoded.Engine.Run_store.status with
  | Engine.Run_store.Interrupted -> ()
  | status ->
      fail "native report status was %s, expected interrupted"
        (Engine.Run_store.status_name status));
  let result =
    match decoded.results with
    | [ result ] -> result
    | results ->
        fail
          "cancellation propagated into %d result records, expected exactly one"
          (List.length results)
  in
  (match result.Engine.Run_store.outcome with
  | Core.Outcome.Error "interrupted" -> ()
  | outcome ->
      fail "executing mutant recorded %s instead of one interrupted error"
        (Core.Outcome.name outcome));
  (match result.stages with
  | [ stage ] when String.equal stage.status "cancelled" -> ()
  | stages ->
      fail "cancellation produced %d stage records without one cancelled stage"
        (List.length stages));
  if
    not
      (contains
         ~needle:("interrupt-stage-stdout:" ^ layout.nonce)
         result.stdout.contents)
  then fail "cancelled result lost the supervised stdout evidence";
  if
    not
      (contains
         ~needle:("interrupt-stage-stderr:" ^ layout.nonce)
         result.stderr.contents)
  then fail "cancelled result lost the supervised stderr evidence";
  let not_run = Engine.Run_store.not_run decoded in
  if not_run = [] then fail "interrupted native report was not partial";
  let open Yojson.Safe.Util in
  expect_string "report document type" "ocaml-mutants.run-report-v1"
    (encoded |> member "document_type");
  expect_string "report status" "interrupted" (encoded |> member "status");
  expect_string "report summary kind" "partial"
    (encoded |> member "summary" |> member "kind");
  expect_int "report executed count" 1
    (encoded |> member "summary" |> member "executed");
  expect_int "report error count" 1
    (encoded |> member "summary" |> member "error");
  (match encoded |> member "failure" with
  | `Null -> ()
  | failure ->
      fail "interrupted report unexpectedly encoded a failure: %s"
        (Yojson.Safe.to_string failure));
  let root_marker = Filename.concat layout.cache ".ocaml-mutants-cache-v2" in
  let marker =
    get_ok "cannot read cache ownership marker"
      (Engine.Util.read_file root_marker)
  in
  if not (String.equal marker "owner=ocaml-mutants\nschema=2\n") then
    fail "cache ownership marker changed";
  let files = Engine.Util.files_recursive layout.cache in
  if
    List.exists
      (fun path ->
        Filename.check_suffix path ".pending"
        || Filename.check_suffix path ".reserved")
      files
  then fail "interrupted report left pending or reservation cache artifacts";
  if not (List.exists (fun path -> Filename.check_suffix path ".json") files)
  then fail "interrupted report was not published as an authoritative JSON file"

let recover_owned_snapshots_after_forced_shutdown layout =
  match
    Engine.Workspace_snapshot.with_snapshot layout.workspace (fun _ -> Ok ())
  with
  | Ok () -> ()
  | Error error ->
      fail "could not recover a snapshot after forced CLI shutdown: %s"
        (Format.asprintf "%a" Engine.Error.pp error)

let run_contract cli executable =
  let layout = create_layout () in
  Fun.protect
    ~finally:(fun () -> cleanup_layout layout)
    (fun () ->
      let listener, port = create_listener () in
      let connections = ref [] in
      Fun.protect
        ~finally:(fun () ->
          List.iter
            (fun descriptor ->
              try Unix.close descriptor with Unix.Unix_error _ -> ())
            !connections;
          try Unix.close listener with Unix.Unix_error _ -> ())
        (fun () ->
          write_fixture layout executable port;
          let before = source_digest layout in
          let arguments =
            [| cli; "run"; layout.workspace; "--quiet"; "--no-color" |]
          in
          let cli_stdout = Filename.concat layout.control "cli.stdout" in
          let cli_stderr = Filename.concat layout.control "cli.stderr" in
          try
            Native_launcher.with_process ~stdout_path:cli_stdout
              ~stderr_path:cli_stderr arguments (fun owned ->
                let readiness =
                  await_readiness layout listener connections owned
                in
                let snapshots =
                  List.map (fun ready -> ready.snapshot) readiness
                  |> List.sort_uniq String.compare
                in
                let snapshot =
                  match snapshots with
                  | [ snapshot ] -> validate_snapshot_path snapshot
                  | _ ->
                      fail "process tree reported %d distinct snapshot roots"
                        (List.length snapshots)
                in
                (* Witnesses must be captured while the tree is provably alive
                   (its readiness sockets are still connected), so that the
                   later death proof answers for these exact processes rather
                   than whatever a recycled PID names by then. *)
                let witnesses =
                  List.map
                    (fun ready ->
                      ( Printf.sprintf "%s(job=%s)" ready.role ready.in_job,
                        Engine.Process_supervisor.open_liveness_witness
                          ready.pid ))
                    readiness
                in
                Native_launcher.send_interrupt owned;
                (match Native_launcher.wait owned policy.shutdown_deadline with
                | Native_launcher.Exited 130 -> ()
                | Native_launcher.Exited code
                  when Sys.win32 && code = windows_status_control_c_exit ->
                    fail
                      "targeted CTRL_BREAK reached the CLI but bypassed the \
                       one-shot cancellation bridge (STATUS_CONTROL_C_EXIT)"
                | Native_launcher.Exited code ->
                    fail "interrupted CLI exited %d instead of 130" code
                | Native_launcher.Signaled signal ->
                    fail
                      "interrupted CLI died from signal %d instead of exiting \
                       130"
                      signal
                | Native_launcher.Timed_out ->
                    fail "interrupted CLI exceeded its shutdown deadline"
                | Native_launcher.Wait_failed code ->
                    fail "interrupted CLI wait failed with native error %d" code);
                (* The CLI's own death was already proved exactly by
                   [Native_launcher.wait] on its retained process handle;
                   re-polling its PID could only report a recycled PID as a
                   survivor. *)
                prove_process_tree_stopped connections witnesses;
                if Sys.file_exists snapshot then
                  fail "owned workspace snapshot remained after interrupt: %s"
                    snapshot;
                check_report layout;
                let after = source_digest layout in
                if not (String.equal before after) then
                  fail "real OS interrupt changed the source workspace")
          with exception_ ->
            let backtrace = Printexc.get_raw_backtrace () in
            recover_owned_snapshots_after_forced_shutdown layout;
            Printexc.raise_with_backtrace exception_ backtrace))

let () =
  if Array.length Sys.argv >= 2 && String.equal Sys.argv.(1) stage_flag then
    run_stage ()
  else if Array.length Sys.argv >= 2 && String.equal Sys.argv.(1) child_flag
  then run_child ()
  else if
    Array.length Sys.argv >= 2 && String.equal Sys.argv.(1) grandchild_flag
  then run_grandchild ()
  else
    try
      if Array.length Sys.argv <> 2 then
        fail "expected the ocaml-mutants CLI path as the only argument";
      run_contract
        (Unix.realpath Sys.argv.(1))
        (Unix.realpath Sys.executable_name)
    with
    | Unsupported_environment proof ->
        Printf.eprintf
          "SKIP cli interrupt contract: unsupported environment proved by %s\n\
           %!"
          proof
    | Contract_failure message ->
        prerr_endline message;
        exit 1
