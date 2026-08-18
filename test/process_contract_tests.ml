module Engine = Ocaml_mutants_engine

let child_mode = "OCAML_MUTANTS_PROCESS_CONTRACT_CHILD"
let marker_environment = "OCAML_MUTANTS_PROCESS_CONTRACT_MARKER"
let executable () = Unix.realpath Sys.executable_name

let wait_until_terminated pid =
  let observation_window_seconds = 2. in
  let started = Unix.gettimeofday () in
  while
    Engine.Process_supervisor.process_is_alive pid
    && Unix.gettimeofday () -. started < observation_window_seconds
  do
    Unix.sleepf 0.02
  done;
  not (Engine.Process_supervisor.process_is_alive pid)

let with_marker action =
  let marker = Filename.temp_file "ocaml-mutants-process-contract-" ".pid" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove marker with Sys_error _ -> ())
    (fun () ->
      Sys.remove marker;
      action marker)

let descendant_pid marker =
  match Engine.Util.read_file marker with
  | Ok value -> int_of_string (String.trim value)
  | Error message -> Alcotest.fail message

let test_normal_exit_terminates_descendants () =
  with_marker (fun marker ->
      let result =
        Engine.Process_supervisor.run ~timeout:5. ~cwd:(Sys.getcwd ())
          ~env:
            [
              (child_mode, Some "spawn-descendant-and-exit");
              (marker_environment, Some marker);
            ]
          [ executable () ]
      in
      (match result.status with
      | Engine.Process_supervisor.Exited 0 -> ()
      | status ->
          Alcotest.failf "normal-exit tree returned %s"
            (Engine.Process_supervisor.status_string status));
      let pid = descendant_pid marker in
      Alcotest.(check bool)
        "normal completion leaves no descendant" true
        (wait_until_terminated pid))

let test_cancellation_terminates_process () =
  let cancel = Engine.Cancel.create () in
  let requester =
    Thread.create
      (fun () ->
        Unix.sleepf 0.05;
        Engine.Cancel.request cancel)
      ()
  in
  let result =
    Engine.Process_supervisor.run ~cancel ~cwd:(Sys.getcwd ())
      ~env:[ (child_mode, Some "sleep") ]
      [ executable () ]
  in
  Thread.join requester;
  match result.status with
  | Engine.Process_supervisor.Cancelled -> ()
  | status ->
      Alcotest.failf "cancelled child returned %s"
        (Engine.Process_supervisor.status_string status)

let spawn_descendant_and_exit () =
  let marker = Sys.getenv marker_environment in
  Unix.putenv child_mode "sleep";
  let child =
    Unix.create_process (executable ())
      [| executable () |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  let pid = Engine.Process_supervisor.process_id child in
  match Engine.Util.atomic_write marker (string_of_int pid ^ "\n") with
  | Ok () -> ()
  | Error message -> failwith message

let run_tests () =
  Alcotest.run "process contracts"
    [
      ( "ownership",
        [
          Alcotest.test_case "normal exit terminates descendants" `Quick
            test_normal_exit_terminates_descendants;
          Alcotest.test_case "cancellation terminates process" `Quick
            test_cancellation_terminates_process;
        ] );
    ]

let () =
  if Engine.Process_supervisor.helper_requested Sys.argv then
    exit (Engine.Process_supervisor.run_helper Sys.argv)
  else
    match Sys.getenv_opt child_mode with
    | Some "spawn-descendant-and-exit" -> spawn_descendant_and_exit ()
    | Some "sleep" -> Unix.sleepf 60.
    | Some mode -> failwith ("unknown process contract child mode: " ^ mode)
    | None ->
        Engine.Process_supervisor.configure_helper_executable (executable ());
        run_tests ()
