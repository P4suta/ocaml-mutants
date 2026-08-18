module type S = sig
  type token

  val install : int -> (unit -> unit) -> token
  val restore : token -> unit
end

module System = struct
  type subscriber = { active : bool Atomic.t; handler : unit -> unit }
  type token = subscriber
  type initialization = Uninitialized | Ready | Failed of exn

  let sigint_subscribers = Atomic.make []
  let sigterm_subscribers = Atomic.make []
  let initialization_lock = Mutex.create ()
  let sigint_initialization = ref Uninitialized
  let sigterm_initialization = ref Uninitialized
  let windows_worker = ref None
  let windows_failure = ref None

  external windows_install : unit -> int
    = "ocaml_mutants_console_interrupt_install"

  external windows_wait : unit -> int = "ocaml_mutants_console_interrupt_wait"

  external windows_restore : unit -> int
    = "ocaml_mutants_console_interrupt_restore"

  let windows_error operation code =
    Failure
      (Printf.sprintf "Windows console interrupt %s failed with native error %d"
         operation code)

  let dispatch subscribers =
    Atomic.get subscribers
    |> List.iter (fun subscriber ->
        if Atomic.get subscriber.active then
          try subscriber.handler () with _ -> ())

  let rec register subscribers subscriber =
    let current = Atomic.get subscribers in
    let retained =
      List.filter (fun candidate -> Atomic.get candidate.active) current
    in
    if not (Atomic.compare_and_set subscribers current (subscriber :: retained))
    then register subscribers subscriber

  let initialize state action =
    Mutex.lock initialization_lock;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock initialization_lock)
      (fun () ->
        match !state with
        | Ready -> ()
        | Failed exception_ -> raise exception_
        | Uninitialized -> (
            try
              action ();
              state := Ready
            with exception_ ->
              state := Failed exception_;
              raise exception_))

  let start_windows_interrupt_router () =
    let installed = windows_install () in
    if installed <> 0 then raise (windows_error "install" installed);
    let rec pump () =
      match windows_wait () with
      | 1 ->
          dispatch sigint_subscribers;
          pump ()
      | 0 -> ()
      | code ->
          windows_failure := Some (windows_error "wait" (-code));
          (* Losing the process-lifetime bridge fails closed by requesting
             cancellation from every active run before the worker stops. *)
          dispatch sigint_subscribers
    in
    match Thread.create pump () with
    | worker -> windows_worker := Some worker
    | exception exception_ ->
        ignore (windows_restore ());
        raise exception_

  let start_system_signal signal subscribers =
    ignore
      (Sys.signal signal
         (Sys.Signal_handle (fun _signal -> dispatch subscribers)))

  let ensure_router signal =
    if signal = Sys.sigint then
      initialize sigint_initialization (fun () ->
          if Sys.win32 then start_windows_interrupt_router ()
          else start_system_signal signal sigint_subscribers)
    else if signal = Sys.sigterm then
      initialize sigterm_initialization (fun () ->
          start_system_signal signal sigterm_subscribers)
    else invalid_arg "Signal_service.System.install: unsupported signal"

  let subscribers signal =
    if signal = Sys.sigint then sigint_subscribers
    else if signal = Sys.sigterm then sigterm_subscribers
    else invalid_arg "Signal_service.System.install: unsupported signal"

  let install signal handler =
    let subscriber = { active = Atomic.make true; handler } in
    let subscribers = subscribers signal in
    register subscribers subscriber;
    match ensure_router signal with
    | () -> (
        match !windows_failure with
        | Some exception_ when Sys.win32 && signal = Sys.sigint ->
            Atomic.set subscriber.active false;
            raise exception_
        | None | Some _ -> subscriber)
    | exception exception_ ->
        Atomic.set subscriber.active false;
        raise exception_

  let restore subscriber = Atomic.set subscriber.active false
end
