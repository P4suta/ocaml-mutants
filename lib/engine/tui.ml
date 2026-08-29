module Core = Ocaml_mutants_core

external windows_console_set_raw : Unix.file_descr -> int * int64
  = "ocaml_mutants_windows_console_set_raw"

external windows_console_restore : Unix.file_descr -> int64 -> int
  = "ocaml_mutants_windows_console_restore"

external windows_console_prepare_output : Unix.file_descr -> int * int64 * int
  = "ocaml_mutants_windows_console_prepare_output"

external windows_console_restore_output : Unix.file_descr -> int64 -> int -> int
  = "ocaml_mutants_windows_console_restore_output"

external windows_console_size : Unix.file_descr -> int * int * int
  = "ocaml_mutants_windows_console_size"

external windows_console_flush_input : Unix.file_descr -> int
  = "ocaml_mutants_windows_console_flush_input"

let backend_diagnostic () =
  let input_is_tty = Matrix.Terminal.is_tty Unix.stdin in
  let output_is_tty = Matrix.Terminal.is_tty Unix.stdout in
  let monochrome = Sys.getenv_opt "NO_COLOR" <> None in
  let result =
    if not (input_is_tty && output_is_tty) then
      Ok "Matrix 0.1.0 (not attached: stdin/stdout are not both TTYs)"
    else if Sys.win32 then
      let input_error, input_mode = windows_console_set_raw Unix.stdin in
      if input_error <> 0 then
        Error
          (Printf.sprintf "Windows Console input probe failed with error %d"
             input_error)
      else
        let output_error, output_mode, output_code_page =
          windows_console_prepare_output Unix.stdout
        in
        let output_restore_error =
          if output_error = 0 then
            windows_console_restore_output Unix.stdout output_mode
              output_code_page
          else 0
        in
        let input_restore_error =
          windows_console_restore Unix.stdin input_mode
        in
        if output_error <> 0 then
          Error
            (Printf.sprintf "Windows Console output probe failed with error %d"
               output_error)
        else if output_restore_error <> 0 || input_restore_error <> 0 then
          Error
            (Printf.sprintf
               "Windows Console restoration probe failed (input=%d output=%d)"
               input_restore_error output_restore_error)
        else
          let size_error, width, height = windows_console_size Unix.stdout in
          if size_error <> 0 then
            Error
              (Printf.sprintf "Windows ConPTY size probe failed with error %d"
                 size_error)
          else
            Ok
              (Printf.sprintf "Matrix 0.1.0 / Windows ConPTY (%dx%d)" width
                 height)
    else
      try
        ignore (Unix.tcgetattr Unix.stdin);
        let width, height = Matrix.Terminal.size Unix.stdout in
        Ok (Printf.sprintf "Matrix 0.1.0 / POSIX PTY (%dx%d)" width height)
      with
      | Unix.Unix_error (error, operation, argument) ->
          Error
            (Printf.sprintf "%s(%s): %s" operation argument
               (Unix.error_message error))
      | Invalid_argument message -> Error message
  in
  Result.map
    (fun detail ->
      if monochrome then detail ^ " / NO_COLOR monochrome" else detail)
    result

type filter = Actionable | All | Killed

type live_status =
  | Idle
  | Running of Cancel.t
  | Cancelling of Cancel.t
  | Finished of int

type live_result = { result : Run_store.mutant_result; coverage : string }

type model = {
  run : Run_store.run option;
  history : Run_store.run array;
  history_index : int;
  selected : int;
  filter : filter;
  width : int;
  height : int;
  color : bool;
  interactive : bool;
  live_status : live_status;
  live_progress : Event_bus.progress option;
  live_phase : string option;
  live_run_id : string option;
  live_settled : int;
  live_last_settled : (string * string) option;
  live_results : live_result list;
  live_warnings : (string * string) list;
  live_error : string option;
}

type msg =
  | Move of int
  | Previous_run
  | Next_run
  | Cycle_filter
  | Resize of int * int
  | Start_live
  | Cancel_live
  | Live_event of Event_bus.event
  | Live_finished of {
      exit_code : int;
      runs : Run_store.run list option;
      error : string option;
    }
  | Quit

let init_history ?(width = 100) ?(height = 30) ?(color = true) runs =
  let history = Array.of_list runs in
  let run = if Array.length history = 0 then None else Some history.(0) in
  {
    run;
    history;
    history_index = 0;
    selected = 0;
    filter = Actionable;
    width;
    height;
    color;
    interactive = false;
    live_status = Idle;
    live_progress = None;
    live_phase = None;
    live_run_id = None;
    live_settled = 0;
    live_last_settled = None;
    live_results = [];
    live_warnings = [];
    live_error = None;
  }

let init ?width ?height ?color run =
  init_history ?width ?height ?color (Option.to_list run)

let actionable (result : Run_store.mutant_result) =
  match result.outcome with
  | Core.Outcome.Killed -> false
  | Core.Outcome.Survived | Core.Outcome.Timeout | Core.Outcome.Inconclusive _
  | Core.Outcome.Error _ ->
      true

let showing_live model =
  match model.live_status with
  | Running _ | Cancelling _ -> true
  | Idle | Finished _ -> false

let displayed_results model =
  if showing_live model then model.live_results
  else
    match model.run with
    | None -> []
    | Some run ->
        List.map
          (fun result ->
            { result; coverage = Run_store.result_coverage run result })
          run.Run_store.results

let visible_displayed_results model =
  List.filter
    (fun displayed ->
      match model.filter with
      | Actionable -> actionable displayed.result
      | All -> true
      | Killed -> displayed.result.Run_store.outcome = Core.Outcome.Killed)
    (displayed_results model)

let visible_results model =
  List.map (fun displayed -> displayed.result) (visible_displayed_results model)

let clamp_selection model =
  let length = List.length (visible_displayed_results model) in
  let selected =
    if length = 0 then 0 else max 0 (min (length - 1) model.selected)
  in
  { model with selected }

let take count values =
  let rec loop count taken = function
    | _ when count <= 0 -> List.rev taken
    | [] -> List.rev taken
    | value :: rest -> loop (count - 1) (value :: taken) rest
  in
  loop count [] values

let run_id (run : Run_store.run) =
  Core.Run_id.to_string run.Run_store.metadata.id

let select_refreshed_run model runs =
  let history = Array.of_list runs in
  let rec find index =
    if index >= Array.length history then None
    else
      match model.live_run_id with
      | Some id when String.equal id (run_id history.(index)) -> Some index
      | Some _ -> find (index + 1)
      | None -> None
  in
  let history_index = Option.value ~default:0 (find 0) in
  let run =
    if Array.length history = 0 then None else Some history.(history_index)
  in
  clamp_selection { model with run; history; history_index; selected = 0 }

let finish_phase exit_code =
  match exit_code with 0 -> "finished" | 130 -> "cancelled" | _ -> "failed"

let update_live_event event model =
  match event with
  | Event_bus.Run_started { run_id } ->
      {
        model with
        live_run_id = Some run_id;
        live_phase = Some "starting";
        live_error = None;
      }
  | Event_bus.Phase_started { phase; total = _ } ->
      { model with live_phase = Some phase }
  | Event_bus.Progress progress ->
      {
        model with
        live_progress = Some progress;
        live_phase = Some progress.phase;
      }
  | Event_bus.Mutant_settled { result; coverage } ->
      let mutant_id = Core.Mutant.Id.full (Core.Mutant.id result.mutant) in
      let outcome =
        match result.outcome with
        | Core.Outcome.Killed -> "killed"
        | Core.Outcome.Survived -> "survived"
        | Core.Outcome.Timeout ->
            if result.timeout_confirmed then "timeout"
            else "timeout-unconfirmed"
        | Core.Outcome.Inconclusive _ -> "inconclusive"
        | Core.Outcome.Error _ -> "error"
      in
      {
        model with
        live_settled = model.live_settled + 1;
        live_last_settled = Some (mutant_id, outcome);
        live_results = { result; coverage } :: model.live_results;
      }
  | Event_bus.Warning { code; message } ->
      let warning = (code, message) in
      {
        model with
        live_warnings =
          (if List.mem warning model.live_warnings then model.live_warnings
           else take 5 (warning :: model.live_warnings));
      }
  | Event_bus.Run_finished { exit_code } ->
      { model with live_phase = Some (finish_phase exit_code) }

let update_model message model =
  match message with
  | Move delta ->
      clamp_selection { model with selected = model.selected + delta }
  | Cycle_filter ->
      let filter =
        match model.filter with
        | Actionable -> All
        | All -> Killed
        | Killed -> Actionable
      in
      clamp_selection { model with filter; selected = 0 }
  | Previous_run | Next_run ->
      let delta =
        match message with Previous_run -> 1 | Next_run -> -1 | _ -> 0
      in
      let history_index =
        if Array.length model.history = 0 then 0
        else
          max 0
            (min (Array.length model.history - 1) (model.history_index + delta))
      in
      let run =
        if Array.length model.history = 0 then None
        else Some model.history.(history_index)
      in
      clamp_selection { model with run; history_index; selected = 0 }
  | Resize (width, height) ->
      { model with width = max 1 width; height = max 1 height }
  | Live_event event -> update_live_event event model
  | Live_finished { exit_code; runs; error } ->
      let model =
        {
          model with
          live_status = Finished exit_code;
          live_phase = Some (finish_phase exit_code);
          live_error = error;
        }
      in
      Option.fold ~none:model ~some:(select_refreshed_run model) runs
  | Start_live | Cancel_live -> model
  | Quit -> model

let update message model =
  match message with
  | Quit | Cancel_live -> (model, Mosaic.Cmd.quit)
  | Move _ | Previous_run | Next_run | Cycle_filter | Resize _ | Start_live
  | Live_event _ | Live_finished _ ->
      (update_model message model, Mosaic.Cmd.none)

let filter_name = function
  | Actionable -> "actionable"
  | All -> "all"
  | Killed -> "killed"

let sanitize value =
  String.map
    (fun character ->
      match character with
      | '\n' | '\r' | '\t' -> character
      | '\000' .. '\008' | '\011' | '\012' | '\014' .. '\031' | '\127' -> ' '
      | character -> character)
    value

let outcome_name (result : Run_store.mutant_result) =
  match result.outcome with
  | Core.Outcome.Killed -> "killed"
  | Core.Outcome.Survived -> "SURVIVED"
  | Core.Outcome.Timeout ->
      if result.timeout_confirmed then "timeout" else "INCONCLUSIVE TIMEOUT"
  | Core.Outcome.Inconclusive _ -> "INCONCLUSIVE"
  | Core.Outcome.Error _ -> "ERROR"

let nth_opt values index =
  let rec loop current = function
    | [] -> None
    | value :: _ when current = index -> Some value
    | _ :: rest -> loop (current + 1) rest
  in
  loop 0 values

let selected_displayed_result model =
  nth_opt (visible_displayed_results model) model.selected

let result_line selected index displayed =
  let result = displayed.result in
  let mutant = result.mutant in
  Printf.sprintf "%s %-20s %-18s %s:%d"
    (if selected = index then ">" else " ")
    (outcome_name result)
    (Core.Mutant.Id.short (Core.Mutant.id mutant))
    (Core.Mutant.path mutant)
    (Core.Source_range.start_line (Core.Mutant.range mutant))

let result_list model =
  let results = visible_displayed_results model in
  let rows = max 3 (model.height / 2) in
  let first = max 0 (model.selected - (rows / 2)) in
  results
  |> List.mapi (result_line model.selected)
  |> List.filteri (fun index _ -> index >= first)
  |> take rows |> String.concat "\n"

let stage_detail (result : Run_store.mutant_result) =
  match List.rev result.stages with
  | [] -> "killing/last test: unavailable"
  | stage :: _ ->
      Printf.sprintf "killing/last test: %s (%s)" stage.name stage.status

let tests_detail (result : Run_store.mutant_result) =
  match result.stages with
  | [] -> "tests: unavailable"
  | stages ->
      "tests: "
      ^ String.concat ", "
          (List.map
             (fun (stage : Run_store.stage_result) ->
               Printf.sprintf "%s [%s]" stage.name stage.status)
             stages)

let expectation_detail (result : Run_store.mutant_result) =
  match result.expected_reason with
  | None -> "expectation: none"
  | Some reason ->
      Printf.sprintf "expectation: %s — %s"
        (Run_store.expectation_status_of_result result
        |> Run_store.expectation_status_name)
        (sanitize reason)

let result_detail model =
  match selected_displayed_result model with
  | None -> "No mutants match this filter. Press Tab to change filters."
  | Some displayed ->
      let result = displayed.result in
      let mutant = result.Run_store.mutant in
      let evidence =
        Run_store.result_evidence_level result |> Run_store.evidence_level_name
      in
      String.concat "\n"
        [
          Printf.sprintf "%s  %s" (outcome_name result)
            (Core.Mutant.Id.full (Core.Mutant.id mutant));
          Printf.sprintf "lineage: %s" (Core.Mutant.lineage_id mutant);
          Format.asprintf "%s:%a  %s" (Core.Mutant.path mutant)
            Core.Source_range.pp (Core.Mutant.range mutant)
            (Core.Operator.Rule.stable_name (Core.Mutant.rule mutant));
          Printf.sprintf "evidence: %s (%s)%s" evidence
            (Run_store.evidence_origin_name result.evidence_origin)
            (if result.cached then " (resumed/cache)" else "");
          Printf.sprintf "coverage: %s" displayed.coverage;
          expectation_detail result;
          stage_detail result;
          tests_detail result;
          "";
          "- " ^ sanitize (Core.Mutant.original mutant);
          "+ " ^ sanitize (Core.Mutant.replacement mutant);
          "";
          "stdout:";
          sanitize result.stdout.contents;
          "stderr:";
          sanitize result.stderr.contents;
        ]

let summary_line run =
  let summary = Run_store.summary run in
  Printf.sprintf
    "Run %s  %s  %d/%d executed  %d killed  %d unexpected survivor  evidence %s"
    (Core.Run_id.to_string run.Run_store.metadata.id)
    (Run_store.status_name run.status)
    summary.executed summary.total summary.killed summary.unexpected_survivors
    (Run_store.run_evidence_level run |> Run_store.evidence_level_name)

let duration value =
  if value >= 3600. then Printf.sprintf "%.1fh" (value /. 3600.)
  else if value >= 60. then Printf.sprintf "%.1fm" (value /. 60.)
  else Printf.sprintf "%.1fs" value

let live_summary model =
  let status =
    match model.live_status with
    | Idle -> "ready"
    | Running _ -> "running"
    | Cancelling _ -> "cancelling"
    | Finished exit_code -> Printf.sprintf "finished (exit %d)" exit_code
  in
  let run_id = Option.value ~default:"pending" model.live_run_id in
  match model.live_progress with
  | Some progress ->
      Printf.sprintf
        "Live %s  %s  %s  %d/%d  workers %d  cache %d  resume %d  elapsed %s%s"
        run_id status progress.phase progress.completed progress.total
        progress.workers progress.cache_hits progress.resume_hits
        (duration progress.elapsed_seconds)
        (match progress.eta_seconds with
        | None -> ""
        | Some eta -> "  ETA " ^ duration eta)
  | None ->
      Printf.sprintf "Live %s  %s  phase %s  %d settled" run_id status
        (Option.value ~default:"waiting" model.live_phase)
        model.live_settled

let live_notice model =
  let error =
    Option.map (fun message -> "error: " ^ sanitize message) model.live_error
  in
  let warning =
    match model.live_warnings with
    | [] -> None
    | (code, message) :: _ ->
        Some (Printf.sprintf "warning[%s]: %s" code (sanitize message))
  in
  let settled =
    match model.live_last_settled with
    | None -> None
    | Some (id, outcome) ->
        Some
          (Printf.sprintf "last settled: %s %s" outcome
             (if String.length id <= 18 then id else String.sub id 0 18))
  in
  List.filter_map Fun.id [ error; warning; settled ] |> String.concat "  "

let view model =
  let open Mosaic in
  let stored_header =
    match model.run with
    | None -> "ocaml-mutants 1.0 — no stored runs"
    | Some run ->
        Printf.sprintf "%s  history %d/%d" (summary_line run)
          (model.history_index + 1)
          (Array.length model.history)
  in
  let header =
    match model.live_status with
    | Running _ | Cancelling _ -> live_summary model
    | Idle | Finished _ -> stored_header
  in
  let footer =
    let action =
      if not model.interactive then "q/Esc quit"
      else
        match model.live_status with
        | Running _ | Cancelling _ -> "q/Esc/Ctrl-C cancel"
        | Idle | Finished _ -> "r run  q/Esc/Ctrl-C quit"
    in
    Printf.sprintf
      "←/h older  →/l newer  ↑/k ↓/j move  Tab filter (%s)  %s  %dx%d"
      (filter_name model.filter) action model.width model.height
  in
  let panel ~title ~wrap contents =
    if model.color then
      box ~border:true ~title ~padding:(padding 1) ~flex_grow:1.
        [ text ~wrap contents ]
    else
      box ~padding:(padding 1) ~flex_direction:Flex_direction.Column
        ~flex_grow:1.
        [ text (title ^ ":"); text ~wrap contents ]
  in
  let list_panel = panel ~title:"Results" ~wrap:`None (result_list model) in
  let detail_panel =
    panel ~title:"Evidence and diff" ~wrap:`Word (result_detail model)
  in
  let body =
    if model.width < 80 then
      box ~flex_direction:Flex_direction.Column ~flex_grow:1.
        [ list_panel; detail_panel ]
    else
      box ~flex_direction:Flex_direction.Row ~flex_grow:1.
        [ list_panel; detail_panel ]
  in
  box ~flex_direction:Flex_direction.Column
    ~size:(size_wh (pct 100) (pct 100))
    ~padding:(padding 1)
    (let notice = live_notice model in
     [ text header ]
     @ (if String.equal notice "" then [] else [ text notice ])
     @ [ body; text footer ])

let key_message event =
  let event = Mosaic.Event.Key.data event in
  match (event.Matrix.Input.Key.event_type, event.key) with
  | Matrix.Input.Key.Release, _ -> None
  | _, Matrix.Input.Key.Char character
    when Uchar.to_int character = 0x03
         || (event.modifier.ctrl && Uchar.to_int character = 0x63) ->
      Some Cancel_live
  | _, (Matrix.Input.Key.Up | Matrix.Input.Key.KP_up) -> Some (Move (-1))
  | _, (Matrix.Input.Key.Down | Matrix.Input.Key.KP_down) -> Some (Move 1)
  | _, Matrix.Input.Key.Page_up -> Some (Move (-10))
  | _, Matrix.Input.Key.Page_down -> Some (Move 10)
  | _, (Matrix.Input.Key.Left | Matrix.Input.Key.KP_left) -> Some Previous_run
  | _, (Matrix.Input.Key.Right | Matrix.Input.Key.KP_right) -> Some Next_run
  | _, Matrix.Input.Key.Tab -> Some Cycle_filter
  | _, Matrix.Input.Key.Escape -> Some Quit
  | _, Matrix.Input.Key.Char character -> (
      match Uchar.to_int character with
      | 0x6a -> Some (Move 1)
      | 0x6b -> Some (Move (-1))
      | 0x68 -> Some Previous_run
      | 0x6c -> Some Next_run
      | 0x72 -> Some Start_live
      | 0x71 -> Some Quit
      | _ -> None)
  | _ -> None

let interactive_error operation exception_ =
  Error.create ~phase:Error.Cli ~cause:Error.Invariant_violation
    ~context:
      [
        ("operation", operation);
        ("exception", Printexc.to_string exception_);
        ("backtrace", Printexc.get_backtrace ());
      ]
    "interactive terminal operation raised unexpectedly"

let error_string error = Format.asprintf "%a" Error.pp error

let live_command ~cancel ~reload ~start =
  Mosaic.Cmd.perform (fun dispatch ->
      let emit event = dispatch (Live_event event) in
      let result =
        try start ~cancel ~emit
        with exception_ -> Error (interactive_error "start-run" exception_)
      in
      let exit_code, run_error =
        match result with
        | Ok exit_code -> (exit_code, None)
        | Error error ->
            let exit_code = Error.exit_code error in
            let error =
              if exit_code = 130 then None else Some (error_string error)
            in
            (exit_code, error)
      in
      emit (Event_bus.Run_finished { exit_code });
      let runs, reload_error =
        match
          try reload ()
          with exception_ ->
            Error (interactive_error "reload-runs" exception_)
        with
        | Ok (runs, warnings) ->
            List.iter
              (fun (code, message) ->
                emit (Event_bus.Warning { code; message }))
              warnings;
            (Some runs, None)
        | Error error -> (None, Some (error_string error))
      in
      let error =
        match (run_error, reload_error) with
        | None, None -> None
        | Some error, None | None, Some error -> Some error
        | Some first, Some second -> Some (first ^ "\n" ^ second)
      in
      dispatch (Live_finished { exit_code; runs; error }))

let update_interactive ~reload ~start message model =
  match message with
  | Start_live -> (
      match model.live_status with
      | Running _ | Cancelling _ -> (model, Mosaic.Cmd.none)
      | Idle | Finished _ ->
          let cancel = Cancel.create () in
          let model =
            {
              model with
              live_status = Running cancel;
              live_progress = None;
              live_phase = Some "starting";
              live_run_id = None;
              live_settled = 0;
              live_last_settled = None;
              live_results = [];
              live_warnings = [];
              live_error = None;
            }
          in
          (model, live_command ~cancel ~reload ~start))
  | Quit | Cancel_live -> (
      match model.live_status with
      | Running cancel ->
          Cancel.request cancel;
          ( {
              model with
              live_status = Cancelling cancel;
              live_phase = Some "cancelling";
            },
            Mosaic.Cmd.none )
      | Cancelling _ -> (model, Mosaic.Cmd.none)
      | Idle | Finished _ -> (model, Mosaic.Cmd.quit))
  | Move _ | Previous_run | Next_run | Cycle_filter | Resize _ | Live_event _
  | Live_finished _ ->
      (update_model message model, Mosaic.Cmd.none)

let write_all file bytes offset length =
  let rec loop offset remaining =
    if remaining > 0 then
      match Unix.single_write file bytes offset remaining with
      | 0 -> raise End_of_file
      | written -> loop (offset + written) (remaining - written)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop offset remaining
  in
  loop offset length

type output_filter_state = Plain | Escape of Buffer.t | Csi of Buffer.t * bool

let monochrome_output_to write_sink =
  let state = ref Plain in
  let emit buffer =
    let value = Buffer.to_bytes buffer in
    write_sink value 0 (Bytes.length value)
  in
  let reset () = state := Plain in
  let flush_pending () =
    match !state with
    | Plain -> ()
    | Escape buffer | Csi (buffer, _) ->
        emit buffer;
        reset ()
  in
  let write bytes offset length =
    let finish = offset + length in
    for index = offset to finish - 1 do
      let character = Bytes.get bytes index in
      match !state with
      | Plain ->
          if character = '\027' then (
            let buffer = Buffer.create 24 in
            Buffer.add_char buffer character;
            state := Escape buffer)
          else write_sink bytes index 1
      | Escape buffer ->
          Buffer.add_char buffer character;
          if character = '[' then state := Csi (buffer, true)
          else (
            emit buffer;
            reset ())
      | Csi (buffer, standard_sgr) ->
          Buffer.add_char buffer character;
          let code = Char.code character in
          let is_parameter =
            (character >= '0' && character <= '9')
            || character = ';' || character = ':'
          in
          if code >= 0x40 && code <= 0x7e then (
            if not (standard_sgr && character = 'm') then emit buffer;
            reset ())
          else if is_parameter then ()
          else state := Csi (buffer, false);
          if Buffer.length buffer > 128 then (
            emit buffer;
            reset ())
    done
  in
  (write, flush_pending)

let monochrome_output output =
  monochrome_output_to (fun bytes offset length ->
      write_all output bytes offset length)

module For_testing = struct
  let monochrome chunks =
    let output = Buffer.create 128 in
    let write bytes offset length =
      Buffer.add_subbytes output bytes offset length
    in
    let filter, flush = monochrome_output_to write in
    List.iter
      (fun chunk ->
        let bytes = Bytes.unsafe_of_string chunk in
        filter bytes 0 (Bytes.length bytes))
      chunks;
    flush ();
    Buffer.contents output
end

let terminal_output ~color output =
  if color then
    ( (fun bytes offset length -> write_all output bytes offset length),
      fun () -> () )
  else monochrome_output output

let windows_matrix ~color ~exit_on_ctrl_c =
  let input = Unix.stdin and output = Unix.stdout in
  let input_is_tty = Matrix.Terminal.is_tty input in
  let output_is_tty = Matrix.Terminal.is_tty output in
  let win32_failure operation error =
    failwith (Printf.sprintf "%s failed with Win32 error %d" operation error)
  in
  let output_error, output_mode, output_code_page =
    windows_console_prepare_output output
  in
  if output_is_tty && output_error <> 0 then
    win32_failure "SetConsoleMode(output)" output_error;
  let output_restored = Atomic.make false in
  let restore_output () =
    if output_is_tty && Atomic.compare_and_set output_restored false true then
      ignore
        (windows_console_restore_output output output_mode output_code_page)
  in
  let terminal_size () =
    let error, width, height = windows_console_size output in
    if error = 0 then (max 1 width, max 1 height) else (80, 24)
  in
  let output_bytes, flush_output = terminal_output ~color output in
  let output_string value =
    let bytes = Bytes.unsafe_of_string value in
    output_bytes bytes 0 (Bytes.length bytes)
  in
  let terminal =
    Matrix.Terminal.make ~output:output_string ~tty:output_is_tty ()
  in
  let parser = Matrix.Input.Parser.create () in
  let original_mode = ref None in
  let set_raw_mode enabled =
    if enabled then
      match !original_mode with
      | Some _ -> ()
      | None -> (
          if input_is_tty then
            let error, mode = windows_console_set_raw input in
            if error <> 0 then win32_failure "SetConsoleMode(input)" error
            else original_mode := Some mode
          else
            match !original_mode with
            | None -> ()
            | Some mode ->
                let error = windows_console_restore input mode in
                if error <> 0 then
                  win32_failure "RestoreConsoleMode(input)" error
                else original_mode := None)
  in
  let queue = Queue.create () in
  let queue_mutex = Mutex.create () in
  let closed = Atomic.make false in
  let clear_input () =
    Mutex.protect queue_mutex (fun () -> Queue.clear queue);
    if input_is_tty then ignore (windows_console_flush_input input)
  in
  let last_size = ref (terminal_size ()) in
  let read_events ~timeout ~on_event =
    let has_input =
      Mutex.protect queue_mutex (fun () -> not (Queue.is_empty queue))
    in
    if not has_input then
      Thread.delay (max 0.001 (min 0.05 (Option.value ~default:0.05 timeout)));
    let chunks =
      Mutex.protect queue_mutex (fun () ->
          let rec drain values =
            if Queue.is_empty queue then List.rev values
            else drain (Queue.take queue :: values)
          in
          drain [])
    in
    let on_caps event = Matrix.Terminal.apply_capability_event terminal event in
    List.iter
      (fun bytes ->
        Matrix.Input.Parser.feed parser bytes 0 (Bytes.length bytes)
          ~now:(Unix.gettimeofday ()) ~on_event ~on_caps)
      chunks;
    let size = terminal_size () in
    if size <> !last_size then (
      last_size := size;
      let width, height = size in
      on_event (Matrix.Input.Resize (width, height)));
    Matrix.Input.Parser.drain parser ~now:(Unix.gettimeofday ()) ~on_event
      ~on_caps
  in
  let width, height = !last_size in
  let matrix =
    Matrix.attach ~mode:`Alt ~raw_mode:true ~mouse_enabled:false
      ~bracketed_paste:false ~focus_reporting:true ~kitty_keyboard:`Auto
      ~exit_on_ctrl_c ~cursor_visible:false ~write_output:output_bytes
      ~now:Unix.gettimeofday
      ~wake:(fun () -> ())
      ~terminal_size ~set_raw_mode ~flush_input:clear_input ~read_events
      ~query_cursor_position:(fun ~timeout:_ -> None)
      ~cleanup:(fun () ->
        Atomic.set closed true;
        flush_output ();
        restore_output ())
      ~parser ~terminal ~width ~height ()
  in
  let rec input_loop () =
    if not (Atomic.get closed) then
      let buffer = Bytes.create 4096 in
      match Unix.read input buffer 0 (Bytes.length buffer) with
      | 0 -> Atomic.set closed true
      | length ->
          let bytes = Bytes.sub buffer 0 length in
          Mutex.protect queue_mutex (fun () -> Queue.add bytes queue);
          input_loop ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> input_loop ()
      | exception Unix.Unix_error _ -> Atomic.set closed true
  in
  ignore (Thread.create input_loop ());
  Matrix.Terminal.query_pixel_resolution terminal;
  matrix

let posix_monochrome_matrix ~exit_on_ctrl_c =
  let input = Unix.stdin and output = Unix.stdout in
  let input_is_tty = Matrix.Terminal.is_tty input in
  let output_is_tty = Matrix.Terminal.is_tty output in
  let output_bytes, flush_output = terminal_output ~color:false output in
  let output_string value =
    let bytes = Bytes.unsafe_of_string value in
    output_bytes bytes 0 (Bytes.length bytes)
  in
  let terminal =
    Matrix.Terminal.make ~output:output_string ~tty:output_is_tty ()
  in
  let parser = Matrix.Input.Parser.create () in
  let original_mode = ref None in
  let set_raw_mode enabled =
    if enabled then
      match !original_mode with
      | Some _ -> ()
      | None -> (
          if input_is_tty then
            original_mode := Some (Matrix.Terminal.set_raw input)
          else
            match !original_mode with
            | None -> ()
            | Some mode ->
                Matrix.Terminal.restore input mode;
                original_mode := None)
  in
  let terminal_size () = Matrix.Terminal.size output in
  let last_size = ref (terminal_size ()) in
  let read_events ~timeout ~on_event =
    let timeout = max 0.001 (min 0.05 (Option.value ~default:0.05 timeout)) in
    let readable, _, _ =
      try Unix.select [ input ] [] [] timeout
      with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
    in
    let on_caps event = Matrix.Terminal.apply_capability_event terminal event in
    if readable <> [] then
      let buffer = Bytes.create 4096 in
      match Unix.read input buffer 0 (Bytes.length buffer) with
      | 0 -> ()
      | length ->
          Matrix.Input.Parser.feed parser buffer 0 length
            ~now:(Unix.gettimeofday ()) ~on_event ~on_caps
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EINTR), _, _) ->
          ();
          let size = terminal_size () in
          if size <> !last_size then (
            last_size := size;
            let width, height = size in
            on_event (Matrix.Input.Resize (width, height)));
          Matrix.Input.Parser.drain parser ~now:(Unix.gettimeofday ()) ~on_event
            ~on_caps
  in
  let width, height = !last_size in
  let matrix =
    Matrix.attach ~mode:`Alt ~raw_mode:true ~mouse_enabled:false
      ~bracketed_paste:false ~focus_reporting:true ~kitty_keyboard:`Auto
      ~exit_on_ctrl_c ~cursor_visible:false ~write_output:output_bytes
      ~now:Unix.gettimeofday
      ~wake:(fun () -> ())
      ~terminal_size ~set_raw_mode
      ~flush_input:(fun () -> Matrix.Terminal.flush_input input)
      ~read_events
      ~query_cursor_position:(fun ~timeout:_ -> None)
      ~cleanup:flush_output ~parser ~terminal ~width ~height ()
  in
  Matrix.Terminal.query_pixel_resolution terminal;
  matrix

let matrix ~color ~exit_on_ctrl_c =
  if Sys.win32 then windows_matrix ~color ~exit_on_ctrl_c
  else if not color then posix_monochrome_matrix ~exit_on_ctrl_c
  else
    Matrix.create ~mode:`Alt ~raw_mode:true ~mouse_enabled:false
      ~bracketed_paste:false ~focus_reporting:true ~kitty_keyboard:`Auto
      ~exit_on_ctrl_c ~signal_handlers:true ~cursor_visible:false ()

let subscriptions _ =
  Mosaic.Sub.batch
    [
      Mosaic.Sub.on_key_all key_message;
      Mosaic.Sub.on_resize (fun ~width ~height -> Resize (width, height));
    ]

let run_history ?(color = true) stored_runs =
  let matrix = matrix ~color ~exit_on_ctrl_c:true in
  Fun.protect
    (fun () ->
      Mosaic.run ~matrix
        {
          Mosaic.init =
            (fun () -> (init_history ~color stored_runs, Mosaic.Cmd.none));
          update;
          view;
          subscriptions;
        })
    ~finally:(fun () -> Matrix.close matrix)

let run ?color stored_run = run_history ?color (Option.to_list stored_run)

let run_interactive ?initial_error ?(initial_warnings = []) ?(color = true)
    stored_runs ~reload ~start =
  let matrix = matrix ~color ~exit_on_ctrl_c:false in
  let active_cancel = ref None in
  let workers = ref [] in
  let workers_mutex = Mutex.create () in
  let process_perform action =
    let worker = Thread.create action () in
    Mutex.protect workers_mutex (fun () -> workers := worker :: !workers)
  in
  let update message model =
    let model, command = update_interactive ~reload ~start message model in
    (active_cancel :=
       match model.live_status with
       | Running cancel | Cancelling cancel -> Some cancel
       | Idle | Finished _ -> None);
    (model, command)
  in
  Fun.protect
    (fun () ->
      Mosaic.run ~matrix ~process_perform
        {
          Mosaic.init =
            (fun () ->
              ( {
                  (init_history ~color stored_runs) with
                  interactive = true;
                  live_error = initial_error;
                  live_warnings = initial_warnings;
                },
                Mosaic.Cmd.none ));
          update;
          view;
          subscriptions;
        })
    ~finally:(fun () ->
      Option.iter Cancel.request !active_cancel;
      Mutex.protect workers_mutex (fun () -> !workers) |> List.iter Thread.join;
      Matrix.close matrix)
