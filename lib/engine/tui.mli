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

val backend_diagnostic : unit -> (string, string) result

val init :
  ?width:int -> ?height:int -> ?color:bool -> Run_store.run option -> model

val init_history :
  ?width:int -> ?height:int -> ?color:bool -> Run_store.run list -> model

val visible_results : model -> Run_store.mutant_result list
val update_model : msg -> model -> model
val update : msg -> model -> model * msg Mosaic.Cmd.t
val view : model -> msg Mosaic.t
val run : ?color:bool -> Run_store.run option -> unit
val run_history : ?color:bool -> Run_store.run list -> unit

val run_interactive :
  ?initial_error:string ->
  ?initial_warnings:(string * string) list ->
  ?color:bool ->
  Run_store.run list ->
  reload:(unit -> (Run_store.run list * (string * string) list, Error.t) result) ->
  start:
    (cancel:Cancel.t -> emit:(Event_bus.event -> unit) -> (int, Error.t) result) ->
  unit

module For_testing : sig
  val monochrome : string list -> string
  (** Removes only ordinary SGR sequences, even across chunk boundaries, while
      retaining terminal protocol CSI sequences. *)
end
