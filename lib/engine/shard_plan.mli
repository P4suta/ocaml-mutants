module Core = Ocaml_mutants_core

type assignment = {
  index : int;
  mutant_ids : string list;
  estimated_duration_seconds : float;
}

type t = {
  plan_id : string;
  input_fingerprint : string;
  workspace_digest : string;
  toolchain : string;
  catalog_ids : string list;
  assignments : assignment list;
}

val input_fingerprint :
  workspace_digest:string ->
  toolchain:string ->
  config:Config.t ->
  catalog:Core.Catalog.t ->
  string

val create :
  workspace_digest:string ->
  toolchain:string ->
  config:Config.t ->
  catalog:Core.Catalog.t ->
  shard_count:int ->
  durations:(string * float) list ->
  (t, string) result

val assignment : t -> int -> (assignment, string) result
val selection_tag : t -> index:int -> string
val to_yojson : t -> Yojson.Safe.t
val to_string : t -> string
val of_yojson : Yojson.Safe.t -> (t, string) result
val of_string : string -> (t, string) result

val merge :
  plan:t ->
  id:Core.Run_id.t ->
  finished_at:string ->
  Run_store.run list ->
  (Run_store.run, string) result
