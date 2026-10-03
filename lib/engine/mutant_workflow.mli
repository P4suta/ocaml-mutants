module Core = Ocaml_mutants_core

val find : Run_store.run -> string -> (Run_store.mutant_result, Error.t) result
val show : Run_store.mutant_result -> string
val patch : Core.Mutant.t -> string

val apply : root:string -> Core.Mutant.t -> (unit, Error.t) result
(** Applies only after exact digest, regular-file, range, and original-byte
    checks. An owner-private undo record is durably written first. *)

val revert : root:string -> id:string -> (unit, Error.t) result
(** Restores only when the current file exactly matches the recorded applied
    digest. No Git operation is performed. *)

val revert_patch : root:string -> id:string -> (string, Error.t) result

type expectation_edit = { path : string; before : string; after : string }

val prepare_expectation :
  root:string ->
  config:Config.loaded ->
  id:string ->
  reason:string ->
  (expectation_edit, Error.t) result

val commit_expectation : expectation_edit -> (unit, Error.t) result
