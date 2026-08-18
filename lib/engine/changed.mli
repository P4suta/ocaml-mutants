module type PROCESS = sig
  type result

  val run :
    cancel:Cancel.t ->
    cwd:string ->
    env:(string * string option) list ->
    string list ->
    result

  val cancelled : result -> bool
  val succeeded : result -> bool
  val stdout : result -> string
  val stderr : result -> string
end

module Make (Process : PROCESS) : sig
  val files :
    cancel:Cancel.t ->
    root:string ->
    from:string option ->
    (string list, Error.t) result
end

module System : sig
  val files :
    cancel:Cancel.t ->
    root:string ->
    from:string option ->
    (string list, Error.t) result
end

val files :
  cancel:Cancel.t ->
  root:string ->
  from:string option ->
  (string list, Error.t) result
(** Compatibility entry point backed by [System]. *)
