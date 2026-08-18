module type S = sig
  type token

  val install : int -> (unit -> unit) -> token

  val restore : token -> unit
  (** [install] subscribes to a process-lifetime OS interrupt router. Its first
      call may fail while establishing that router; after a token is returned,
      [restore] is a total in-memory deactivation and never restores the OS
      handler. Implementations must support [Sys.sigint] and [Sys.sigterm]. *)
end

module System : S
