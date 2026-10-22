(** Resolution of configured test stages to concrete argument vectors. *)

val dune_managed : Ocaml_mutants_core.Nonempty_argv.t -> bool
(** [dune_managed command] holds for a Dune build, runtest, or test command
    whose build directory can be safely redirected by [resolve]. *)

val resolve : Ocaml_mutants_core.Nonempty_argv.t -> string -> string list
(** [resolve command build_dir] injects [--build-dir build_dir] immediately
    after a managed Dune subcommand. Explicit build directories and commands
    outside that closed set are preserved byte-for-byte. *)
