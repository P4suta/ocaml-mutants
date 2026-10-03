(** Resolution of configured test-stage commands to concrete argv vectors.

    Stage commands that Dune itself manages are redirected into a private build
    directory so that concurrent workers never share build state: a command of
    the form [dune build|runtest|test ...] that does not already name a
    [--build-dir] receives [--build-dir DIR] directly after the subcommand.
    Every other command runs verbatim, and its concurrency safety remains the
    user's declaration ([test.parallel_safe]). *)

val dune_managed : Ocaml_mutants_core.Nonempty_argv.t -> bool
(** [dune_managed command] holds when [command] is a Dune build, runtest, or
    test invocation without an explicit [--build-dir], i.e. when [resolve] will
    redirect it into a per-worker build directory and concurrent execution is
    safe without [test.parallel_safe]. *)

val resolve : Ocaml_mutants_core.Nonempty_argv.t -> string -> string list
(** [resolve command build_dir] is the argv to execute for a stage: the original
    vector with [--build-dir build_dir] injected when [dune_managed command]
    holds, the original vector unchanged otherwise. *)

val compiler_cache_directory : root:string -> string
(** Private cache below the disposable workspace snapshot. *)

val dune_cache_environment : root:string -> (string * string option) list
(** Dune's compiler artifacts may be reused between worker build directories,
    while user rules (including test actions) remain deliberately uncached. *)
