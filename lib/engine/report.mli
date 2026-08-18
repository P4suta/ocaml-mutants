val print_run : ?color:bool -> Format.formatter -> Run_store.run -> unit
val print_catalog : Format.formatter -> Ocaml_mutants_core.Catalog.t -> unit
val print_skipped : Format.formatter -> Run_store.skip_summary list -> unit
