module Core = Ocaml_mutants_core

let dune_subcommands = [ "build"; "runtest"; "test" ]

let names_build_dir argument =
  argument = "--build-dir" || String.starts_with ~prefix:"--build-dir=" argument

let dune_managed command =
  match Core.Nonempty_argv.to_list command with
  | "dune" :: subcommand :: rest ->
      List.mem subcommand dune_subcommands
      && not (List.exists names_build_dir rest)
  | _ -> false

let resolve command build_dir =
  if dune_managed command then
    match Core.Nonempty_argv.to_list command with
    | "dune" :: subcommand :: rest ->
        "dune" :: subcommand :: "--build-dir" :: build_dir :: rest
    | _ -> assert false
  else Core.Nonempty_argv.to_list command

let compiler_cache_directory ~root =
  Filename.concat root ".ocaml-mutants-compiler-cache"

let dune_cache_environment ~root =
  [
    ("DUNE_CACHE", Some "enabled-except-user-rules");
    ("DUNE_CACHE_ROOT", Some (compiler_cache_directory ~root));
  ]
