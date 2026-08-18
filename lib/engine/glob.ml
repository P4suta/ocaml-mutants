let normalize value =
  let value = Ocaml_mutants_core.Mutant.normalize_path value in
  if Sys.win32 then String.lowercase_ascii value else value

let matches ~pattern path =
  let pattern = normalize pattern in
  let path = normalize path in
  let pattern_length = String.length pattern in
  let path_length = String.length path in
  let memo = Hashtbl.create 128 in
  let rec loop pattern_index path_index =
    match Hashtbl.find_opt memo (pattern_index, path_index) with
    | Some result -> result
    | None ->
        let result =
          if pattern_index = pattern_length then path_index = path_length
          else if
            pattern_index + 2 < pattern_length
            && pattern.[pattern_index] = '*'
            && pattern.[pattern_index + 1] = '*'
            && pattern.[pattern_index + 2] = '/'
          then
            loop (pattern_index + 3) path_index
            || (path_index < path_length && loop pattern_index (path_index + 1))
          else if
            pattern_index + 1 < pattern_length
            && pattern.[pattern_index] = '*'
            && pattern.[pattern_index + 1] = '*'
          then
            loop (pattern_index + 2) path_index
            || (path_index < path_length && loop pattern_index (path_index + 1))
          else
            match pattern.[pattern_index] with
            | '*' ->
                loop (pattern_index + 1) path_index
                || path_index < path_length
                   && path.[path_index] <> '/'
                   && loop pattern_index (path_index + 1)
            | '?' ->
                path_index < path_length
                && path.[path_index] <> '/'
                && loop (pattern_index + 1) (path_index + 1)
            | character ->
                path_index < path_length
                && character = path.[path_index]
                && loop (pattern_index + 1) (path_index + 1)
        in
        Hashtbl.replace memo (pattern_index, path_index) result;
        result
  in
  loop 0 0

let selected ~include_ ~exclude path =
  List.exists (fun pattern -> matches ~pattern path) include_
  && not (List.exists (fun pattern -> matches ~pattern path) exclude)
