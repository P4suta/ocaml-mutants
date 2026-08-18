type error = Interval_forest.error

let pp_error = Interval_forest.pp_error

let top_level_module_names source =
  let add_name names name =
    match name.Asttypes.txt with None -> names | Some name -> name :: names
  in
  try
    let structure = Parse.implementation (Lexing.from_string source) in
    List.fold_left
      (fun names item ->
        match item.Parsetree.pstr_desc with
        | Parsetree.Pstr_module binding ->
            add_name names binding.Parsetree.pmb_name
        | Parsetree.Pstr_recmodule bindings ->
            List.fold_left
              (fun names binding -> add_name names binding.Parsetree.pmb_name)
              names bindings
        | _ -> names)
      [] structure
  with _ -> []

let fresh_module_name source =
  let occupied = top_level_module_names source in
  let rec choose index =
    let candidate = Printf.sprintf "Ocaml_mutants_runtime_%d" index in
    if List.mem candidate occupied then choose (index + 1) else candidate
  in
  choose 0

let instrument ~source mutants =
  if mutants = [] then Ok (Source.to_string source)
  else
    match Interval_forest.create ~source mutants with
    | Error _ as error -> error
    | Ok forest ->
        let bytes = Source.to_string source in
        let runtime = fresh_module_name bytes in
        let rec render_node node =
          let range = Interval_forest.range node in
          let original =
            render_slice
              (Source_range.start_byte range)
              (Source_range.end_byte range)
              (Interval_forest.children node)
          in
          let alternatives =
            Interval_forest.mutants node
            |> List.map (fun mutant ->
                Printf.sprintf "| Some %S -> (%s)"
                  (Mutant.Id.short (Mutant.id mutant))
                  (Mutant.replacement mutant))
            |> String.concat " "
          in
          Printf.sprintf
            "(match %s.active with | None -> (%s) %s | Some _ -> (%s))" runtime
            original alternatives original
        and render_slice start_byte end_byte children =
          let buffer = Buffer.create (end_byte - start_byte + 128) in
          let cursor = ref start_byte in
          List.iter
            (fun child ->
              let range = Interval_forest.range child in
              Buffer.add_substring buffer bytes !cursor
                (Source_range.start_byte range - !cursor);
              Buffer.add_string buffer (render_node child);
              cursor := Source_range.end_byte range)
            children;
          Buffer.add_substring buffer bytes !cursor (end_byte - !cursor);
          Buffer.contents buffer
        in
        let body =
          render_slice 0 (String.length bytes) (Interval_forest.roots forest)
        in
        Ok
          (Printf.sprintf
             "module %s = struct\n\
             \  let active = Stdlib.Sys.getenv_opt \"OCAML_MUTANTS_ACTIVE\"\n\
              end\n\
              %s"
             runtime body)
