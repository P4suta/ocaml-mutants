module String_map = Map.Make (String)

type partial
type complete
type 'state t = { catalog : Catalog.t; outcomes : Outcome.t String_map.t }

type error =
  | Unknown_mutant of string
  | Duplicate_result of string
  | Missing_results of Mutant.t list

let create catalog = { catalog; outcomes = String_map.empty }

let record results mutant outcome =
  let id = Mutant.Id.short (Mutant.id mutant) in
  match Catalog.find id results.catalog with
  | None -> Error (Unknown_mutant id)
  | Some expected when not (Mutant.equal_identity expected mutant) ->
      Error (Unknown_mutant id)
  | Some _ when String_map.mem id results.outcomes ->
      Error (Duplicate_result id)
  | Some _ ->
      Ok { results with outcomes = String_map.add id outcome results.outcomes }

let not_run results =
  Catalog.to_list results.catalog
  |> List.filter (fun mutant ->
      not (String_map.mem (Mutant.Id.short (Mutant.id mutant)) results.outcomes))

let finish results =
  match not_run results with
  | [] -> Ok { catalog = results.catalog; outcomes = results.outcomes }
  | missing -> Error (Missing_results missing)

let of_complete_list catalog outcomes =
  let result =
    List.fold_left
      (fun result (mutant, outcome) ->
        match result with
        | Error _ as error -> error
        | Ok partial -> record partial mutant outcome)
      (Ok (create catalog))
      outcomes
  in
  match result with Error _ as error -> error | Ok partial -> finish partial

let to_list results =
  Catalog.to_list results.catalog
  |> List.filter_map (fun mutant ->
      let id = Mutant.Id.short (Mutant.id mutant) in
      Option.map
        (fun outcome -> (mutant, outcome))
        (String_map.find_opt id results.outcomes))

let pp_error formatter = function
  | Unknown_mutant id -> Format.fprintf formatter "unknown mutant %s" id
  | Duplicate_result id ->
      Format.fprintf formatter "duplicate result for mutant %s" id
  | Missing_results mutants ->
      Format.fprintf formatter "%d mutant result(s) are missing"
        (List.length mutants)
