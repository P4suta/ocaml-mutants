module Id_map = Map.Make (String)

type t = { mutants : Mutant.t Id_map.t; exact_duplicates : int }

type error =
  | Hash_collision of { id : string; left : Mutant.t; right : Mutant.t }

let empty = { mutants = Id_map.empty; exact_duplicates = 0 }

let of_list_with_short_id short_id mutants =
  let sorted = List.sort Mutant.compare mutants in
  let add catalog mutant =
    let short = short_id mutant in
    match Id_map.find_opt short catalog.mutants with
    | None ->
        Ok { catalog with mutants = Id_map.add short mutant catalog.mutants }
    | Some existing when Mutant.equal_identity existing mutant ->
        Ok { catalog with exact_duplicates = catalog.exact_duplicates + 1 }
    | Some existing ->
        Error (Hash_collision { id = short; left = existing; right = mutant })
  in
  List.fold_left
    (fun result mutant ->
      match result with
      | Error _ as error -> error
      | Ok catalog -> add catalog mutant)
    (Ok empty) sorted

let of_list =
  of_list_with_short_id (fun mutant -> Mutant.Id.short (Mutant.id mutant))

module For_testing = struct
  let of_list_with_short_id = of_list_with_short_id
end

let to_list catalog =
  Id_map.bindings catalog.mutants |> List.map snd |> List.sort Mutant.compare

let length catalog = Id_map.cardinal catalog.mutants
let exact_duplicates catalog = catalog.exact_duplicates

let filter predicate catalog =
  {
    catalog with
    mutants = Id_map.filter (fun _ mutant -> predicate mutant) catalog.mutants;
  }

let find id catalog =
  match Id_map.find_opt id catalog.mutants with
  | Some _ as found -> found
  | None ->
      Id_map.fold
        (fun _ mutant found ->
          match found with
          | Some _ -> found
          | None ->
              if String.equal (Mutant.Id.full (Mutant.id mutant)) id then
                Some mutant
              else None)
        catalog.mutants None

let pp_error formatter = function
  | Hash_collision { id; left; right } ->
      Format.fprintf formatter "mutant ID collision for %s: %a and %a" id
        Mutant.pp left Mutant.pp right
