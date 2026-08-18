type node = {
  range : Source_range.t;
  mutants : Mutant.t list;
  children : node list;
}

type t = node list

type error =
  | Invalid_range of Mutant.t
  | Source_mismatch of Mutant.t
  | Crossing_ranges of Mutant.t * Mutant.t

type builder = {
  range : Source_range.t;
  mutable mutants : Mutant.t list;
  mutable children : builder list;
}

let compare_mutant left right =
  match
    Int.compare
      (Source_range.start_byte (Mutant.range left))
      (Source_range.start_byte (Mutant.range right))
  with
  | 0 -> (
      match
        Int.compare
          (Source_range.end_byte (Mutant.range right))
          (Source_range.end_byte (Mutant.range left))
      with
      | 0 -> Mutant.Id.compare (Mutant.id left) (Mutant.id right)
      | value -> value)
  | value -> value

let groups mutants =
  let sorted = List.sort compare_mutant mutants in
  let rec loop accumulated = function
    | [] -> List.rev accumulated
    | mutant :: rest -> (
        match accumulated with
        | group :: groups
          when Source_range.equal group.range (Mutant.range mutant) ->
            group.mutants <- mutant :: group.mutants;
            loop accumulated rest
        | _ ->
            loop
              ({
                 range = Mutant.range mutant;
                 mutants = [ mutant ];
                 children = [];
               }
              :: accumulated)
              rest)
  in
  loop [] sorted

let create ~source mutants =
  let source_length = Source.length source in
  match
    List.find_opt
      (fun mutant ->
        let range = Mutant.range mutant in
        Source_range.start_byte range < 0
        || Source_range.end_byte range > source_length
        || Source_range.start_byte range >= Source_range.end_byte range)
      mutants
  with
  | Some mutant -> Error (Invalid_range mutant)
  | None -> (
      match
        List.find_opt
          (fun mutant ->
            not
              (String.equal
                 (Mutant.source_digest mutant)
                 (Source.digest source)))
          mutants
      with
      | Some mutant -> Error (Source_mismatch mutant)
      | None -> (
          let roots = ref [] in
          let stack = ref [] in
          let rec discard_finished start =
            match !stack with
            | parent :: rest when Source_range.end_byte parent.range <= start ->
                stack := rest;
                discard_finished start
            | _ -> ()
          in
          let rec add = function
            | [] -> Ok ()
            | group :: rest -> (
                let start = Source_range.start_byte group.range in
                discard_finished start;
                match !stack with
                | [] ->
                    roots := group :: !roots;
                    stack := [ group ];
                    add rest
                | parent :: _
                  when Source_range.contains ~outer:parent.range
                         ~inner:group.range ->
                    parent.children <- group :: parent.children;
                    stack := group :: !stack;
                    add rest
                | parent :: _ ->
                    Error
                      (Crossing_ranges
                         (List.hd parent.mutants, List.hd group.mutants)))
          in
          match add (groups mutants) with
          | Error _ as error -> error
          | Ok () ->
              let rec freeze (builder : builder) : node =
                {
                  range = builder.range;
                  mutants = List.sort Mutant.compare builder.mutants;
                  children = List.rev_map freeze builder.children;
                }
              in
              Ok (List.rev_map freeze !roots)))

let roots forest = forest
let range (node : node) = node.range
let mutants (node : node) = node.mutants
let children (node : node) = node.children

let pp_error formatter = function
  | Invalid_range mutant ->
      Format.fprintf formatter "invalid source range for %a" Mutant.pp mutant
  | Source_mismatch mutant ->
      Format.fprintf formatter "source digest changed for %a" Mutant.pp mutant
  | Crossing_ranges (left, right) ->
      Format.fprintf formatter "crossing mutation ranges: %a and %a" Mutant.pp
        left Mutant.pp right
