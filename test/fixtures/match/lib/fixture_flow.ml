let grade score =
  match score with
  | 0 -> "empty"
  | value when value > 89 -> "excellent"
  | _ -> "ordinary"

let tail_length values =
  match values with [] -> 0 | _ :: tail -> List.length tail

let first_even values =
  match List.filter (fun value -> value mod 2 = 0) values with
  | [] -> None
  | value :: _ -> Some value

let labelled value = [ Some value ]
let extended values = List.rev (0 :: values)

let described thunk =
  try thunk ()
  with Failure message -> String.concat ": " [ "recovered"; message ]
