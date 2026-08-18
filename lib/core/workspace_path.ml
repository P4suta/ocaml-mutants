type t = Fpath.t

let has_parent_component path =
  path
  |> String.map (function '\\' -> '/' | character -> character)
  |> String.split_on_char '/'
  |> List.exists (String.equal "..")

let of_string path =
  if String.trim path = "" then Error "workspace path cannot be empty"
  else if has_parent_component path then
    Error "workspace path cannot contain '..'"
  else
    match Fpath.of_string path with
    | Error (`Msg message) -> Error message
    | Ok path when not (Fpath.is_abs path) ->
        Error "workspace path must be absolute"
    | Ok path -> Ok (Fpath.normalize path)

let to_string = Fpath.to_string

let append root relative =
  if relative = "" then Error "relative path cannot be empty"
  else if not (Filename.is_relative relative) then Error "path must be relative"
  else if has_parent_component relative then Error "path cannot contain '..'"
  else
    match Fpath.of_string relative with
    | Error (`Msg message) -> Error message
    | Ok relative -> Ok Fpath.(to_string (root // relative))

let equal = Fpath.equal
let pp = Fpath.pp
