type t = string

let valid_character = function
  | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '-' | '_' | '.' -> true
  | _ -> false

let of_string value =
  if value = "" then Error "run ID cannot be empty"
  else if not (String.for_all valid_character value) then
    Error "run ID contains an unsafe character"
  else Ok value

let create ~started_at ~nonce =
  let sanitize value =
    String.map
      (fun character -> if valid_character character then character else '-')
      value
  in
  of_string (sanitize started_at ^ "-" ^ sanitize nonce)

let to_string value = value
let compare = String.compare
let pp = Format.pp_print_string
