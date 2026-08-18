type t =
  | Killed
  | Survived
  | Timeout
  | Inconclusive of string
  | Error of string

let name = function
  | Killed -> "killed"
  | Survived -> "survived"
  | Timeout -> "timeout"
  | Inconclusive _ -> "inconclusive"
  | Error _ -> "error"

let of_string = function
  | "killed" -> Ok Killed
  | "survived" -> Ok Survived
  | "timeout" -> Ok Timeout
  | "inconclusive" -> Ok (Inconclusive "stored inconclusive result")
  | "error" -> Ok (Error "stored process error")
  | value -> Error (Printf.sprintf "unknown outcome %S" value)

let pp formatter outcome = Format.pp_print_string formatter (name outcome)
