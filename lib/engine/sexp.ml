type t = Atom of string | List of t list

module Codec = Csexp.Make (struct
  type nonrec t = t = Atom of string | List of t list
end)

let parse contents =
  match Codec.parse_string contents with
  | Ok value -> Ok value
  | Error (offset, message) ->
      Error
        (Printf.sprintf "invalid canonical S-expression at byte %d: %s" offset
           message)

let rec atoms = function
  | Atom value -> [ value ]
  | List values -> List.concat_map atoms values

let rec pp formatter = function
  | Atom value -> Format.fprintf formatter "%S" value
  | List values ->
      Format.fprintf formatter "(@[%a@])"
        (Format.pp_print_list
           ~pp_sep:(fun formatter () -> Format.pp_print_space formatter ())
           pp)
        values
