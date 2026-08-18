type t = string * string list

let of_list = function
  | [] -> Error "command argv cannot be empty"
  | program :: _ when String.trim program = "" ->
      Error "command program cannot be empty"
  | program :: arguments -> Ok (program, arguments)

let to_list (program, arguments) = program :: arguments
let program (program, _) = program
let equal left right = to_list left = to_list right

let pp formatter argv =
  Format.pp_print_list
    ~pp_sep:(fun formatter () -> Format.pp_print_char formatter ' ')
    (fun formatter value -> Format.fprintf formatter "%S" value)
    formatter (to_list argv)
