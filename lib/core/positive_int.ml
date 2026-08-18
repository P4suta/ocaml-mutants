type t = int

let of_int value =
  if value <= 0 then Error "value must be positive" else Ok value

let to_int value = value
let one = 1
let pp = Format.pp_print_int
