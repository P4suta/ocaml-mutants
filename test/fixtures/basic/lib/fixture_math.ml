let add left right = left + right
let positive value = value > 0
let classify value = if positive value then true else false
let below_limit value = value < 10
let maybe_positive value = if positive value then Some value else None
let items value = if positive value then [ value ] else []

let guarded value =
  match value with
  | candidate when candidate > 0 -> true
  | _ -> false

module Local_stdlib = struct
  let ( + ) left right = left - right
end

let combine left right = Local_stdlib.(left + right)
let always _ = true
