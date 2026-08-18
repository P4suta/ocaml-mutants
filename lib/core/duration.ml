type t = float

let zero = 0.

let of_seconds value =
  match Float.classify_float value with
  | FP_nan | FP_infinite -> Error "duration must be finite"
  | FP_normal | FP_subnormal | FP_zero ->
      if value < 0. then Error "duration must be non-negative" else Ok value

let to_seconds value = value
let add left right = left +. right
let compare = Float.compare
let pp formatter value = Format.fprintf formatter "%.6gs" value
