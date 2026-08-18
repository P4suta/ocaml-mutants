type t = { start_byte : int; end_byte : int }

let make ~start_byte ~end_byte =
  if start_byte < 0 then Error "start byte must be non-negative"
  else if end_byte <= start_byte then Error "end byte must follow start byte"
  else Ok { start_byte; end_byte }

let start_byte span = span.start_byte
let end_byte span = span.end_byte
let length span = span.end_byte - span.start_byte

let compare left right =
  match Int.compare left.start_byte right.start_byte with
  | 0 -> Int.compare left.end_byte right.end_byte
  | value -> value

let equal left right =
  left.start_byte = right.start_byte && left.end_byte = right.end_byte

let contains ~outer ~inner =
  outer.start_byte <= inner.start_byte && inner.end_byte <= outer.end_byte

let strictly_contains ~outer ~inner =
  contains ~outer ~inner && not (equal outer inner)

let overlaps left right =
  left.start_byte < right.end_byte && right.start_byte < left.end_byte

let pp formatter span =
  Format.fprintf formatter "%d-%d" span.start_byte span.end_byte
