type t = { line : int; column : int }

let make ~line ~column =
  if line < 1 then Error "line numbers are one-based"
  else if column < 0 then Error "columns must be non-negative"
  else Ok { line; column }

let line location = location.line
let column location = location.column

let compare left right =
  match Int.compare left.line right.line with
  | 0 -> Int.compare left.column right.column
  | value -> value

let pp formatter location =
  Format.fprintf formatter "%d:%d" location.line location.column
