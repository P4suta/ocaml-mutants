type t = {
  span : Byte_span.t;
  start_location : Location.t;
  end_location : Location.t;
}

let make ~start_byte ~end_byte ~start_line ~start_column ~end_line ~end_column =
  match
    ( Byte_span.make ~start_byte ~end_byte,
      Location.make ~line:start_line ~column:start_column,
      Location.make ~line:end_line ~column:end_column )
  with
  | Ok span, Ok start_location, Ok end_location ->
      Ok { span; start_location; end_location }
  | Error message, _, _ | _, Error message, _ | _, _, Error message ->
      Error message

let span range = range.span
let start_location range = range.start_location
let end_location range = range.end_location
let start_byte range = Byte_span.start_byte range.span
let end_byte range = Byte_span.end_byte range.span
let start_line range = Location.line range.start_location
let start_column range = Location.column range.start_location
let end_line range = Location.line range.end_location
let end_column range = Location.column range.end_location
let byte_length range = Byte_span.length range.span
let compare left right = Byte_span.compare left.span right.span
let equal left right = Byte_span.equal left.span right.span

let contains ~outer ~inner =
  Byte_span.contains ~outer:outer.span ~inner:inner.span

let strictly_contains ~outer ~inner =
  Byte_span.strictly_contains ~outer:outer.span ~inner:inner.span

let overlaps left right = Byte_span.overlaps left.span right.span

let pp formatter range =
  Format.fprintf formatter "%a-%a" Location.pp range.start_location Location.pp
    range.end_location
