type t = { bytes : string; digest : string; line_starts : int array }
type error = Span_out_of_bounds of Source_range.t

let sha256 value = Digestif.SHA256.(to_hex (digest_string value))

let index_lines bytes =
  let starts = ref [ 0 ] in
  String.iteri
    (fun index character ->
      if character = '\n' then starts := (index + 1) :: !starts)
    bytes;
  Array.of_list (List.rev !starts)

let of_string bytes =
  { bytes; digest = sha256 bytes; line_starts = index_lines bytes }

let to_string source = source.bytes
let length source = String.length source.bytes
let digest source = source.digest

let location_at_byte source ~byte =
  if byte < 0 || byte > String.length source.bytes then None
  else
    let rec find low high =
      if low = high then low
      else
        let middle = low + ((high - low + 1) / 2) in
        if source.line_starts.(middle) <= byte then find middle high
        else find low (middle - 1)
    in
    let line_index = find 0 (Array.length source.line_starts - 1) in
    Location.make ~line:(line_index + 1)
      ~column:(byte - source.line_starts.(line_index))
    |> Result.to_option

let slice source range =
  let start = Source_range.start_byte range in
  let length = Source_range.byte_length range in
  if start < 0 || length <= 0 || start + length > String.length source.bytes
  then Error (Span_out_of_bounds range)
  else Ok (String.sub source.bytes start length)

let pp_error formatter = function
  | Span_out_of_bounds range ->
      Format.fprintf formatter "source span %a is outside the source bytes"
        Source_range.pp range
