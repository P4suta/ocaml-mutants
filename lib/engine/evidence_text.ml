type t = {
  contents : string;
  encoding_errors : int;
  retained_raw_sha256 : string;
}

let byte value index = Char.code value.[index]
let continuation value index =
  let value = byte value index in
  value >= 0x80 && value <= 0xbf

let valid_sequence_length value index =
  let limit = String.length value in
  let first = byte value index in
  if first <= 0x7f then 1
  else if first >= 0xc2 && first <= 0xdf then
    if index + 1 < limit && continuation value (index + 1) then 2 else 0
  else if first = 0xe0 then
    if
      index + 2 < limit
      && byte value (index + 1) >= 0xa0
      && byte value (index + 1) <= 0xbf
      && continuation value (index + 2)
    then 3
    else 0
  else if
    (first >= 0xe1 && first <= 0xec) || (first >= 0xee && first <= 0xef)
  then
    if
      index + 2 < limit
      && continuation value (index + 1)
      && continuation value (index + 2)
    then 3
    else 0
  else if first = 0xed then
    if
      index + 2 < limit
      && byte value (index + 1) >= 0x80
      && byte value (index + 1) <= 0x9f
      && continuation value (index + 2)
    then 3
    else 0
  else if first = 0xf0 then
    if
      index + 3 < limit
      && byte value (index + 1) >= 0x90
      && byte value (index + 1) <= 0xbf
      && continuation value (index + 2)
      && continuation value (index + 3)
    then 4
    else 0
  else if first >= 0xf1 && first <= 0xf3 then
    if
      index + 3 < limit
      && continuation value (index + 1)
      && continuation value (index + 2)
      && continuation value (index + 3)
    then 4
    else 0
  else if first = 0xf4 then
    if
      index + 3 < limit
      && byte value (index + 1) >= 0x80
      && byte value (index + 1) <= 0x8f
      && continuation value (index + 2)
      && continuation value (index + 3)
    then 4
    else 0
  else 0

let normalize raw =
  let contents = Bytes.of_string raw in
  let errors = ref 0 in
  let index = ref 0 in
  while !index < String.length raw do
    match valid_sequence_length raw !index with
    | 0 ->
        Bytes.set contents !index '?';
        incr errors;
        incr index
    | length -> index := !index + length
  done;
  {
    contents = Bytes.unsafe_to_string contents;
    encoding_errors = !errors;
    retained_raw_sha256 =
      Digestif.SHA256.(to_hex (digest_string raw));
  }

let valid_sha256 value =
  String.length value = 64
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
       value
