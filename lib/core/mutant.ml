type unchecked
type validated

module Id = struct
  type t = { full : string; short : string }

  let make full =
    let short = String.sub full 0 (min 20 (String.length full)) in
    { full; short }

  let full id = id.full
  let short id = id.short

  let is_valid_prefix value =
    let length = String.length value in
    length > 0 && length <= 64
    && String.for_all
         (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
         value

  let equal left right = String.equal left.full right.full
  let compare left right = String.compare left.full right.full
  let pp formatter id = Format.pp_print_string formatter id.short
end

type claims = { original : string; source_digest : string; full_id : string }

type 'phase mutant = {
  id : Id.t;
  path : string;
  range : Source_range.t;
  rule : Operator.Rule.t;
  original : string;
  replacement : string;
  source_digest : string;
  claims : claims option;
}

type t = validated mutant

type validation_error =
  | Invalid_path of string
  | Source_error of Source.error
  | Original_mismatch of { expected : string; actual : string }
  | Source_digest_mismatch of { expected : string; actual : string }
  | Id_mismatch of { expected : string; actual : string }
  | Empty_replacement

module Identity_sexp = struct
  type t = Atom of string | List of t list
end

module Canonical = Csexp.Make (Identity_sexp)

let sha256 value = Digestif.SHA256.(to_hex (digest_string value))

let normalize_path path =
  let components =
    path
    |> String.map (function '\\' -> '/' | character -> character)
    |> String.split_on_char '/'
    |> List.filter (fun component -> component <> "" && component <> ".")
  in
  String.concat "/" components

let validate_path path =
  let normalized = normalize_path path in
  let components = String.split_on_char '/' normalized in
  if normalized = "" then Error (Invalid_path "source path cannot be empty")
  else if not (Filename.is_relative path) then
    Error (Invalid_path "source path must be relative")
  else if List.exists (String.equal "..") components then
    Error (Invalid_path "source path cannot contain '..'")
  else if Sys.win32 && String.contains normalized ':' then
    Error (Invalid_path "source path cannot contain a drive prefix")
  else Ok normalized

let identity ~path ~range ~rule ~source_digest ~original ~replacement =
  let open Identity_sexp in
  List
    [
      Atom "ocaml-mutants-mutant-id-v2";
      Atom path;
      Atom (Operator.Rule.stable_name rule);
      Atom (string_of_int (Source_range.start_byte range));
      Atom (string_of_int (Source_range.end_byte range));
      Atom source_digest;
      Atom (sha256 original);
      Atom (sha256 replacement);
    ]
  |> Canonical.to_string |> sha256

let placeholder_id = Id.make (String.make 64 '0')

let unchecked ~path ~range ~rule ~replacement =
  match validate_path path with
  | Error error -> Error error
  | Ok path ->
      if String.trim replacement = "" then Error Empty_replacement
      else
        Ok
          {
            id = placeholder_id;
            path;
            range;
            rule;
            original = "";
            replacement;
            source_digest = "";
            claims = None;
          }

let decoded ~path ~range ~rule ~original ~replacement ~source_digest ~full_id =
  match unchecked ~path ~range ~rule ~replacement with
  | Error error -> Error error
  | Ok mutant ->
      Ok { mutant with claims = Some { original; source_digest; full_id } }

let validate ~source mutant =
  match Source.slice source mutant.range with
  | Error error -> Error (Source_error error)
  | Ok original -> (
      let source_digest = Source.digest source in
      let full_id =
        identity ~path:mutant.path ~range:mutant.range ~rule:mutant.rule
          ~source_digest ~original ~replacement:mutant.replacement
      in
      let check_claims = function
        | None -> Ok ()
        | Some (claims : claims)
          when not (String.equal claims.original original) ->
            Error
              (Original_mismatch
                 { expected = claims.original; actual = original })
        | Some (claims : claims)
          when not (String.equal claims.source_digest source_digest) ->
            Error
              (Source_digest_mismatch
                 { expected = claims.source_digest; actual = source_digest })
        | Some (claims : claims) when not (String.equal claims.full_id full_id)
          ->
            Error (Id_mismatch { expected = claims.full_id; actual = full_id })
        | Some _ -> Ok ()
      in
      match check_claims mutant.claims with
      | Error error -> Error error
      | Ok () ->
          Ok
            {
              mutant with
              id = Id.make full_id;
              original;
              source_digest;
              claims = None;
            })

let restore ~path ~range ~rule ~original ~replacement ~source_digest ~full_id =
  match validate_path path with
  | Error error -> Error error
  | Ok path ->
      let actual =
        identity ~path ~range ~rule ~source_digest ~original ~replacement
      in
      if not (String.equal actual full_id) then
        Error (Id_mismatch { expected = full_id; actual })
      else
        Ok
          {
            id = Id.make full_id;
            path;
            range;
            rule;
            original;
            replacement;
            source_digest;
            claims = None;
          }

let id mutant = mutant.id
let path mutant = mutant.path
let range mutant = mutant.range
let rule mutant = mutant.rule
let family mutant = Operator.Rule.family mutant.rule
let original mutant = mutant.original
let replacement mutant = mutant.replacement
let source_digest mutant = mutant.source_digest

let equal_identity left right =
  Id.equal left.id right.id
  && String.equal left.path right.path
  && Source_range.equal left.range right.range
  && Operator.Rule.equal left.rule right.rule
  && String.equal left.original right.original
  && String.equal left.replacement right.replacement
  && String.equal left.source_digest right.source_digest

let compare left right =
  match String.compare left.path right.path with
  | 0 -> (
      match Source_range.compare left.range right.range with
      | 0 -> Id.compare left.id right.id
      | value -> value)
  | value -> value

let pp formatter mutant =
  Format.fprintf formatter "%s:%a [%a] %a" mutant.path Source_range.pp
    mutant.range Operator.Rule.pp mutant.rule Id.pp mutant.id

let pp_validation_error formatter = function
  | Invalid_path message -> Format.pp_print_string formatter message
  | Source_error error -> Source.pp_error formatter error
  | Original_mismatch { expected; actual } ->
      Format.fprintf formatter "source slice mismatch: cached %S, actual %S"
        expected actual
  | Source_digest_mismatch { expected; actual } ->
      Format.fprintf formatter "source digest mismatch: cached %s, actual %s"
        expected actual
  | Id_mismatch { expected; actual } ->
      Format.fprintf formatter "mutant ID mismatch: cached %s, actual %s"
        expected actual
  | Empty_replacement ->
      Format.pp_print_string formatter "replacement cannot be empty"
