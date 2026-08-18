let ( let* ) value continuation = Result.bind value continuation
let protect f = try Ok (f ()) with Sys_error message -> Error message

let read_file path =
  protect (fun () ->
      let channel = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () -> really_input_string channel (in_channel_length channel)))

let write_file path contents =
  protect (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () -> output_string channel contents))

let rec mkdir_p path =
  if path = "" || path = "." then Ok ()
  else if Sys.file_exists path then
    if Sys.is_directory path then Ok ()
    else Error (Printf.sprintf "%s exists and is not a directory" path)
  else
    let parent = Filename.dirname path in
    let* () = if String.equal parent path then Ok () else mkdir_p parent in
    try
      Unix.mkdir path 0o755;
      Ok ()
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) when Sys.is_directory path -> Ok ()
    | Unix.Unix_error (error, function_name, argument) ->
        Error
          (Printf.sprintf "%s(%s): %s" function_name argument
             (Unix.error_message error))
    | Sys_error message -> Error message

external atomic_replace : string -> string -> bool
  = "ocaml_mutants_atomic_replace"

let atomic_write_counter = Atomic.make 0
let atomic_write_mutex = Mutex.create ()

let atomic_write path contents =
  Mutex.lock atomic_write_mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock atomic_write_mutex)
    (fun () ->
      let* () = mkdir_p (Filename.dirname path) in
      let temporary =
        Printf.sprintf "%s.tmp.%d.%d" path (Unix.getpid ())
          (Atomic.fetch_and_add atomic_write_counter 1)
      in
      match write_file temporary contents with
      | Error _ as error -> error
      | Ok () ->
          if atomic_replace temporary path then Ok ()
          else (
            (try Sys.remove temporary with Sys_error _ -> ());
            Error (Printf.sprintf "atomic replace failed for %s" path)))

let rec remove_tree path =
  try
    let stats = Unix.lstat path in
    if stats.st_kind = Unix.S_DIR then
      let entries = Sys.readdir path |> Array.to_list in
      let rec remove_entries = function
        | [] ->
            Unix.rmdir path;
            Ok ()
        | name :: rest -> (
            match remove_tree (Filename.concat path name) with
            | Ok () -> remove_entries rest
            | Error _ as error -> error)
      in
      remove_entries entries
    else (
      (try Unix.chmod path 0o600 with Unix.Unix_error _ -> ());
      Sys.remove path;
      Ok ())
  with
  | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Ok ()
  | Sys_error message -> Error message
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (Printf.sprintf "%s(%s): %s" function_name argument
           (Unix.error_message error))

let files_recursive ?(skip = fun _ -> false) root =
  let rec visit relative accumulator =
    let absolute =
      if relative = "" then root else Filename.concat root relative
    in
    if relative <> "" && skip relative then accumulator
    else if (Unix.lstat absolute).st_kind = Unix.S_DIR then
      Sys.readdir absolute |> Array.to_list |> List.sort String.compare
      |> List.fold_left
           (fun files name ->
             let child =
               if relative = "" then name else Filename.concat relative name
             in
             visit child files)
           accumulator
    else relative :: accumulator
  in
  if Sys.file_exists root then List.rev (visit "" []) else []

let normalize_relative ~root path =
  let root = Filename.concat (Unix.realpath root) "" in
  let path = Unix.realpath path in
  let relative =
    if
      String.length path >= String.length root
      && String.equal (String.sub path 0 (String.length root)) root
    then
      String.sub path (String.length root)
        (String.length path - String.length root)
    else path
  in
  Ocaml_mutants_core.Mutant.normalize_path relative

let sha256 value = Digestif.SHA256.(to_hex (digest_string value))
let digest_file path = Result.map sha256 (read_file path)

let digest_tree ?(skip = fun _ -> false) root =
  let files = files_recursive ~skip root in
  let buffer = Buffer.create 4096 in
  let rec add = function
    | [] -> Ok (sha256 (Buffer.contents buffer))
    | relative :: rest ->
        let* digest = digest_file (Filename.concat root relative) in
        Buffer.add_string buffer
          (Ocaml_mutants_core.Mutant.normalize_path relative);
        Buffer.add_char buffer '\000';
        Buffer.add_string buffer digest;
        Buffer.add_char buffer '\000';
        add rest
  in
  add files

let timestamp () =
  let time = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ" (time.tm_year + 1900)
    (time.tm_mon + 1) time.tm_mday time.tm_hour time.tm_min time.tm_sec

let split_lines value =
  let rec loop start index lines =
    if index = String.length value then
      List.rev (String.sub value start (index - start) :: lines)
    else if value.[index] = '\n' then
      let length =
        if index > start && value.[index - 1] = '\r' then index - start - 1
        else index - start
      in
      loop (index + 1) (index + 1) (String.sub value start length :: lines)
    else loop start (index + 1) lines
  in
  if value = "" then [] else loop 0 0 []

let string_starts_with ~prefix value =
  String.length value >= String.length prefix
  && String.equal (String.sub value 0 (String.length prefix)) prefix

let string_ends_with ~suffix value =
  String.length value >= String.length suffix
  && String.equal
       (String.sub value
          (String.length value - String.length suffix)
          (String.length suffix))
       suffix
