module Engine = Ocaml_mutants_engine
module C = Engine.Dir_cap

let ( let* ) value continuation = Result.bind value continuation

module Fake = struct
  module Native_name = struct
    type t = string

    let equal = String.equal
    let encode value = value
    let pp = Format.pp_print_string
  end

  module Identity = struct
    type t = int

    let equal = Int.equal
    let encode value = Printf.sprintf "memory-object:%d" value
    let pp = Format.pp_print_int
  end

  type owner = int

  type node_kind =
    | Dir of (string, int) Hashtbl.t
    | File of string
    | Link of string

  type node = {
    id : int;
    mutable kind : node_kind;
    mutable permissions : C.Permissions.t;
  }

  type held = { identity : int; mutable shared : int; mutable exclusive : bool }

  type world = {
    nodes : (int, node) Hashtbl.t;
    locks : (int * string, held) Hashtbl.t;
    mutable next : int;
    mutable owner : owner;
    mutable advisory : C.error option;
    mutable close_faults : C.local_handle_state list;
    mutable close_calls : int;
    mutable enumeration_claim : (string list * int64 * int64) option;
    mutable file_creation_fault : [ `Before_commit | `After_commit ] option;
    mutable deletion_fault : [ `Before_commit | `After_commit ] option;
  }

  type local = { mutable closed : owner list }

  type dir = {
    world : world;
    id : int;
    parent_id : int option;
    captured_name : string option;
    deletion_authority : bool;
    owner : owner;
    local : local;
  }

  type file = {
    world : world;
    id : int;
    mutable parent : int;
    mutable name : string;
    deletion_authority : bool;
    owner : owner;
    local : local;
  }

  type path_probe = {
    world : world;
    owner : owner;
    existing : int;
    chain : int list;
    missing : string list;
    local : local;
    mutable available : bool;
  }

  type separation_witness = {
    world : world;
    owner : owner;
    forbidden : int list;
    candidate : int;
    missing : string list;
    forbidden_local : local;
    candidate_local : local;
    mutable materialization_available : bool;
  }

  type enumeration_budget = {
    max_entries : int64;
    max_native_name_bytes : int64;
  }

  type lock = {
    world : world;
    owner : owner;
    key : int * string;
    file_identity : int;
    mode : C.lock_mode;
    local : local;
    mutable released : bool;
  }

  type stat = {
    identity : Identity.t;
    kind : C.entry_kind;
    size : int64;
    permissions : C.Permissions.t;
    mtime_ns : int64;
  }

  type captured_read = { contents : string; stat : stat }

  type creation_evidence =
    | Creation_observed of Native_name.t
    | Creation_may_have_committed of Native_name.t

  type materialized = {
    directory : dir;
    disposition : C.materialization;
    created : creation_evidence list;
    advisories : C.issue list;
  }

  type enumeration_consumption = { entries : int64; native_name_bytes : int64 }

  type enumeration_outcome =
    | Enumerated of {
        names : Native_name.t list;
        consumption : enumeration_consumption;
      }
    | Enumeration_incomplete of {
        consumption : enumeration_consumption;
        failure : C.failure;
      }

  type materialization_outcome =
    | Materialized of materialized
    | Materialization_incomplete of {
        created : creation_evidence list;
        failure : C.failure;
      }

  type 'cap creation_residual =
    | Captured of 'cap
    | Uncaptured of creation_evidence

  type 'cap creation_outcome =
    | Not_created of C.error
    | Created of 'cap
    | Creation_incomplete of {
        residual : 'cap creation_residual;
        failure : C.failure;
      }

  let active : world option ref = ref None

  let world () =
    match !active with Some world -> world | None -> failwith "fake not reset"

  let error ?component operation class_ code =
    C.make_error ~operation ~class_ ~native_domain:C.In_memory ~native_code:code
      ?component ()

  let failure error = C.failure_of_error error

  let permissions value =
    match C.Permissions.of_int value with
    | Ok value -> value
    | Error _ -> invalid_arg "invalid fake permission literal"

  let node world id = Hashtbl.find world.nodes id

  let kind = function
    | Dir _ -> C.Directory
    | File _ -> C.Regular
    | Link _ -> C.Symbolic_link

  let entries operation world id =
    match (node world id).kind with
    | Dir entries -> Ok entries
    | Link _ -> Error (error operation C.Link_like "link")
    | File _ -> Error (error operation C.Not_directory "not-directory")

  let alloc world permissions kind =
    let id = world.next in
    world.next <- id + 1;
    Hashtbl.add world.nodes id { id; kind; permissions };
    id

  let name_of_component value =
    if
      String.equal value "" || String.equal value "." || String.equal value ".."
      || String.contains value '/'
      || String.contains value '\000'
    then Error (error C.Name_of_component C.Invalid_name "invalid-name")
    else Ok value

  let path_components operation path =
    if String.equal path "/" then Ok []
    else if String.length path < 2 || path.[0] <> '/' then
      Error (error operation C.Invalid_name "not-absolute")
    else
      let rec validate result = function
        | [] -> Ok (List.rev result)
        | item :: rest -> (
            match name_of_component item with
            | Ok item -> validate (item :: result) rest
            | Error problem -> Error { problem with operation })
      in
      validate []
        (String.sub path 1 (String.length path - 1) |> String.split_on_char '/')

  let current_owner () = (world ()).owner
  let owner_equal = Int.equal
  let probe_owner (value : path_probe) = value.owner
  let dir_owner (value : dir) = value.owner
  let file_owner (value : file) = value.owner
  let lock_owner (value : lock) = value.owner
  let dir_identity (value : dir) = value.id
  let file_identity (value : file) = value.id
  let lock_directory_identity (value : lock) = fst value.key
  let lock_file_identity (value : lock) = value.file_identity

  let probe_display (value : path_probe) =
    List.fold_left Filename.concat "/captured" value.missing

  let check operation (world : world) (owner : owner) (local : local) =
    if owner <> world.owner then Error (error operation C.Wrong_process "fork")
    else if List.mem owner local.closed then
      Error (error operation C.Closed_capability "closed")
    else Ok ()

  let close operation (world : world) (owner : owner) (local : local) =
    world.close_calls <- world.close_calls + 1;
    if List.mem world.owner local.closed then C.Cleanup_complete
    else
      match world.close_faults with
      | state :: remaining ->
          world.close_faults <- remaining;
          if state <> C.Still_open then
            local.closed <- world.owner :: local.closed;
          C.Cleanup_failed
            {
              primary =
                {
                  error = error operation C.Other "injected-close-failure";
                  local_handle_state = state;
                  namespace_released = false;
                };
              suppressed = [];
            }
      | [] ->
          local.closed <- world.owner :: local.closed;
          if owner = world.owner then C.Cleanup_complete
          else C.Cleanup_local_only

  let probe_path path =
    let world = world () in
    match path_components C.Probe_path path with
    | Error primary -> Error (failure primary)
    | Ok components ->
        let rec walk current chain = function
          | [] ->
              Ok
                {
                  world;
                  owner = world.owner;
                  existing = current;
                  chain = List.rev chain;
                  missing = [];
                  local = { closed = [] };
                  available = true;
                }
          | name :: rest -> (
              let* directory = entries C.Probe_path world current in
              match Hashtbl.find_opt directory name with
              | None ->
                  Ok
                    {
                      world;
                      owner = world.owner;
                      existing = current;
                      chain = List.rev chain;
                      missing = name :: rest;
                      local = { closed = [] };
                      available = true;
                    }
              | Some child -> (
                  match (node world child).kind with
                  | Dir _ -> walk child (child :: chain) rest
                  | Link _ -> Error (error C.Probe_path C.Link_like "link")
                  | File _ ->
                      Error (error C.Probe_path C.Not_directory "not-directory")
                  ))
        in
        walk 1 [ 1 ] components |> Result.map_error failure

  let rec prefix left right =
    match (left, right) with
    | [], _ -> true
    | _, [] -> false
    | x :: xs, y :: ys -> String.equal x y && prefix xs ys

  let check_probe operation (probe : path_probe) =
    let* () = check operation probe.world probe.owner probe.local in
    if probe.available then Ok ()
    else Error (error operation C.Closed_capability "probe-consumed")

  let relationship (left : path_probe) (right : path_probe) =
    let* () = check_probe C.Relate_paths left in
    let* () = check_probe C.Relate_paths right in
    let complete (value : path_probe) = value.missing = [] in
    let contains (first : path_probe) (second : path_probe) =
      if complete first then
        List.mem first.existing second.chain
        && not (complete second && first.existing = second.existing)
      else
        first.existing = second.existing
        && first.missing <> second.missing
        && prefix first.missing second.missing
    in
    if left.world != right.world then Ok C.Separate
    else if left.existing = right.existing && left.missing = right.missing then
      Ok C.Same
    else if contains left right then Ok C.Left_contains_right
    else if contains right left then Ok C.Right_contains_left
    else Ok C.Separate

  let establish_separation ~(forbidden : path_probe) ~(candidate : path_probe) =
    match
      let* () = check_probe C.Establish_separation forbidden in
      let* () = check_probe C.Establish_separation candidate in
      let* relation = relationship forbidden candidate in
      if relation = C.Separate then Ok ()
      else
        Error
          (error C.Establish_separation C.Unsupported
             "paths-not-proven-separate")
    with
    | Error problem -> Error (failure problem)
    | Ok () ->
        forbidden.available <- false;
        candidate.available <- false;
        Ok
          {
            world = candidate.world;
            owner = candidate.owner;
            forbidden = forbidden.chain;
            candidate = candidate.existing;
            missing = candidate.missing;
            forbidden_local = forbidden.local;
            candidate_local = candidate.local;
            materialization_available = true;
          }

  let materialize (witness : separation_witness) ~permissions =
    let fail created problem =
      Materialization_incomplete
        { created = List.rev created; failure = failure problem }
    in
    let rec create current made created = function
      | [] ->
          Materialized
            {
              directory =
                {
                  world = witness.world;
                  id = current;
                  parent_id = None;
                  captured_name = None;
                  deletion_authority = false;
                  owner = witness.owner;
                  local = { closed = [] };
                };
              disposition =
                (if made then C.Newly_created else C.Already_present);
              created = List.rev created;
              advisories = [];
            }
      | name :: rest -> (
          match entries C.Materialize witness.world current with
          | Error primary -> fail created primary
          | Ok directory -> (
              let child, made_here =
                match Hashtbl.find_opt directory name with
                | Some child -> (child, false)
                | None ->
                    let child =
                      alloc witness.world permissions (Dir (Hashtbl.create 4))
                    in
                    Hashtbl.add directory name child;
                    (child, true)
              in
              if List.mem child witness.forbidden then
                fail created
                  (error C.Materialize C.Unsupported
                     "component-entered-forbidden-chain")
              else
                match (node witness.world child).kind with
                | Dir _ ->
                    let created =
                      if made_here then Creation_observed name :: created
                      else created
                    in
                    create child (made || made_here) created rest
                | Link _ ->
                    fail created (error C.Materialize C.Link_like "link")
                | File _ ->
                    fail created
                      (error C.Materialize C.Not_directory "not-directory")))
    in
    let outcome =
      match
        check C.Materialize witness.world witness.owner witness.candidate_local
      with
      | Error primary -> fail [] primary
      | Ok () when not witness.materialization_available ->
          fail []
            (error C.Materialize C.Closed_capability "materialization-consumed")
      | Ok () ->
          witness.materialization_available <- false;
          if List.mem witness.candidate witness.forbidden then
            fail []
              (error C.Materialize C.Unsupported
                 "candidate-entered-forbidden-chain")
          else create witness.candidate false [] witness.missing
    in
    let cleanup =
      [
        close C.Close_separation witness.world witness.owner
          witness.candidate_local;
        close C.Close_separation witness.world witness.owner
          witness.forbidden_local;
      ]
    in
    let advisories =
      List.filter_map
        (function
          | C.Cleanup_complete | C.Cleanup_local_only -> None
          | C.Cleanup_failed failure -> Some (C.Cleanup_error failure))
        cleanup
    in
    match outcome with
    | Materialized materialized -> Materialized { materialized with advisories }
    | Materialization_incomplete incomplete ->
        Materialization_incomplete
          {
            incomplete with
            failure =
              {
                incomplete.failure with
                suppressed = incomplete.failure.suppressed @ advisories;
              };
          }

  let check_dir operation (directory : dir) =
    check operation directory.world directory.owner directory.local

  let check_file operation (file : file) =
    check operation file.world file.owner file.local

  let as_failure result = Result.map_error failure result

  let enumeration_budget ~max_entries ~max_native_name_bytes =
    if Int64.compare max_entries 0L < 0 then
      Error (C.Negative_max_entries max_entries)
    else if Int64.compare max_native_name_bytes 0L < 0 then
      Error (C.Negative_max_native_name_bytes max_native_name_bytes)
    else Ok { max_entries; max_native_name_bytes }

  let enumerate_no_follow (directory : dir) ~budget =
    let consumption entries native_name_bytes =
      { entries; native_name_bytes }
    in
    let incomplete entries native_name_bytes problem =
      Enumeration_incomplete
        {
          consumption = consumption entries native_name_bytes;
          failure = failure problem;
        }
    in
    match check_dir C.Enumerate directory with
    | Error problem -> incomplete 0L 0L problem
    | Ok () -> (
        match directory.world.enumeration_claim with
        | Some (names, entries, native_name_bytes) ->
            directory.world.enumeration_claim <- None;
            Enumerated
              { names; consumption = consumption entries native_name_bytes }
        | None -> (
            match entries C.Enumerate directory.world directory.id with
            | Error problem -> incomplete 0L 0L problem
            | Ok children ->
                let names =
                  Hashtbl.to_seq_keys children
                  |> List.of_seq |> List.sort String.compare
                in
                let rec consume accepted entries native_name_bytes = function
                  | [] ->
                      Enumerated
                        {
                          names = List.rev accepted;
                          consumption = consumption entries native_name_bytes;
                        }
                  | name :: rest ->
                      let bytes = Int64.of_int (String.length name) in
                      if
                        Int64.compare entries budget.max_entries >= 0
                        || Int64.compare bytes
                             (Int64.sub budget.max_native_name_bytes
                                native_name_bytes)
                           > 0
                      then
                        incomplete entries native_name_bytes
                          (error C.Enumerate C.Too_large
                             "enumeration-budget-exhausted")
                      else
                        consume (name :: accepted) (Int64.succ entries)
                          (Int64.add native_name_bytes bytes)
                          rest
                in
                consume [] 0L 0L names))

  let probe_entry_no_follow (directory : dir) name =
    as_failure
      (let* () = check_dir C.Probe_entry directory in
       let* children = entries C.Probe_entry directory.world directory.id in
       match Hashtbl.find_opt children name with
       | None -> Ok None
       | Some identity ->
           let node = node directory.world identity in
           let size =
             match node.kind with
             | File contents -> String.length contents
             | Dir _ | Link _ -> 0
           in
           Ok
             (Some
                {
                  identity;
                  kind = kind node.kind;
                  size = Int64.of_int size;
                  permissions = node.permissions;
                  mtime_ns = 0L;
                }))

  let open_directory_no_follow (directory : dir) name =
    as_failure
      (let* () = check_dir C.Open_directory directory in
       let* children = entries C.Open_directory directory.world directory.id in
       match Hashtbl.find_opt children name with
       | None -> Error (error C.Open_directory C.Missing "missing")
       | Some id -> (
           match (node directory.world id).kind with
           | Dir _ ->
               Ok
                 {
                   world = directory.world;
                   id;
                   parent_id = Some directory.id;
                   captured_name = Some name;
                   deletion_authority = false;
                   owner = directory.owner;
                   local = { closed = [] };
                 }
           | Link _ -> Error (error C.Open_directory C.Link_like "link")
           | File _ ->
               Error (error C.Open_directory C.Not_directory "not-directory")))

  let open_directory_for_delete_no_follow (directory : dir) name =
    as_failure
      (let* () = check_dir C.Open_directory directory in
       let* children = entries C.Open_directory directory.world directory.id in
       match Hashtbl.find_opt children name with
       | None -> Error (error C.Open_directory C.Missing "missing")
       | Some id -> (
           match (node directory.world id).kind with
           | Dir _ ->
               Ok
                 {
                   world = directory.world;
                   id;
                   parent_id = Some directory.id;
                   captured_name = Some name;
                   deletion_authority = true;
                   owner = directory.owner;
                   local = { closed = [] };
                 }
           | Link _ -> Error (error C.Open_directory C.Link_like "link")
           | File _ ->
               Error (error C.Open_directory C.Not_directory "not-directory")))

  let duplicate_directory (directory : dir) =
    as_failure
      (let* () = check_dir C.Duplicate_directory directory in
       Ok
         {
           world = directory.world;
           id = directory.id;
           parent_id = directory.parent_id;
           captured_name = directory.captured_name;
           deletion_authority = directory.deletion_authority;
           owner = directory.owner;
           local = { closed = [] };
         })

  let open_file_no_follow (directory : dir) name =
    as_failure
      (let* () = check_dir C.Open_file directory in
       let* children = entries C.Open_file directory.world directory.id in
       match Hashtbl.find_opt children name with
       | None -> Error (error C.Open_file C.Missing "missing")
       | Some id -> (
           match (node directory.world id).kind with
           | File _ ->
               Ok
                 {
                   world = directory.world;
                   id;
                   parent = directory.id;
                   name;
                   deletion_authority = false;
                   owner = directory.owner;
                   local = { closed = [] };
                 }
           | Link _ -> Error (error C.Open_file C.Link_like "link")
           | Dir _ -> Error (error C.Open_file C.Not_regular "not-file")))

  let open_file_for_delete_no_follow (directory : dir) name =
    as_failure
      (let* () = check_dir C.Open_file directory in
       let* children = entries C.Open_file directory.world directory.id in
       match Hashtbl.find_opt children name with
       | None -> Error (error C.Open_file C.Missing "missing")
       | Some id -> (
           match (node directory.world id).kind with
           | File _ ->
               Ok
                 {
                   world = directory.world;
                   id;
                   parent = directory.id;
                   name;
                   deletion_authority = true;
                   owner = directory.owner;
                   local = { closed = [] };
                 }
           | Link _ -> Error (error C.Open_file C.Link_like "link")
           | Dir _ -> Error (error C.Open_file C.Not_regular "not-file")))

  let stat (target : node) : stat =
    let size =
      match target.kind with
      | File contents | Link contents -> Int64.of_int (String.length contents)
      | Dir _ -> 0L
    in
    {
      identity = target.id;
      kind = kind target.kind;
      size;
      permissions = target.permissions;
      mtime_ns = Int64.of_int target.id;
    }

  let read_captured (file : file) ~limit =
    let* () = check_file C.Read_file file in
    let target = node file.world file.id in
    match target.kind with
    | File contents ->
        if Int64.of_int (String.length contents) > limit then
          Error (error C.Read_file C.Too_large "limit")
        else Ok { contents; stat = stat target }
    | Dir _ | Link _ -> Error (error C.Read_file C.Not_regular "not-file")

  let read_link_no_follow (directory : dir) name =
    let* () = check_dir C.Read_link directory in
    let* children = entries C.Read_link directory.world directory.id in
    match Hashtbl.find_opt children name with
    | None -> Error (error C.Read_link C.Missing "missing")
    | Some id -> (
        match (node directory.world id).kind with
        | Link target -> Ok target
        | Dir _ | File _ -> Error (error C.Read_link C.Not_link "not-link"))

  let create_directory (directory : dir) name ~permissions =
    match check_dir C.Create_directory directory with
    | Error problem -> Not_created problem
    | Ok () -> (
        match entries C.Create_directory directory.world directory.id with
        | Error problem -> Not_created problem
        | Ok children ->
            if Hashtbl.mem children name then
              Not_created (error C.Create_directory C.Already_exists "exists")
            else
              let id =
                alloc directory.world permissions (Dir (Hashtbl.create 4))
              in
              Hashtbl.add children name id;
              Created
                {
                  world = directory.world;
                  id;
                  parent_id = Some directory.id;
                  captured_name = Some name;
                  deletion_authority = true;
                  owner = directory.owner;
                  local = { closed = [] };
                })

  let create_file (directory : dir) name ~permissions ~contents =
    match check_dir C.Create_file directory with
    | Error problem -> Not_created problem
    | Ok () -> (
        match directory.world.file_creation_fault with
        | Some `Before_commit ->
            directory.world.file_creation_fault <- None;
            Not_created
              (error C.Create_file C.Other "injected-create-file-before-commit")
        | Some `After_commit | None -> (
            match entries C.Create_file directory.world directory.id with
            | Error problem -> Not_created problem
            | Ok children -> (
                if Hashtbl.mem children name then
                  Not_created (error C.Create_file C.Already_exists "exists")
                else
                  let id = alloc directory.world permissions (File contents) in
                  Hashtbl.add children name id;
                  let file =
                    {
                      world = directory.world;
                      id;
                      parent = directory.id;
                      name;
                      deletion_authority = true;
                      owner = directory.owner;
                      local = { closed = [] };
                    }
                  in
                  match directory.world.file_creation_fault with
                  | Some `After_commit ->
                      directory.world.file_creation_fault <- None;
                      Creation_incomplete
                        {
                          residual = Captured file;
                          failure =
                            failure
                              (error C.Create_file C.Other
                                 "injected-create-file-after-commit");
                        }
                  | Some `Before_commit | None -> Created file)))

  let create_symlink (directory : dir) name ~target =
    let* () = check_dir C.Create_symlink directory in
    let* children = entries C.Create_symlink directory.world directory.id in
    if Hashtbl.mem children name then
      Error (error C.Create_symlink C.Already_exists "exists")
    else (
      Hashtbl.add children name
        (alloc directory.world (permissions 0o777) (Link target));
      Ok ())

  let chmod_directory (directory : dir) ~permissions =
    let* () = check_dir C.Chmod_directory directory in
    (node directory.world directory.id).permissions <- permissions;
    Ok ()

  let chmod_file (file : file) ~permissions =
    let* () = check_file C.Chmod_file file in
    (node file.world file.id).permissions <- permissions;
    Ok ()

  let atomic_rename (file : file) ~(into : dir) ~as_ ~replacement =
    match check_file C.Atomic_rename file with
    | Error problem -> C.Not_published problem
    | Ok () -> (
        match check_dir C.Atomic_rename into with
        | Error problem -> C.Not_published problem
        | Ok () when file.parent <> into.id ->
            C.Not_published
              (error C.Atomic_rename C.Unsupported
                 "cross-directory-publication-not-supported")
        | Ok () -> (
            match
              ( entries C.Atomic_rename file.world file.parent,
                entries C.Atomic_rename into.world into.id )
            with
            | Error problem, _ | _, Error problem -> C.Not_published problem
            | Ok source, Ok destination -> (
                match Hashtbl.find_opt source file.name with
                | Some id when id = file.id ->
                    if replacement = C.No_replace && Hashtbl.mem destination as_
                    then
                      C.Not_published
                        (error C.Atomic_rename C.Already_exists "exists")
                    else (
                      Hashtbl.remove source file.name;
                      Hashtbl.replace destination as_ file.id;
                      file.parent <- into.id;
                      file.name <- as_;
                      let advisories =
                        match file.world.advisory with
                        | None -> []
                        | Some advisory ->
                            file.world.advisory <- None;
                            [ advisory ]
                      in
                      C.Published { advisories })
                | Some _ | None ->
                    C.Not_published
                      (error C.Atomic_rename C.Missing "source-changed"))))

  let unlink_if_identity (directory : dir) name ~expected =
    let* () = check_dir C.Conditional_unlink directory in
    let* children = entries C.Conditional_unlink directory.world directory.id in
    match Hashtbl.find_opt children name with
    | None -> Ok C.Absent
    | Some id when id <> expected -> Ok C.Identity_changed
    | Some _ ->
        Hashtbl.remove children name;
        Ok C.Unlinked

  let cleanup_failure_with_namespace namespace_released
      (cleanup : C.cleanup_failure) =
    let update (problem : C.cleanup_problem) =
      { problem with C.namespace_released }
    in
    ({
       C.primary = update cleanup.primary;
       suppressed = List.map update cleanup.suppressed;
     }
      : C.cleanup_failure)

  let deletion_progress_of_cleanup namespace_released = function
    | C.Cleanup_complete | C.Cleanup_local_only ->
        ({ local_handle_state = C.Closed; namespace_released }
          : C.deletion_progress)
    | C.Cleanup_failed { primary; suppressed } ->
        let problems = primary :: suppressed in
        let local_handle_state =
          if
            List.exists
              (fun (problem : C.cleanup_problem) ->
                problem.local_handle_state = C.Still_open)
              problems
          then C.Still_open
          else if
            List.exists
              (fun (problem : C.cleanup_problem) ->
                problem.local_handle_state = C.Invalidated_unknown)
              problems
          then C.Invalidated_unknown
          else C.Closed
        in
        { local_handle_state; namespace_released }

  let committed_deletion_outcome world operation cleanup =
    let namespace_released = true in
    let progress = deletion_progress_of_cleanup namespace_released cleanup in
    let cleanup_issue =
      match cleanup with
      | C.Cleanup_complete | C.Cleanup_local_only -> None
      | C.Cleanup_failed failure ->
          Some
            (C.Cleanup_error
               (cleanup_failure_with_namespace namespace_released failure))
    in
    match world.deletion_fault with
    | Some `After_commit ->
        world.deletion_fault <- None;
        let primary =
          C.Operation_error
            (error operation C.Other "injected-delete-after-commit")
        in
        C.Deletion_incomplete
          {
            progress;
            failure = { C.primary; suppressed = Option.to_list cleanup_issue };
          }
    | Some `Before_commit | None -> (
        match cleanup_issue with
        | None -> C.Deletion_complete C.Unlinked
        | Some primary ->
            C.Deletion_incomplete
              { progress; failure = { C.primary; suppressed = [] } })

  let unlink_captured_file_if_identity ~(parent : dir) (file : file) ~expected =
    let operation = C.Conditional_unlink in
    match check_dir operation parent with
    | Error problem -> C.Deletion_not_committed problem
    | Ok () -> (
        match check_file operation file with
        | Error problem -> C.Deletion_not_committed problem
        | Ok () when not file.deletion_authority ->
            C.Deletion_not_committed
              (error operation C.Unsupported
                 "file-delete-authority-not-captured")
        | Ok () when file.parent <> parent.id ->
            C.Deletion_not_committed
              (error operation C.Unsupported
                 "captured-file-parent-identity-mismatch")
        | Ok () when file.id <> expected ->
            C.Deletion_complete C.Identity_changed
        | Ok () -> (
            match parent.world.deletion_fault with
            | Some `Before_commit ->
                parent.world.deletion_fault <- None;
                C.Deletion_not_committed
                  (error operation C.Other "injected-delete-before-commit")
            | Some `After_commit | None -> (
                match entries operation parent.world parent.id with
                | Error problem -> C.Deletion_not_committed problem
                | Ok children -> (
                    match Hashtbl.find_opt children file.name with
                    | Some identity when identity = file.id ->
                        Hashtbl.remove children file.name;
                        committed_deletion_outcome parent.world operation
                          (close C.Close_file file.world file.owner file.local)
                    | Some _ | None -> C.Deletion_complete C.Identity_changed)))
        )

  let unlink_link_no_follow (directory : dir) name ~expected =
    let* () = check_dir C.Unlink_link directory in
    let* children = entries C.Unlink_link directory.world directory.id in
    match Hashtbl.find_opt children name with
    | None -> Ok C.Absent
    | Some id when id <> expected -> Ok C.Identity_changed
    | Some id -> (
        match (node directory.world id).kind with
        | Link _ ->
            Hashtbl.remove children name;
            Ok C.Unlinked
        | Dir _ | File _ ->
            Error (error C.Unlink_link C.Not_link "entry-is-not-link"))

  let remove_empty_directory_if_identity (directory : dir) name ~expected =
    let* () = check_dir C.Remove_empty_directory directory in
    let* children =
      entries C.Remove_empty_directory directory.world directory.id
    in
    match Hashtbl.find_opt children name with
    | None -> Ok C.Absent
    | Some id when id <> expected -> Ok C.Identity_changed
    | Some id -> (
        match (node directory.world id).kind with
        | Dir nested when Hashtbl.length nested = 0 ->
            Hashtbl.remove children name;
            Ok C.Unlinked
        | Dir _ -> Error (error C.Remove_empty_directory C.Busy "not-empty")
        | File _ | Link _ ->
            Error (error C.Remove_empty_directory C.Not_directory "not-dir"))

  let remove_captured_empty_directory_if_identity ~(parent : dir) (target : dir)
      ~expected =
    let operation = C.Remove_empty_directory in
    match check_dir operation parent with
    | Error problem -> C.Deletion_not_committed problem
    | Ok () -> (
        match check_dir operation target with
        | Error problem -> C.Deletion_not_committed problem
        | Ok () when not target.deletion_authority ->
            C.Deletion_not_committed
              (error operation C.Unsupported
                 "directory-delete-authority-not-captured")
        | Ok ()
          when target.parent_id <> Some parent.id
               || Option.is_none target.captured_name ->
            C.Deletion_not_committed
              (error operation C.Unsupported
                 "captured-directory-parent-identity-mismatch")
        | Ok () when target.id <> expected ->
            C.Deletion_complete C.Identity_changed
        | Ok () -> (
            match parent.world.deletion_fault with
            | Some `Before_commit ->
                parent.world.deletion_fault <- None;
                C.Deletion_not_committed
                  (error operation C.Other "injected-delete-before-commit")
            | Some `After_commit | None -> (
                match entries operation parent.world parent.id with
                | Error problem -> C.Deletion_not_committed problem
                | Ok children -> (
                    let name = Option.get target.captured_name in
                    match Hashtbl.find_opt children name with
                    | Some identity when identity = target.id -> (
                        match (node target.world target.id).kind with
                        | Dir nested when Hashtbl.length nested = 0 ->
                            Hashtbl.remove children name;
                            committed_deletion_outcome parent.world operation
                              (close C.Close_directory target.world target.owner
                                 target.local)
                        | Dir _ ->
                            C.Deletion_not_committed
                              (error operation C.Busy "not-empty")
                        | File _ | Link _ ->
                            C.Deletion_not_committed
                              (error operation C.Not_directory "not-dir"))
                    | Some _ | None -> C.Deletion_complete C.Identity_changed)))
        )

  let try_lock (directory : dir) name mode =
    let* () = as_failure (check_dir C.Try_lock directory) in
    let key = (directory.id, name) in
    let held =
      match Hashtbl.find_opt directory.world.locks key with
      | Some held -> held
      | None ->
          let identity = directory.world.next in
          directory.world.next <- identity + 1;
          let held = { identity; shared = 0; exclusive = false } in
          Hashtbl.add directory.world.locks key held;
          held
    in
    let busy =
      match mode with
      | C.Shared -> held.exclusive
      | C.Exclusive -> held.exclusive || held.shared > 0
    in
    if busy then Ok `Busy
    else (
      (match mode with
      | C.Shared -> held.shared <- held.shared + 1
      | C.Exclusive -> held.exclusive <- true);
      Ok
        (`Acquired
           {
             world = directory.world;
             owner = directory.owner;
             key;
             file_identity = held.identity;
             mode;
             local = { closed = [] };
             released = false;
           }))

  let open_file_for_publish_no_follow = open_file_no_follow

  let close_probe (value : path_probe) =
    value.available <- false;
    close C.Close_probe value.world value.owner value.local

  let close_directory (value : dir) =
    close C.Close_directory value.world value.owner value.local

  let close_file (value : file) =
    close C.Close_file value.world value.owner value.local

  let close_separation (value : separation_witness) =
    value.materialization_available <- false;
    let candidate =
      close C.Close_separation value.world value.owner value.candidate_local
    in
    let forbidden =
      close C.Close_separation value.world value.owner value.forbidden_local
    in
    match (candidate, forbidden) with
    | C.Cleanup_failed failure, C.Cleanup_complete
    | C.Cleanup_complete, C.Cleanup_failed failure ->
        C.Cleanup_failed failure
    | C.Cleanup_failed first, C.Cleanup_failed second ->
        C.Cleanup_failed
          {
            first with
            suppressed = first.suppressed @ (second.primary :: second.suppressed);
          }
    | C.Cleanup_local_only, _ | _, C.Cleanup_local_only -> C.Cleanup_local_only
    | C.Cleanup_complete, C.Cleanup_complete -> C.Cleanup_complete

  let release_lock (value : lock) =
    if value.owner <> value.world.owner then
      close C.Release_lock value.world value.owner value.local
    else if value.released then C.Cleanup_complete
    else
      match Hashtbl.find_opt value.world.locks value.key with
      | None ->
          C.Cleanup_failed
            {
              primary =
                {
                  error = error C.Release_lock C.Other "lock-state-missing";
                  local_handle_state = C.Still_open;
                  namespace_released = false;
                };
              suppressed = [];
            }
      | Some held ->
          (match value.mode with
          | C.Shared -> held.shared <- held.shared - 1
          | C.Exclusive -> held.exclusive <- false);
          value.released <- true;
          value.local.closed <- value.owner :: value.local.closed;
          C.Cleanup_complete

  module Control = struct
    let reset () =
      let nodes = Hashtbl.create 24 in
      Hashtbl.add nodes 1
        {
          id = 1;
          kind = Dir (Hashtbl.create 12);
          permissions = permissions 0o755;
        };
      active :=
        Some
          {
            nodes;
            locks = Hashtbl.create 4;
            next = 2;
            owner = 1;
            advisory = None;
            close_faults = [];
            close_calls = 0;
            enumeration_claim = None;
            file_creation_fault = None;
            deletion_fault = None;
          }

    let components path =
      if String.equal path "/" then []
      else
        String.sub path 1 (String.length path - 1) |> String.split_on_char '/'

    let child world parent name =
      match (node world parent).kind with
      | Dir children -> Hashtbl.find_opt children name
      | File _ | Link _ -> None

    let rec resolve_from world current = function
      | [] -> current
      | name :: rest -> (
          match child world current name with
          | Some next -> resolve_from world next rest
          | None -> invalid_arg ("missing fake path: " ^ name))

    let resolve path = resolve_from (world ()) 1 (components path)

    let parent path =
      match List.rev (components path) with
      | [] -> invalid_arg "root has no parent"
      | name :: rest -> (resolve_from (world ()) 1 (List.rev rest), name)

    let bind path id =
      let parent, name = parent path in
      match (node (world ()) parent).kind with
      | Dir children -> Hashtbl.replace children name id
      | File _ | Link _ -> invalid_arg "fake parent is not a directory"

    let mkdir path =
      let world = world () in
      let rec create current = function
        | [] -> current
        | name :: rest ->
            let children =
              match (node world current).kind with
              | Dir children -> children
              | File _ | Link _ -> invalid_arg "fake non-directory"
            in
            let next =
              match Hashtbl.find_opt children name with
              | Some next -> next
              | None ->
                  let next =
                    alloc world (permissions 0o755) (Dir (Hashtbl.create 4))
                  in
                  Hashtbl.add children name next;
                  next
            in
            create next rest
      in
      create 1 (components path)

    let put_file path contents =
      let world = world () in
      let parent_components =
        match List.rev (components path) with
        | [] -> invalid_arg "cannot write root"
        | _ :: rest -> List.rev rest
      in
      ignore (mkdir ("/" ^ String.concat "/" parent_components));
      let id = alloc world (permissions 0o600) (File contents) in
      bind path id;
      id

    let swap_to_link path target =
      let id = alloc (world ()) (permissions 0o777) (Link target) in
      bind path id;
      id

    let swap_to_directory path source =
      let world = world () in
      let id = resolve source in
      match (node world id).kind with
      | Dir _ ->
          bind path id;
          id
      | File _ | Link _ -> invalid_arg "fake replacement is not a directory"

    let exists path =
      try
        ignore (resolve path);
        true
      with Invalid_argument _ -> false

    let content path =
      try
        match (node (world ()) (resolve path)).kind with
        | File contents -> Some contents
        | Dir _ | Link _ -> None
      with Invalid_argument _ -> None

    let content_under directory names =
      try
        match
          (node (world ()) (resolve_from (world ()) directory names)).kind
        with
        | File contents -> Some contents
        | Dir _ | Link _ -> None
      with Invalid_argument _ -> None

    let set_owner owner = (world ()).owner <- owner
    let set_advisory advisory = (world ()).advisory <- Some advisory
    let fail_next_close state = (world ()).close_faults <- [ state ]
    let fail_closes states = (world ()).close_faults <- states
    let close_calls () = (world ()).close_calls

    let fail_next_file_creation_before_commit () =
      (world ()).file_creation_fault <- Some `Before_commit

    let fail_next_file_creation_after_commit () =
      (world ()).file_creation_fault <- Some `After_commit

    let fail_next_deletion_before_commit () =
      (world ()).deletion_fault <- Some `Before_commit

    let fail_next_deletion_after_commit () =
      (world ()).deletion_fault <- Some `After_commit

    let claim_enumeration ~names ~entries ~native_name_bytes =
      (world ()).enumeration_claim <- Some (names, entries, native_name_bytes)
  end
end

module _ : C.S = Fake

let get_ok = function
  | Ok value -> value
  | Error (error : C.error) ->
      Alcotest.failf "%s failed: %s"
        (C.operation_name error.operation)
        error.native_code

let get_ok_failure = function
  | Ok value -> value
  | Error (failure : C.failure) ->
      let error = C.issue_error failure.primary in
      Alcotest.failf "%s failed: %s"
        (C.operation_name error.operation)
        error.native_code

let get_created = function
  | Fake.Created value -> value
  | Fake.Not_created error ->
      Alcotest.failf "%s failed before commit: %s"
        (C.operation_name error.operation)
        error.native_code
  | Fake.Creation_incomplete { failure; _ } ->
      let error = C.issue_error failure.primary in
      Alcotest.failf "%s failed after commit: %s"
        (C.operation_name error.operation)
        error.native_code

let name value = get_ok (Fake.name_of_component value)
let probe path = get_ok_failure (Fake.probe_path path)

let permissions value =
  match C.Permissions.of_int value with
  | Ok permissions -> permissions
  | Error _ -> Alcotest.failf "invalid test permission literal: %o" value

let enumeration_budget ~entries ~bytes =
  match
    Fake.enumeration_budget ~max_entries:entries ~max_native_name_bytes:bytes
  with
  | Ok budget -> budget
  | Error _ -> Alcotest.fail "invalid test enumeration budget"

let enumerated = function
  | Fake.Enumerated { names; consumption } -> (names, consumption)
  | Fake.Enumeration_incomplete { failure; _ } ->
      let error = C.issue_error failure.primary in
      Alcotest.failf "enumeration failed: %s" error.native_code

let directory path =
  ignore (Fake.Control.mkdir "/.dir-cap-forbidden");
  let witness =
    get_ok_failure
      (Fake.establish_separation
         ~forbidden:(probe "/.dir-cap-forbidden")
         ~candidate:(probe path))
  in
  match Fake.materialize witness ~permissions:(permissions 0o700) with
  | Fake.Materialized materialized -> materialized.directory
  | Fake.Materialization_incomplete { failure; _ } ->
      let error = C.issue_error failure.primary in
      Alcotest.failf "%s failed: %s"
        (C.operation_name error.operation)
        error.native_code

let expect_error expected = function
  | Error (error : C.error) ->
      Alcotest.(check bool) "error class" true (error.class_ = expected)
  | Ok _ -> Alcotest.fail "operation unexpectedly succeeded"

let expect_failure expected = function
  | Error (failure : C.failure) ->
      let error = C.issue_error failure.primary in
      Alcotest.(check bool) "error class" true (error.class_ = expected)
  | Ok _ -> Alcotest.fail "operation unexpectedly succeeded"

let failure_error (failure : C.failure) = C.issue_error failure.primary

let test_capture_and_native_names () =
  Fake.Control.reset ();
  let captured_identity = Fake.Control.mkdir "/cache" in
  ignore (Fake.Control.mkdir "/outside");
  ignore (Fake.Control.put_file "/cache/original" "captured");
  let cache = directory "/cache" in
  let original =
    get_ok_failure (Fake.open_file_no_follow cache (name "original"))
  in
  ignore (Fake.Control.put_file "/cache/original" "replacement");
  let read = get_ok (Fake.read_captured original ~limit:64L) in
  Alcotest.(check string) "captured file" "captured" read.contents;
  Alcotest.(check string)
    "stable identity encoding"
    (Fake.Identity.encode read.stat.identity)
    (Fake.Identity.encode read.stat.identity);
  ignore (Fake.Control.swap_to_link "/cache" "/outside");
  let raw = "report with spaces 雪.json" in
  let native = name raw in
  Alcotest.(check string)
    "lossless native name" raw
    (Fake.Native_name.encode native);
  ignore
    (get_created
       (Fake.create_file cache native ~permissions:(permissions 0o600)
          ~contents:"owned"));
  Alcotest.(check (option string))
    "write remains below captured handle" (Some "owned")
    (Fake.Control.content_under captured_identity [ raw ]);
  Alcotest.(check bool)
    "link target not traversed" false
    (Fake.Control.exists ("/outside/" ^ raw));
  let names, consumption =
    enumerated
      (Fake.enumerate_no_follow cache
         ~budget:(enumeration_budget ~entries:16L ~bytes:4096L))
  in
  Alcotest.(check (list string))
    "enumeration preserves arbitrary native name" [ "original"; raw ]
    (List.map Fake.Native_name.encode names);
  Alcotest.(check int64) "exact entry consumption" 2L consumption.entries

let test_create_file_commit_boundary_and_exact_bytes () =
  Fake.Control.reset ();
  ignore (Fake.Control.mkdir "/cache");
  ignore (Fake.Control.mkdir "/outside");
  ignore (Fake.Control.put_file "/cache/existing" "foreign");
  ignore (Fake.Control.put_file "/outside/sentinel" "unchanged");
  ignore (Fake.Control.swap_to_link "/cache/raced-link" "/outside");
  let cache = directory "/cache" in
  let create filename contents =
    Fake.create_file cache (name filename) ~permissions:(permissions 0o600)
      ~contents
  in
  (match create "existing" "replacement" with
  | Fake.Not_created error ->
      Alcotest.(check bool)
        "collision is pre-commit" true
        (error.class_ = C.Already_exists)
  | Fake.Created file ->
      ignore (Fake.close_file file);
      Alcotest.fail "existing file was replaced"
  | Fake.Creation_incomplete _ ->
      Alcotest.fail "collision reported a possible commit");
  Alcotest.(check (option string))
    "collision preserves bytes" (Some "foreign")
    (Fake.Control.content "/cache/existing");
  (match create "raced-link" "replacement" with
  | Fake.Not_created error ->
      Alcotest.(check bool)
        "link collision is not traversed" true
        (error.class_ = C.Already_exists)
  | Fake.Created file ->
      ignore (Fake.close_file file);
      Alcotest.fail "link was traversed or replaced"
  | Fake.Creation_incomplete _ ->
      Alcotest.fail "link collision reported a possible commit");
  Alcotest.(check (option string))
    "link target remains unchanged" (Some "unchanged")
    (Fake.Control.content "/outside/sentinel");

  Fake.Control.fail_next_file_creation_before_commit ();
  (match create "before" "must-not-exist" with
  | Fake.Not_created error ->
      Alcotest.(check string)
        "pre-commit fault" "injected-create-file-before-commit"
        error.native_code
  | Fake.Created file ->
      ignore (Fake.close_file file);
      Alcotest.fail "pre-commit fault created a file"
  | Fake.Creation_incomplete _ ->
      Alcotest.fail "pre-commit fault became commit-uncertain");
  Alcotest.(check bool)
    "pre-commit namespace unchanged" false
    (Fake.Control.exists "/cache/before");

  Fake.Control.fail_next_file_creation_after_commit ();
  (match create "existing" "still-foreign" with
  | Fake.Not_created error ->
      Alcotest.(check bool)
        "collision wins before post-commit site" true
        (error.class_ = C.Already_exists)
  | Fake.Created file ->
      ignore (Fake.close_file file);
      Alcotest.fail "fault setup collision replaced a file"
  | Fake.Creation_incomplete _ ->
      Alcotest.fail "fault setup collision reached post-commit");
  let committed = "head\000snow-雪-tail" in
  (match create "after" committed with
  | Fake.Creation_incomplete { residual = Fake.Captured file; failure } ->
      Alcotest.(check string)
        "post-commit primary" "injected-create-file-after-commit"
        (failure_error failure).native_code;
      Alcotest.(check int)
        "live residual needs no hidden cleanup" 0
        (List.length failure.suppressed);
      Alcotest.(check string)
        "captured residual has exact committed bytes" committed
        (get_ok (Fake.read_captured file ~limit:64L)).contents;
      Alcotest.(check bool)
        "captured residual closes truthfully" true
        (Fake.close_file file = C.Cleanup_complete)
  | Fake.Creation_incomplete { residual = Fake.Uncaptured _; _ } ->
      Alcotest.fail "Fake discarded live post-commit authority"
  | Fake.Not_created _ ->
      Alcotest.fail "post-commit fault was reported not-created"
  | Fake.Created file ->
      ignore (Fake.close_file file);
      Alcotest.fail "post-commit fault was hidden");
  let ordinary = "exact\000bytes" in
  let file = get_created (create "ordinary" ordinary) in
  Alcotest.(check string)
    "ordinary create writes exact bytes" ordinary
    (get_ok (Fake.read_captured file ~limit:64L)).contents;
  Alcotest.(check bool)
    "ordinary created cap closes" true
    (Fake.close_file file = C.Cleanup_complete)

let test_captured_deletion_commit_and_cleanup_evidence () =
  Fake.Control.reset ();
  ignore (Fake.Control.mkdir "/cache");
  ignore (Fake.Control.mkdir "/other");
  ignore (Fake.Control.mkdir "/outside");
  let cache = directory "/cache" in
  let other = directory "/other" in
  let open_file filename contents =
    ignore (Fake.Control.put_file ("/cache/" ^ filename) contents);
    get_ok_failure (Fake.open_file_for_delete_no_follow cache (name filename))
  in
  let ordinary_name = "ordinary" in
  ignore (Fake.Control.put_file ("/cache/" ^ ordinary_name) "ordinary");
  let ordinary =
    get_ok_failure (Fake.open_file_no_follow cache (name ordinary_name))
  in
  (match
     Fake.unlink_captured_file_if_identity ~parent:cache ordinary
       ~expected:(Fake.file_identity ordinary)
   with
  | C.Deletion_not_committed error ->
      Alcotest.(check string)
        "ordinary capture has no delete authority"
        "file-delete-authority-not-captured" error.native_code
  | C.Deletion_complete _ | C.Deletion_incomplete _ ->
      Alcotest.fail "ordinary capture authorized deletion");
  Alcotest.(check string)
    "rejected ordinary capture remains live" "ordinary"
    (get_ok (Fake.read_captured ordinary ~limit:64L)).contents;
  ignore (Fake.close_file ordinary);

  let file = open_file "target" "owned" in
  let expected = Fake.file_identity file in
  (match Fake.unlink_captured_file_if_identity ~parent:other file ~expected with
  | C.Deletion_not_committed error ->
      Alcotest.(check string)
        "wrong parent is pre-commit" "captured-file-parent-identity-mismatch"
        error.native_code
  | C.Deletion_complete _ | C.Deletion_incomplete _ ->
      Alcotest.fail "wrong parent authorized deletion");
  (match
     Fake.unlink_captured_file_if_identity ~parent:cache file
       ~expected:(Fake.dir_identity other)
   with
  | C.Deletion_complete C.Identity_changed -> ()
  | C.Deletion_not_committed _ | C.Deletion_complete _ | C.Deletion_incomplete _
    ->
      Alcotest.fail "wrong target identity was not rejected deterministically");
  Alcotest.(check string)
    "identity refusals leave target live" "owned"
    (get_ok (Fake.read_captured file ~limit:64L)).contents;
  (match Fake.unlink_captured_file_if_identity ~parent:cache file ~expected with
  | C.Deletion_complete C.Unlinked -> ()
  | C.Deletion_not_committed _ | C.Deletion_complete _ | C.Deletion_incomplete _
    ->
      Alcotest.fail "captured file was not deleted");
  Alcotest.(check bool)
    "successful deletion releases namespace" false
    (Fake.Control.exists "/cache/target");
  expect_error C.Closed_capability (Fake.read_captured file ~limit:64L);

  let raced = open_file "raced" "pinned" in
  let raced_identity = Fake.file_identity raced in
  ignore (Fake.Control.swap_to_link "/cache/raced" "/outside");
  (match
     Fake.unlink_captured_file_if_identity ~parent:cache raced
       ~expected:raced_identity
   with
  | C.Deletion_complete C.Identity_changed -> ()
  | C.Deletion_not_committed _ | C.Deletion_complete _ | C.Deletion_incomplete _
    ->
      Alcotest.fail "replacement link was not retained");
  Alcotest.(check bool)
    "raced link binding remains" true
    (Fake.Control.exists "/cache/raced");
  ignore (Fake.close_file raced);

  let before = open_file "before" "retryable" in
  Fake.Control.fail_next_deletion_before_commit ();
  (match
     Fake.unlink_captured_file_if_identity ~parent:cache before
       ~expected:(Fake.file_identity before)
   with
  | C.Deletion_not_committed error ->
      Alcotest.(check string)
        "pre-commit fault is exact" "injected-delete-before-commit"
        error.native_code
  | C.Deletion_complete _ | C.Deletion_incomplete _ ->
      Alcotest.fail "pre-commit deletion fault crossed the commit boundary");
  Alcotest.(check string)
    "pre-commit target remains live" "retryable"
    (get_ok (Fake.read_captured before ~limit:64L)).contents;
  (match
     Fake.unlink_captured_file_if_identity ~parent:cache before
       ~expected:(Fake.file_identity before)
   with
  | C.Deletion_complete C.Unlinked -> ()
  | C.Deletion_not_committed _ | C.Deletion_complete _ | C.Deletion_incomplete _
    ->
      Alcotest.fail "pre-commit capability could not be retried");

  let after = open_file "after" "committed" in
  Fake.Control.fail_next_deletion_after_commit ();
  (match
     Fake.unlink_captured_file_if_identity ~parent:cache after
       ~expected:(Fake.file_identity after)
   with
  | C.Deletion_incomplete
      {
        progress = { local_handle_state = C.Closed; namespace_released = true };
        failure = { primary = C.Operation_error action; suppressed = [] };
      } ->
      Alcotest.(check string)
        "post-commit action is exact" "injected-delete-after-commit"
        action.native_code
  | C.Deletion_not_committed _ | C.Deletion_complete _ | C.Deletion_incomplete _
    ->
      Alcotest.fail "post-commit deletion evidence was not exact");
  Alcotest.(check bool)
    "post-commit path is gone" false
    (Fake.Control.exists "/cache/after");
  expect_error C.Closed_capability (Fake.read_captured after ~limit:64L);

  let double_fault = open_file "double-fault" "committed" in
  Fake.Control.fail_next_close C.Invalidated_unknown;
  Fake.Control.fail_next_deletion_after_commit ();
  (match
     Fake.unlink_captured_file_if_identity ~parent:cache double_fault
       ~expected:(Fake.file_identity double_fault)
   with
  | C.Deletion_incomplete
      {
        progress =
          {
            local_handle_state = C.Invalidated_unknown;
            namespace_released = true;
          };
        failure =
          {
            primary = C.Operation_error action;
            suppressed =
              [ C.Cleanup_error { primary = cleanup; suppressed = [] } ];
          };
      } ->
      Alcotest.(check bool)
        "action then cleanup remain ordered" true
        (action.native_code = "injected-delete-after-commit"
        && cleanup.error.native_code = "injected-close-failure"
        && cleanup.local_handle_state = C.Invalidated_unknown
        && cleanup.namespace_released)
  | C.Deletion_not_committed _ | C.Deletion_complete _ | C.Deletion_incomplete _
    ->
      Alcotest.fail "post-commit action and cleanup failures were flattened");
  Alcotest.(check bool)
    "double-fault namespace is released" false
    (Fake.Control.exists "/cache/double-fault");

  let cleanup_fault = open_file "cleanup-fault" "still-readable" in
  Fake.Control.fail_next_close C.Still_open;
  (match
     Fake.unlink_captured_file_if_identity ~parent:cache cleanup_fault
       ~expected:(Fake.file_identity cleanup_fault)
   with
  | C.Deletion_incomplete
      {
        progress =
          { local_handle_state = C.Still_open; namespace_released = true };
        failure =
          {
            primary = C.Cleanup_error { primary = cleanup; suppressed = [] };
            suppressed = [];
          };
      } ->
      Alcotest.(check bool)
        "cleanup-primary evidence is exact" true
        (cleanup.local_handle_state = C.Still_open && cleanup.namespace_released)
  | C.Deletion_not_committed _ | C.Deletion_complete _ | C.Deletion_incomplete _
    ->
      Alcotest.fail "cleanup-primary deletion evidence was not exact");
  Alcotest.(check string)
    "retryable local handle remains readable" "still-readable"
    (get_ok (Fake.read_captured cleanup_fault ~limit:64L)).contents;
  Alcotest.(check bool)
    "retryable deletion close succeeds" true
    (Fake.close_file cleanup_fault = C.Cleanup_complete);

  ignore (Fake.Control.mkdir "/cache/empty");
  let empty =
    get_ok_failure
      (Fake.open_directory_for_delete_no_follow cache (name "empty"))
  in
  (match
     Fake.remove_captured_empty_directory_if_identity ~parent:cache empty
       ~expected:(Fake.dir_identity empty)
   with
  | C.Deletion_complete C.Unlinked -> ()
  | C.Deletion_not_committed _ | C.Deletion_complete _ | C.Deletion_incomplete _
    ->
      Alcotest.fail "captured empty directory was not removed");
  Alcotest.(check bool)
    "empty-directory namespace released" false
    (Fake.Control.exists "/cache/empty");

  ignore (Fake.Control.mkdir "/cache/nonempty");
  ignore (Fake.Control.put_file "/cache/nonempty/child" "child");
  let nonempty =
    get_ok_failure
      (Fake.open_directory_for_delete_no_follow cache (name "nonempty"))
  in
  (match
     Fake.remove_captured_empty_directory_if_identity ~parent:cache nonempty
       ~expected:(Fake.dir_identity nonempty)
   with
  | C.Deletion_not_committed error ->
      Alcotest.(check bool)
        "nonempty directory is pre-commit Busy" true (error.class_ = C.Busy)
  | C.Deletion_complete _ | C.Deletion_incomplete _ ->
      Alcotest.fail "nonempty directory crossed deletion commit");
  Alcotest.(check bool)
    "nonempty target remains" true
    (Fake.Control.exists "/cache/nonempty/child");
  ignore (Fake.close_directory nonempty);
  ignore (Fake.close_directory other);
  ignore (Fake.close_directory cache)

let test_changed_marker_is_retained () =
  Fake.Control.reset ();
  ignore (Fake.Control.mkdir "/cache");
  let cache = directory "/cache" in
  let marker_name = name "run.reserved" in
  let marker =
    get_created
      (Fake.create_file cache marker_name ~permissions:(permissions 0o600)
         ~contents:"owned")
  in
  let expected =
    (get_ok (Fake.read_captured marker ~limit:64L)).stat.identity
  in
  ignore (Fake.Control.put_file "/cache/run.reserved" "foreign");
  let outcome = get_ok (Fake.unlink_if_identity cache marker_name ~expected) in
  Alcotest.(check bool)
    "conditional unlink refuses replacement" true
    (outcome = C.Identity_changed);
  Alcotest.(check (option string))
    "replacement remains" (Some "foreign")
    (Fake.Control.content "/cache/run.reserved")

let test_link_cleanup_is_direct () =
  Fake.Control.reset ();
  ignore (Fake.Control.mkdir "/cache");
  ignore (Fake.Control.mkdir "/outside");
  ignore (Fake.Control.put_file "/outside/sentinel" "unchanged");
  let cache = directory "/cache" in
  let link = name "shortcut" in
  ignore (get_ok (Fake.create_symlink cache link ~target:"/outside"));
  expect_failure C.Link_like (Fake.open_directory_no_follow cache link);
  Alcotest.(check string)
    "target read without following" "/outside"
    (get_ok (Fake.read_link_no_follow cache link));
  Alcotest.(check bool)
    "link entry removed" true
    (get_ok
       (Fake.unlink_link_no_follow cache link
          ~expected:
            (get_ok_failure (Fake.probe_entry_no_follow cache link)
            |> Option.get)
              .identity)
    = C.Unlinked);
  Alcotest.(check bool)
    "entry absent" false
    (Fake.Control.exists "/cache/shortcut");
  Alcotest.(check (option string))
    "target untouched" (Some "unchanged")
    (Fake.Control.content "/outside/sentinel")

let test_published_rename_retains_advisory () =
  Fake.Control.reset ();
  ignore (Fake.Control.mkdir "/cache");
  let cache = directory "/cache" in
  let staged =
    get_created
      (Fake.create_file cache (name "report.pending")
         ~permissions:(permissions 0o600) ~contents:"final")
  in
  Fake.Control.set_advisory
    (C.make_error ~operation:C.Atomic_rename ~class_:C.Other
       ~native_domain:C.In_memory ~native_code:"directory-flush-failed" ());
  (match
     Fake.atomic_rename staged ~into:cache ~as_:(name "report.json")
       ~replacement:C.Replace
   with
  | C.Not_published error ->
      Alcotest.failf "rename was not published: %s" error.native_code
  | C.Published { advisories } ->
      Alcotest.(check (list string))
        "advisory is post-commit evidence"
        [ "directory-flush-failed" ]
        (List.map (fun (error : C.error) -> error.native_code) advisories));
  Alcotest.(check (option string))
    "destination committed" (Some "final")
    (Fake.Control.content "/cache/report.json");
  Alcotest.(check bool)
    "source name removed" false
    (Fake.Control.exists "/cache/report.pending")

let acquired = function
  | Ok (`Acquired lock) -> lock
  | Ok `Busy -> Alcotest.fail "lock unexpectedly busy"
  | Error (failure : C.failure) ->
      Alcotest.failf "lock: %s" (C.issue_error failure.primary).native_code

let test_foreign_owner_cleanup_is_local () =
  Fake.Control.reset ();
  ignore (Fake.Control.mkdir "/cache");
  let cache = directory "/cache" in
  let file =
    get_created
      (Fake.create_file cache (name "owned") ~permissions:(permissions 0o600)
         ~contents:"parent")
  in
  let lock = acquired (Fake.try_lock cache (name "lease.lock") C.Shared) in
  Alcotest.(check int)
    "fake lock retains directory identity" (Fake.dir_identity cache)
    (Fake.lock_directory_identity lock);
  Alcotest.(check bool)
    "fake lock retains a distinct file identity" true
    (Fake.lock_file_identity lock <> Fake.dir_identity cache);
  Fake.Control.set_owner 2;
  expect_error C.Wrong_process (Fake.read_captured file ~limit:64L);
  Alcotest.(check bool)
    "child closes local file handle" true
    (Fake.close_file file = C.Cleanup_local_only);
  Alcotest.(check bool)
    "child leaves parent lock namespace alone" true
    (Fake.release_lock lock = C.Cleanup_local_only);
  Fake.Control.set_owner 1;
  Alcotest.(check string)
    "parent handle remains usable" "parent"
    (get_ok (Fake.read_captured file ~limit:64L)).contents;
  (match Fake.try_lock cache (name "lease.lock") C.Exclusive with
  | Ok `Busy -> ()
  | Ok (`Acquired _) -> Alcotest.fail "child release unlocked parent lock"
  | Error failure ->
      Alcotest.failf "lock probe: %s"
        (C.issue_error failure.primary).native_code);
  Alcotest.(check bool)
    "parent releases lock" true
    (Fake.release_lock lock = C.Cleanup_complete)

let test_enumeration_budget_is_exact () =
  Fake.Control.reset ();
  ignore (Fake.Control.mkdir "/cache");
  ignore (Fake.Control.put_file "/cache/a" "a");
  ignore (Fake.Control.put_file "/cache/bb" "bb");
  let cache = directory "/cache" in
  let budget = enumeration_budget ~entries:1L ~bytes:32L in
  match Fake.enumerate_no_follow cache ~budget with
  | Fake.Enumerated _ -> Alcotest.fail "bounded enumeration was not stopped"
  | Fake.Enumeration_incomplete { consumption; failure } ->
      Alcotest.(check int64) "accepted entries" 1L consumption.entries;
      Alcotest.(check int64)
        "accepted native bytes" 1L consumption.native_name_bytes;
      Alcotest.(check bool)
        "typed budget failure" true
        ((failure_error failure).class_ = C.Too_large)

let test_separation_rechecks_raced_component () =
  Fake.Control.reset ();
  let forbidden_id = Fake.Control.mkdir "/workspace" in
  ignore (Fake.Control.mkdir "/safe");
  let witness =
    get_ok_failure
      (Fake.establish_separation ~forbidden:(probe "/workspace")
         ~candidate:(probe "/safe/cache"))
  in
  Fake.Control.bind "/safe/cache" forbidden_id;
  (match Fake.materialize witness ~permissions:(permissions 0o700) with
  | Fake.Materialized _ ->
      Alcotest.fail "raced component entered the forbidden identity chain"
  | Fake.Materialization_incomplete { created; failure } ->
      Alcotest.(check int) "no claimed creation" 0 (List.length created);
      Alcotest.(check bool)
        "fail-closed separation" true
        ((failure_error failure).class_ = C.Unsupported));
  match Fake.materialize witness ~permissions:(permissions 0o700) with
  | Fake.Materialized _ -> Alcotest.fail "one-shot witness was reused"
  | Fake.Materialization_incomplete { failure; _ } ->
      Alcotest.(check bool)
        "materialization is one-shot" true
        ((failure_error failure).class_ = C.Closed_capability)

let test_creation_evidence_has_no_persisted_identity () =
  Fake.Control.reset ();
  ignore (Fake.Control.mkdir "/workspace");
  ignore (Fake.Control.mkdir "/safe");
  let witness =
    get_ok_failure
      (Fake.establish_separation ~forbidden:(probe "/workspace")
         ~candidate:(probe "/safe/one/two"))
  in
  match Fake.materialize witness ~permissions:(permissions 0o700) with
  | Fake.Materialization_incomplete { failure; _ } ->
      Alcotest.failf "materialization failed: %s"
        (failure_error failure).native_code
  | Fake.Materialized { created; _ } ->
      let names =
        List.map
          (function
            | Fake.Creation_observed name
            | Fake.Creation_may_have_committed name ->
                Fake.Native_name.encode name)
          created
      in
      Alcotest.(check (list string))
        "ordered audit-only evidence" [ "one"; "two" ] names

let test_materialized_root_uses_exact_marker_capability () =
  Fake.Control.reset ();
  ignore (Fake.Control.mkdir "/workspace");
  ignore (Fake.Control.mkdir "/safe");
  let witness =
    get_ok_failure
      (Fake.establish_separation ~forbidden:(probe "/workspace")
         ~candidate:(probe "/safe/cache"))
  in
  let root =
    match Fake.materialize witness ~permissions:(permissions 0o700) with
    | Fake.Materialization_incomplete { failure; _ } ->
        Alcotest.failf "materialize root: %s"
          (failure_error failure).native_code
    | Fake.Materialized { directory; disposition; created; advisories } ->
        Alcotest.(check bool)
          "new root disposition" true
          (disposition = C.Newly_created);
        Alcotest.(check int)
          "materialization cleanup is clean" 0 (List.length advisories);
        (match created with
        | [ Fake.Creation_observed created_name ] ->
            Alcotest.(check string)
              "root evidence" "cache"
              (Fake.Native_name.encode created_name)
        | _ -> Alcotest.fail "new root lost exact observed evidence");
        directory
  in
  let marker =
    get_created
      (Fake.create_file root
         (name "reservation.marker")
         ~permissions:C.Permissions.owner_read_write ~contents:"owner-v2")
  in
  (match
     Fake.unlink_captured_file_if_identity ~parent:root marker
       ~expected:(Fake.file_identity marker)
   with
  | C.Deletion_complete C.Unlinked -> ()
  | C.Deletion_not_committed error ->
      Alcotest.failf "delete marker: %s" error.native_code
  | C.Deletion_complete _ ->
      Alcotest.fail "exact marker capability did not unlink its entry"
  | C.Deletion_incomplete { failure; _ } ->
      Alcotest.failf "delete marker incomplete: %s"
        (failure_error failure).native_code);
  Alcotest.(check bool)
    "marker namespace released" false
    (Fake.Control.exists "/safe/cache/reservation.marker");
  Alcotest.(check bool)
    "root closes cleanly" true
    (Fake.close_directory root = C.Cleanup_complete)

let test_duplicate_and_typed_close_progress () =
  Fake.Control.reset ();
  ignore (Fake.Control.mkdir "/cache");
  let cache = directory "/cache" in
  let duplicate = get_ok_failure (Fake.duplicate_directory cache) in
  Fake.Control.fail_next_close C.Still_open;
  (match Fake.close_directory duplicate with
  | C.Cleanup_failed { primary = { local_handle_state = C.Still_open; _ }; _ }
    ->
      ()
  | _ -> Alcotest.fail "retryable close did not retain Still_open authority");
  let budget = enumeration_budget ~entries:1L ~bytes:1L in
  (match Fake.enumerate_no_follow duplicate ~budget with
  | Fake.Enumerated _ -> ()
  | Fake.Enumeration_incomplete { failure; _ } ->
      Alcotest.failf "Still_open duplicate was not usable: %s"
        (failure_error failure).native_code);
  Alcotest.(check bool)
    "retry closes exact duplicate" true
    (Fake.close_directory duplicate = C.Cleanup_complete);
  let independent = get_ok_failure (Fake.duplicate_directory cache) in
  ignore (Fake.close_directory independent);
  let listing = Fake.enumerate_no_follow cache ~budget in
  (match listing with
  | Fake.Enumerated _ -> ()
  | Fake.Enumeration_incomplete { failure; _ } ->
      Alcotest.failf "closing duplicate closed original: %s"
        (failure_error failure).native_code);
  Fake.Control.fail_next_close C.Invalidated_unknown;
  (match Fake.close_directory cache with
  | C.Cleanup_failed
      { primary = { local_handle_state = C.Invalidated_unknown; _ }; _ } ->
      ()
  | _ -> Alcotest.fail "ambiguous close was not terminally invalidated");
  match Fake.enumerate_no_follow cache ~budget with
  | Fake.Enumeration_incomplete { failure; _ } ->
      Alcotest.(check bool)
        "invalidated capability is closed" true
        ((failure_error failure).class_ = C.Closed_capability)
  | Fake.Enumerated _ -> Alcotest.fail "invalidated capability remained usable"

let test_permission_validation () =
  (match C.Permissions.of_int (-1) with
  | Error (C.Permission_out_of_range -1) -> ()
  | _ -> Alcotest.fail "negative permission accepted");
  (match C.Permissions.of_int 0o10000 with
  | Error (C.Permission_out_of_range 0o10000) -> ()
  | _ -> Alcotest.fail "oversized permission accepted");
  Alcotest.(check int)
    "permission round-trip" 0o750
    (C.Permissions.to_int (permissions 0o750))

let test_cleanup_order () =
  let observed = ref [] in
  let step number result () =
    observed := number :: !observed;
    result
  in
  let problem code operation =
    C.make_error ~operation ~class_:C.Other ~native_domain:C.In_memory
      ~native_code:code ()
  in
  let first = problem "first" C.Close_file in
  let second = problem "second" C.Release_lock in
  let failed error =
    C.Cleanup_failed
      {
        primary =
          { error; local_handle_state = C.Closed; namespace_released = false };
        suppressed = [];
      }
  in
  let results =
    C.run_cleanup_in_order
      [
        step 1 C.Cleanup_complete; step 2 (failed first); step 3 (failed second);
      ]
  in
  Alcotest.(check (list int)) "head-to-tail" [ 1; 2; 3 ] (List.rev !observed);
  match C.resolve_cleanup (Ok ()) results with
  | Ok () -> Alcotest.fail "cleanup errors were discarded"
  | Error failure ->
      Alcotest.(check string)
        "primary" "first" (C.issue_error failure.primary).native_code;
      Alcotest.(check (list string))
        "suppressed order" [ "second" ]
        (List.map
           (fun issue -> (C.issue_error issue).native_code)
           failure.suppressed)

module Cache = Engine.Cache_fs

module Fake_gc = struct
  type dir = Fake.dir
  type identity = Fake.Identity.t
  type owner = Fake.owner
  type binding = { root : dir; identity : identity; owner : owner }

  type t = {
    mutable active : bool;
    mutable continuity_broken : bool;
    mutable binding : binding option;
  }

  let reject code =
    Error
      (C.failure_of_error
         (C.make_error ~operation:C.Probe_entry ~class_:C.Unsupported
            ~native_domain:C.In_memory ~native_code:code ()))

  let root_is_live (root : dir) =
    Fake.owner_equal root.owner root.world.owner
    && not (List.mem root.world.owner root.local.closed)

  let binding_matches binding (root : dir) identity owner =
    binding.root == root
    && Fake.Identity.equal binding.identity identity
    && Fake.owner_equal binding.owner owner
    && Fake.Identity.equal identity (Fake.dir_identity root)
    && Fake.owner_equal owner (Fake.dir_owner root)
    && root_is_live root

  let establish token ~root ~identity ~owner =
    if (not token.active) || token.continuity_broken then
      reject "gc-lease-not-continuous"
    else
      match token.binding with
      | None ->
          if
            Fake.Identity.equal identity (Fake.dir_identity root)
            && Fake.owner_equal owner (Fake.dir_owner root)
            && root_is_live root
          then (
            token.binding <- Some { root; identity; owner };
            Ok ())
          else reject "gc-lease-root-mismatch"
      | Some binding ->
          if binding_matches binding root identity owner then Ok ()
          else reject "gc-lease-root-substitution"

  let validate_continuation token ~root ~identity ~owner =
    if (not token.active) || token.continuity_broken then
      reject "gc-lease-continuity-broken"
    else
      match token.binding with
      | Some binding when binding_matches binding root identity owner -> Ok ()
      | Some _ | None -> reject "gc-lease-continuation-mismatch"

  let mint () = { active = true; continuity_broken = false; binding = None }

  let invalidate token =
    token.active <- false;
    token.continuity_broken <- true

  let reactivate token = token.active <- true
end

module Adapter = Cache.Make (Fake) (Fake_gc)

let adapter_relative components =
  match Cache.Relative.of_strings components with
  | Ok relative -> relative
  | Error invalid ->
      Alcotest.failf "invalid cache component %S: %a" invalid.component
        Cache.Name.pp_error invalid.reason

let adapter_failure_error (failure : Adapter.operation_failure) =
  Cache.issue_error failure.primary

let adapter_fail context failure =
  let error = adapter_failure_error failure in
  Alcotest.failf "%s: %s/%s" context
    (Cache.operation_name error.operation)
    error.native_code

let adapter_get context = function
  | Ok value -> value
  | Error failure -> adapter_fail context failure

let adapter_budget ~depth ~entries ~bytes =
  match
    Cache.Traversal_budget.create ~max_depth:depth ~max_entries:entries
      ~max_native_name_bytes:bytes
  with
  | Ok budget -> budget
  | Error _ -> Alcotest.fail "valid adapter traversal budget rejected"

let expect_teardown context = function
  | Cache.Teardown_complete | Cache.Teardown_local_only -> ()
  | Cache.Teardown_incomplete { failure; _ } -> adapter_fail context failure

let reset_cache () =
  Fake.Control.reset ();
  ignore (Fake.Control.mkdir "/workspace");
  ignore (Fake.Control.mkdir "/cache")

let adapter_acquire requested =
  let workspace = get_ok_failure (Fake.probe_path "/workspace") in
  Adapter.acquire ~workspace ~requested

let adapter_existing_root () =
  match adapter_acquire "/cache" with
  | Adapter.Acquired acquired -> acquired.root
  | Adapter.Acquisition_incomplete { failure; _ } ->
      adapter_fail "acquire existing cache" failure

let adapter_capture_regular root components =
  match
    adapter_get "capture regular"
      (Adapter.capture_regular root (adapter_relative components) ~limit:128L)
  with
  | Cache.Contents (read : Adapter.captured_read) -> read.captured
  | Cache.Read_missing -> Alcotest.fail "captured file disappeared"

let adapter_issue_codes issues =
  List.map (fun issue -> (Cache.issue_error issue).native_code) issues

let cleanup_problems (cleanup : 'retry Cache.cleanup_failure) =
  cleanup.primary :: cleanup.suppressed

let retry_tokens_of_issue = function
  | Cache.Operation_error _ -> []
  | Cache.Cleanup_error cleanup ->
      List.filter_map
        (fun problem ->
          match problem.Cache.local_handle with
          | Cache.Handle_still_open retry -> Some retry
          | Cache.Handle_closed | Cache.Handle_invalidated_unknown -> None)
        (cleanup_problems cleanup)

let retry_tokens_of_failure (failure : Adapter.operation_failure) =
  List.concat_map retry_tokens_of_issue (failure.primary :: failure.suppressed)

let issue_has_invalidated = function
  | Cache.Operation_error _ -> false
  | Cache.Cleanup_error cleanup ->
      List.exists
        (fun problem ->
          problem.Cache.local_handle = Cache.Handle_invalidated_unknown)
        (cleanup_problems cleanup)

let listed_entries listing =
  adapter_get "listed entries" (Adapter.listed_entries listing)

let find_listed encoded listing =
  match
    List.find_opt
      (fun entry ->
        String.equal encoded
          (Adapter.Native_name.encode (Adapter.listed_name entry)))
      (listed_entries listing)
  with
  | Some entry -> entry
  | None -> Alcotest.failf "listed entry %S missing" encoded

let inspect_path authority root components =
  match
    adapter_get "inspect path"
      (Adapter.inspect_path authority root (adapter_relative components))
  with
  | Cache.Contents inspected -> inspected
  | Cache.Read_missing -> Alcotest.fail "inspected path disappeared"

let test_adapter_separation_witness () =
  Fake.Control.reset ();
  ignore (Fake.Control.mkdir "/workspace");
  (match adapter_acquire "/workspace/new/cache" with
  | Adapter.Acquired _ -> Alcotest.fail "overlapping cache root was acquired"
  | Adapter.Acquisition_incomplete { created; failure } ->
      let error = adapter_failure_error failure in
      Alcotest.(check int) "no namespace commit" 0 (List.length created);
      Alcotest.(check bool)
        "separation primitive retained" true
        (error.primitive_operation = Some C.Establish_separation));
  Alcotest.(check bool)
    "failed proof created nothing" false
    (Fake.Control.exists "/workspace/new");
  ignore (Fake.Control.mkdir "/safe");
  let acquired =
    match adapter_acquire "/safe/new/cache" with
    | Adapter.Acquired acquired -> acquired
    | Adapter.Acquisition_incomplete { failure; _ } ->
        adapter_fail "safe acquisition" failure
  in
  let created =
    List.map
      (function
        | Adapter.Creation_observed name
        | Adapter.Creation_may_have_committed name ->
            Adapter.Native_name.encode name)
      acquired.created
  in
  Alcotest.(check (list string))
    "ordered audit evidence" [ "new"; "cache" ] created;
  expect_teardown "close acquired root" (Adapter.close_root acquired.root)

let test_adapter_retained_caps_are_duplicates () =
  reset_cache ();
  let root = adapter_existing_root () in
  let staged =
    match
      Adapter.stage_file root (adapter_relative [ "pending" ]) ~contents:"value"
    with
    | Adapter.Staged staged -> staged
    | Adapter.Staging_not_created failure
    | Adapter.Staging_incomplete_actionable { failure; _ }
    | Adapter.Staging_incomplete_audit_only { failure; _ } ->
        adapter_fail "stage file" failure
  in
  let listing =
    match
      Adapter.list root Cache.Relative.root
        ~budget:(adapter_budget ~depth:0L ~entries:8L ~bytes:128L)
    with
    | Adapter.Listing_complete { listing; _ } -> listing
    | Adapter.Listing_incomplete { failure; _ } ->
        adapter_fail "list cache" failure
  in
  expect_teardown "close root before retained operations"
    (Adapter.close_root root);
  ignore (listed_entries listing);
  (match
     Adapter.publish_no_replace staged
       ~target:(adapter_relative [ "published" ])
   with
  | Adapter.Stage_published _ -> ()
  | Adapter.Stage_publish_rejected failure
  | Adapter.Stage_not_published { failure; _ } ->
      adapter_fail "publish after root close" failure);
  Alcotest.(check (option string))
    "retained stage published" (Some "value")
    (Fake.Control.content "/cache/published");
  expect_teardown "close retained listing" (Adapter.close_listing listing)

let test_adapter_cleanup_aggregate_keeps_retry () =
  reset_cache ();
  Fake.Control.fail_closes [ C.Still_open; C.Invalidated_unknown ];
  let acquired =
    match adapter_acquire "/cache/new" with
    | Adapter.Acquired acquired -> acquired
    | Adapter.Acquisition_incomplete { failure; _ } ->
        adapter_fail "acquire with cleanup faults" failure
  in
  Alcotest.(check bool)
    "terminal sibling retained" true
    (List.exists issue_has_invalidated acquired.advisories);
  let retries = List.concat_map retry_tokens_of_issue acquired.advisories in
  let retry =
    match retries with
    | retry :: _ -> retry
    | [] -> Alcotest.fail "Still_open cleanup retry was flattened"
  in
  let before = Fake.Control.close_calls () in
  expect_teardown "retry mixed cleanup" (Adapter.retry_cleanup retry);
  Alcotest.(check int)
    "retry reached exact witness" (before + 2)
    (Fake.Control.close_calls ());
  expect_teardown "close acquired cache" (Adapter.close_root acquired.root)

let test_adapter_partial_inspection_close_is_live () =
  reset_cache ();
  ignore (Fake.Control.put_file "/cache/marker" "owned");
  let root = adapter_existing_root () in
  let inspected = inspect_path (Fake_gc.mint ()) root [ "marker" ] in
  Fake.Control.fail_next_close C.Still_open;
  let live, failure =
    match Adapter.close_inspected_path inspected with
    | Cache.Teardown_incomplete { live = Some live; failure } -> (live, failure)
    | Cache.Teardown_incomplete { live = None; _ } ->
        Alcotest.fail "partial inspection close lost its live capability"
    | Cache.Teardown_complete | Cache.Teardown_local_only ->
        Alcotest.fail "injected close failure disappeared"
  in
  let retry =
    match retry_tokens_of_failure failure with
    | retry :: _ -> retry
    | [] -> Alcotest.fail "partial inspection close lost retry token"
  in
  expect_teardown "retry inspection handle" (Adapter.retry_cleanup retry);
  expect_teardown "finish inspection close" (Adapter.close_inspected_path live);
  expect_teardown "close cache root" (Adapter.close_root root)

let test_adapter_listing_selection_is_fresh () =
  reset_cache ();
  ignore (Fake.Control.put_file "/cache/candidate" "old");
  let root = adapter_existing_root () in
  let listing =
    match
      Adapter.list root Cache.Relative.root
        ~budget:(adapter_budget ~depth:0L ~entries:8L ~bytes:128L)
    with
    | Adapter.Listing_complete { listing; _ } -> listing
    | Adapter.Listing_incomplete { failure; _ } ->
        adapter_fail "list candidate" failure
  in
  let entry = find_listed "candidate" listing in
  ignore (Fake.Control.put_file "/cache/candidate" "replacement");
  let inspected =
    match
      adapter_get "inspect replaced listing"
        (Adapter.inspect_listed (Fake_gc.mint ()) entry)
    with
    | Cache.Contents inspected -> inspected
    | Cache.Read_missing -> Alcotest.fail "replacement disappeared"
  in
  Alcotest.(check int64)
    "predicate sees replacement stat" 11L
    (Adapter.inspected_entry_stat inspected).size;
  expect_teardown "close fresh inspection"
    (Adapter.close_inspected_entry inspected);
  expect_teardown "close listing" (Adapter.close_listing listing);
  Alcotest.(check (option string))
    "replacement was not selected by stale evidence" (Some "replacement")
    (Fake.Control.content "/cache/candidate");
  expect_teardown "close root" (Adapter.close_root root)

let test_adapter_swap_after_inspection_is_refused () =
  reset_cache ();
  ignore (Fake.Control.put_file "/cache/victim" "old");
  let root = adapter_existing_root () in
  let inspected = inspect_path (Fake_gc.mint ()) root [ "victim" ] in
  ignore (Fake.Control.put_file "/cache/victim" "replacement");
  let live =
    match
      Adapter.remove_tree inspected
        ~budget:(adapter_budget ~depth:0L ~entries:0L ~bytes:0L)
    with
    | Adapter.Removal_complete _ ->
        Alcotest.fail "swapped replacement was removed"
    | Adapter.Removal_incomplete
        { live_inspection = Some live; failure; progress } ->
        Alcotest.(check bool)
          "identity change" true
          ((adapter_failure_error failure).class_ = Cache.Identity_changed);
        Alcotest.(check int64) "nothing removed" 0L progress.removed;
        live
    | Adapter.Removal_incomplete { live_inspection = None; _ } ->
        Alcotest.fail "swap refusal consumed the inspection"
  in
  Alcotest.(check (option string))
    "replacement retained" (Some "replacement")
    (Fake.Control.content "/cache/victim");
  expect_teardown "close refused inspection" (Adapter.close_inspected_path live);
  expect_teardown "close root" (Adapter.close_root root)

let test_adapter_continuity_cannot_be_reactivated () =
  reset_cache ();
  ignore (Fake.Control.put_file "/cache/victim" "owned");
  let root = adapter_existing_root () in
  let authority = Fake_gc.mint () in
  let inspected = inspect_path authority root [ "victim" ] in
  Fake_gc.invalidate authority;
  Fake_gc.reactivate authority;
  let live =
    match
      Adapter.remove_tree inspected
        ~budget:(adapter_budget ~depth:0L ~entries:0L ~bytes:0L)
    with
    | Adapter.Removal_complete _ ->
        Alcotest.fail "reactivated lease authorized deletion"
    | Adapter.Removal_incomplete { live_inspection = Some live; failure; _ } ->
        Alcotest.(check bool)
          "continuity failure" true
          ((adapter_failure_error failure).class_ = Cache.Unsupported);
        live
    | Adapter.Removal_incomplete { live_inspection = None; _ } ->
        Alcotest.fail "continuity refusal consumed inspection"
  in
  Alcotest.(check (option string))
    "namespace unchanged" (Some "owned")
    (Fake.Control.content "/cache/victim");
  expect_teardown "close continuity inspection"
    (Adapter.close_inspected_path live);
  expect_teardown "close root" (Adapter.close_root root)

let run_budget_case ~depth ~entries ~bytes ~expected_entries ~empty =
  reset_cache ();
  ignore (Fake.Control.mkdir "/cache/tree");
  if not empty then ignore (Fake.Control.put_file "/cache/tree/a" "value");
  let root = adapter_existing_root () in
  let inspected = inspect_path (Fake_gc.mint ()) root [ "tree" ] in
  match
    Adapter.remove_tree inspected
      ~budget:(adapter_budget ~depth ~entries ~bytes)
  with
  | Adapter.Removal_complete { progress; _ } when empty ->
      Alcotest.(check int64) "empty target removed" 1L progress.removed;
      expect_teardown "close root" (Adapter.close_root root)
  | Adapter.Removal_complete _ -> Alcotest.fail "budget limit was bypassed"
  | Adapter.Removal_incomplete
      { live_inspection = Some live; failure; progress } ->
      Alcotest.(check bool)
        "budget exhausted" true
        ((adapter_failure_error failure).class_ = Cache.Budget_exhausted);
      Alcotest.(check int64)
        "exact consumed entries" expected_entries progress.entries;
      Alcotest.(check int64) "no mutation" 0L progress.removed;
      Alcotest.(check (option string))
        "child retained" (Some "value")
        (Fake.Control.content "/cache/tree/a");
      expect_teardown "close budget inspection"
        (Adapter.close_inspected_path live);
      expect_teardown "close root" (Adapter.close_root root)
  | Adapter.Removal_incomplete { live_inspection = None; _ } ->
      Alcotest.fail "budget exhaustion consumed the inspection"

let test_adapter_budget_boundaries () =
  run_budget_case ~depth:0L ~entries:0L ~bytes:0L ~expected_entries:0L
    ~empty:true;
  run_budget_case ~depth:0L ~entries:1L ~bytes:1L ~expected_entries:1L
    ~empty:false;
  run_budget_case ~depth:1L ~entries:0L ~bytes:1L ~expected_entries:0L
    ~empty:false;
  run_budget_case ~depth:1L ~entries:1L ~bytes:0L ~expected_entries:0L
    ~empty:false

let test_adapter_rejects_backend_underreporting () =
  reset_cache ();
  ignore (Fake.Control.put_file "/cache/victim" "value");
  let root = adapter_existing_root () in
  let expect_bad_listing label ~names ~entries ~native_name_bytes =
    Fake.Control.claim_enumeration ~names ~entries ~native_name_bytes;
    match
      Adapter.list root Cache.Relative.root
        ~budget:(adapter_budget ~depth:0L ~entries:8L ~bytes:128L)
    with
    | Adapter.Listing_complete _ ->
        Alcotest.failf "%s became listing authority" label
    | Adapter.Listing_incomplete { progress; failure } ->
        Alcotest.(check bool)
          (label ^ " class") true
          ((adapter_failure_error failure).class_
         = Cache.Backend_contract_violation);
        Alcotest.(check int64) (label ^ " progress") 0L progress.entries
  in
  expect_bad_listing "under-reported names" ~names:[ "victim"; "ghost" ]
    ~entries:1L ~native_name_bytes:11L;
  expect_bad_listing "negative consumption" ~names:[] ~entries:(-1L)
    ~native_name_bytes:0L;
  expect_bad_listing "over-budget consumption" ~names:[] ~entries:9L
    ~native_name_bytes:0L;

  ignore (Fake.Control.mkdir "/cache/tree");
  ignore (Fake.Control.put_file "/cache/tree/a" "value");
  let inspected = inspect_path (Fake_gc.mint ()) root [ "tree" ] in
  Fake.Control.claim_enumeration ~names:[ "a"; "ghost" ] ~entries:1L
    ~native_name_bytes:6L;
  let live =
    match
      Adapter.remove_tree inspected
        ~budget:(adapter_budget ~depth:1L ~entries:8L ~bytes:128L)
    with
    | Adapter.Removal_complete _ ->
        Alcotest.fail "under-reported recursive names were mutated"
    | Adapter.Removal_incomplete
        { live_inspection = Some live; progress; failure } ->
        Alcotest.(check bool)
          "recursive contract violation" true
          ((adapter_failure_error failure).class_
         = Cache.Backend_contract_violation);
        Alcotest.(check int64)
          "recursive untrusted progress hidden" 0L progress.entries;
        live
    | Adapter.Removal_incomplete { live_inspection = None; _ } ->
        Alcotest.fail "recursive contract failure consumed inspection"
  in
  Alcotest.(check (option string))
    "recursive child retained" (Some "value")
    (Fake.Control.content "/cache/tree/a");
  expect_teardown "close underreport inspection"
    (Adapter.close_inspected_path live);
  expect_teardown "close root" (Adapter.close_root root)

let test_adapter_recursive_link_cleanup_is_direct () =
  reset_cache ();
  ignore (Fake.Control.mkdir "/outside");
  ignore (Fake.Control.put_file "/outside/sentinel" "unchanged");
  ignore (Fake.Control.mkdir "/cache/tree");
  ignore (Fake.Control.swap_to_link "/cache/tree/link" "/outside");
  let root = adapter_existing_root () in
  let inspected = inspect_path (Fake_gc.mint ()) root [ "tree" ] in
  (match
     Adapter.remove_tree inspected
       ~budget:(adapter_budget ~depth:1L ~entries:1L ~bytes:4L)
   with
  | Adapter.Removal_complete { progress; _ } ->
      Alcotest.(check int64) "link and directory removed" 2L progress.removed
  | Adapter.Removal_incomplete { failure; _ } ->
      adapter_fail "recursive link cleanup" failure);
  Alcotest.(check (option string))
    "link target untouched" (Some "unchanged")
    (Fake.Control.content "/outside/sentinel");
  Alcotest.(check bool) "tree removed" false (Fake.Control.exists "/cache/tree");
  expect_teardown "close root" (Adapter.close_root root)

let test_adapter_captured_unlink_commit_boundaries () =
  reset_cache ();
  ignore (Fake.Control.put_file "/cache/precommit" "owned");
  let root = adapter_existing_root () in
  let captured = adapter_capture_regular root [ "precommit" ] in
  Fake.Control.fail_next_deletion_before_commit ();
  let retained =
    match Adapter.unlink_captured captured with
    | Adapter.Captured_unlink_retained { live_capture; failure } ->
        Alcotest.(check string)
          "precommit failure" "injected-delete-before-commit"
          (adapter_failure_error failure).native_code;
        live_capture
    | Adapter.Captured_unlink_rejected _ | Adapter.Captured_unlink_complete _ ->
        Alcotest.fail "precommit deletion did not retain its exact capture"
  in
  Alcotest.(check (option string))
    "precommit binding retained" (Some "owned")
    (Fake.Control.content "/cache/precommit");
  (match Adapter.unlink_captured retained with
  | Adapter.Captured_unlink_complete
      { disposition = Cache.Unlinked; advisories = [] } ->
      ()
  | Adapter.Captured_unlink_rejected _ | Adapter.Captured_unlink_retained _
  | Adapter.Captured_unlink_complete _ ->
      Alcotest.fail "retained precommit capture was not retryable");
  expect_teardown "close precommit root" (Adapter.close_root root);

  reset_cache ();
  ignore (Fake.Control.put_file "/cache/committed" "owned");
  let root = adapter_existing_root () in
  let captured = adapter_capture_regular root [ "committed" ] in
  Fake.Control.fail_next_deletion_after_commit ();
  (match Adapter.unlink_captured captured with
  | Adapter.Captured_unlink_complete
      { disposition = Cache.Unlinked; advisories } ->
      Alcotest.(check (list string))
        "committed action advisory"
        [ "injected-delete-after-commit" ]
        (adapter_issue_codes advisories)
  | Adapter.Captured_unlink_rejected _ | Adapter.Captured_unlink_retained _
  | Adapter.Captured_unlink_complete _ ->
      Alcotest.fail "committed deletion was not terminal success");
  Alcotest.(check bool)
    "committed binding removed" false
    (Fake.Control.exists "/cache/committed");
  expect_teardown "close committed root" (Adapter.close_root root);

  reset_cache ();
  ignore (Fake.Control.put_file "/cache/terminal" "owned");
  let root = adapter_existing_root () in
  let captured = adapter_capture_regular root [ "terminal" ] in
  Fake.Control.fail_next_close C.Invalidated_unknown;
  Fake.Control.fail_next_deletion_after_commit ();
  (match Adapter.unlink_captured captured with
  | Adapter.Captured_unlink_complete
      { disposition = Cache.Unlinked; advisories } -> (
      Alcotest.(check (list string))
        "action then terminal cleanup"
        [ "injected-delete-after-commit"; "injected-close-failure" ]
        (adapter_issue_codes advisories);
      match advisories with
      | [ _; Cache.Cleanup_error { primary; suppressed = [] } ] ->
          Alcotest.(check bool)
            "terminal cleanup progress" true
            (primary.local_handle = Cache.Handle_invalidated_unknown
            && primary.namespace_released)
      | _ -> Alcotest.fail "terminal cleanup evidence was flattened")
  | Adapter.Captured_unlink_rejected _ | Adapter.Captured_unlink_retained _
  | Adapter.Captured_unlink_complete _ ->
      Alcotest.fail "terminal committed deletion was not preserved");
  Alcotest.(check bool)
    "terminal binding removed" false
    (Fake.Control.exists "/cache/terminal");
  expect_teardown "close terminal root" (Adapter.close_root root)

let test_adapter_captured_unlink_retryable_cleanup () =
  reset_cache ();
  ignore (Fake.Control.put_file "/cache/retryable" "owned");
  let root = adapter_existing_root () in
  let captured = adapter_capture_regular root [ "retryable" ] in
  Fake.Control.fail_next_close C.Still_open;
  let retained, retry =
    match Adapter.unlink_captured captured with
    | Adapter.Captured_unlink_retained { live_capture; failure } ->
        let retry =
          match retry_tokens_of_failure failure with
          | retry :: _ -> retry
          | [] -> Alcotest.fail "retryable deletion lost its exact close token"
        in
        (live_capture, retry)
    | Adapter.Captured_unlink_rejected _ | Adapter.Captured_unlink_complete _ ->
        Alcotest.fail "retryable deletion consumed its live capture"
  in
  Alcotest.(check bool)
    "retryable namespace release retained" false
    (Fake.Control.exists "/cache/retryable");
  expect_teardown "retry captured delete close" (Adapter.retry_cleanup retry);
  (match Adapter.unlink_captured retained with
  | Adapter.Captured_unlink_complete
      { disposition = Cache.Unlinked; advisories = [] } ->
      ()
  | Adapter.Captured_unlink_rejected _ | Adapter.Captured_unlink_retained _
  | Adapter.Captured_unlink_complete _ ->
      Alcotest.fail "retry-closed capture did not finish deterministically");
  expect_teardown "close retryable root" (Adapter.close_root root)

let test_adapter_captured_unlink_swap_safety () =
  reset_cache ();
  ignore (Fake.Control.put_file "/cache/identity" "captured");
  let root = adapter_existing_root () in
  let captured = adapter_capture_regular root [ "identity" ] in
  ignore (Fake.Control.put_file "/cache/identity" "replacement");
  (match Adapter.unlink_captured captured with
  | Adapter.Captured_unlink_complete
      { disposition = Cache.Identity_changed_entry; advisories = [] } ->
      ()
  | Adapter.Captured_unlink_rejected _ | Adapter.Captured_unlink_retained _
  | Adapter.Captured_unlink_complete _ ->
      Alcotest.fail "identity swap was not refused deterministically");
  Alcotest.(check (option string))
    "replacement retained" (Some "replacement")
    (Fake.Control.content "/cache/identity");
  expect_teardown "close identity-swap root" (Adapter.close_root root);

  reset_cache ();
  let captured_parent = Fake.Control.mkdir "/cache/scope" in
  ignore (Fake.Control.put_file "/cache/scope/owned" "captured");
  ignore (Fake.Control.mkdir "/outside");
  ignore (Fake.Control.put_file "/outside/owned" "foreign");
  let root = adapter_existing_root () in
  let captured = adapter_capture_regular root [ "scope"; "owned" ] in
  ignore (Fake.Control.swap_to_directory "/cache/scope" "/outside");
  (match Adapter.unlink_captured captured with
  | Adapter.Captured_unlink_complete
      { disposition = Cache.Unlinked; advisories = [] } ->
      ()
  | Adapter.Captured_unlink_rejected _ | Adapter.Captured_unlink_retained _
  | Adapter.Captured_unlink_complete _ ->
      Alcotest.fail "captured parent authority was not retained");
  Alcotest.(check (option string))
    "captured parent entry removed" None
    (Fake.Control.content_under captured_parent [ "owned" ]);
  Alcotest.(check (option string))
    "replacement parent untouched" (Some "foreign")
    (Fake.Control.content "/outside/owned");
  expect_teardown "close parent-swap root" (Adapter.close_root root)

let test_adapter_stage_discard_exact_outcomes () =
  let stage root name =
    match
      Adapter.stage_file root (adapter_relative [ name ]) ~contents:"draft"
    with
    | Adapter.Staged staged -> staged
    | Adapter.Staging_not_created failure
    | Adapter.Staging_incomplete_actionable { failure; _ }
    | Adapter.Staging_incomplete_audit_only { failure; _ } ->
        adapter_fail ("stage " ^ name) failure
  in
  reset_cache ();
  let root = adapter_existing_root () in
  let staged = stage root "precommit-stage" in
  Fake.Control.fail_next_deletion_before_commit ();
  let retained =
    match Adapter.discard_stage staged with
    | Adapter.Stage_discard_retained { live_stage; failure } ->
        Alcotest.(check string)
          "stage precommit failure" "injected-delete-before-commit"
          (adapter_failure_error failure).native_code;
        live_stage
    | Adapter.Stage_discarded _ | Adapter.Stage_discard_local_only _
    | Adapter.Stage_discard_incomplete_audit_only _ ->
        Alcotest.fail "precommit stage deletion was not retained"
  in
  Alcotest.(check bool)
    "precommit stage retained" true
    (Fake.Control.exists "/cache/precommit-stage");
  (match Adapter.discard_stage retained with
  | Adapter.Stage_discarded { advisories = [] } -> ()
  | Adapter.Stage_discarded _ | Adapter.Stage_discard_local_only _
  | Adapter.Stage_discard_retained _
  | Adapter.Stage_discard_incomplete_audit_only _ ->
      Alcotest.fail "retained precommit stage was not retryable");
  expect_teardown "close precommit stage root" (Adapter.close_root root);

  reset_cache ();
  let root = adapter_existing_root () in
  let staged = stage root "retryable-stage" in
  Fake.Control.fail_next_close C.Still_open;
  let retained, retry =
    match Adapter.discard_stage staged with
    | Adapter.Stage_discard_retained { live_stage; failure } ->
        let retry =
          match retry_tokens_of_failure failure with
          | retry :: _ -> retry
          | [] -> Alcotest.fail "stage deletion lost its exact close token"
        in
        (live_stage, retry)
    | Adapter.Stage_discarded _ | Adapter.Stage_discard_local_only _
    | Adapter.Stage_discard_incomplete_audit_only _ ->
        Alcotest.fail "retryable stage deletion consumed its live stage"
  in
  Alcotest.(check bool)
    "retryable stage namespace released" false
    (Fake.Control.exists "/cache/retryable-stage");
  expect_teardown "retry stage delete close" (Adapter.retry_cleanup retry);
  (match Adapter.discard_stage retained with
  | Adapter.Stage_discarded { advisories = [] } -> ()
  | Adapter.Stage_discarded _ | Adapter.Stage_discard_local_only _
  | Adapter.Stage_discard_retained _
  | Adapter.Stage_discard_incomplete_audit_only _ ->
      Alcotest.fail "retry-closed stage did not finish deterministically");
  expect_teardown "close retryable stage root" (Adapter.close_root root);

  reset_cache ();
  let root = adapter_existing_root () in
  let staged = stage root "terminal-stage" in
  Fake.Control.fail_next_close C.Invalidated_unknown;
  Fake.Control.fail_next_deletion_after_commit ();
  (match Adapter.discard_stage staged with
  | Adapter.Stage_discarded { advisories } ->
      Alcotest.(check (list string))
        "stage action then terminal cleanup"
        [ "injected-delete-after-commit"; "injected-close-failure" ]
        (adapter_issue_codes advisories)
  | Adapter.Stage_discard_local_only _ | Adapter.Stage_discard_retained _
  | Adapter.Stage_discard_incomplete_audit_only _ ->
      Alcotest.fail "terminal committed stage deletion was not preserved");
  Alcotest.(check bool)
    "terminal stage binding removed" false
    (Fake.Control.exists "/cache/terminal-stage");
  expect_teardown "close terminal stage root" (Adapter.close_root root)

let test_adapter_not_published_returns_live_stage () =
  reset_cache ();
  ignore (Fake.Control.put_file "/cache/final" "existing");
  let root = adapter_existing_root () in
  let staged =
    match
      Adapter.stage_file root (adapter_relative [ "pending" ]) ~contents:"draft"
    with
    | Adapter.Staged staged -> staged
    | Adapter.Staging_not_created failure
    | Adapter.Staging_incomplete_actionable { failure; _ }
    | Adapter.Staging_incomplete_audit_only { failure; _ } ->
        adapter_fail "stage pending" failure
  in
  let retained =
    match
      Adapter.publish_no_replace staged ~target:(adapter_relative [ "final" ])
    with
    | Adapter.Stage_not_published { live_stage; _ } -> live_stage
    | Adapter.Stage_published _ -> Alcotest.fail "no-replace overwrote target"
    | Adapter.Stage_publish_rejected failure ->
        adapter_fail "publish rejected before attempt" failure
  in
  (match Adapter.discard_stage retained with
  | Adapter.Stage_discarded _ -> ()
  | Adapter.Stage_discard_local_only _ | Adapter.Stage_discard_retained _
  | Adapter.Stage_discard_incomplete_audit_only _ ->
      Alcotest.fail "returned stage was not actionable");
  Alcotest.(check (option string))
    "target retained" (Some "existing")
    (Fake.Control.content "/cache/final");
  Alcotest.(check bool)
    "stage removed" false
    (Fake.Control.exists "/cache/pending");
  expect_teardown "close root" (Adapter.close_root root)

let test_adapter_stage_partial_and_terminal_cleanup () =
  reset_cache ();
  let root = adapter_existing_root () in
  let staged =
    match
      Adapter.stage_file root (adapter_relative [ "partial" ]) ~contents:"draft"
    with
    | Adapter.Staged staged -> staged
    | Adapter.Staging_not_created failure
    | Adapter.Staging_incomplete_actionable { failure; _ }
    | Adapter.Staging_incomplete_audit_only { failure; _ } ->
        adapter_fail "stage partial" failure
  in
  Fake.Control.set_owner 2;
  Fake.Control.fail_next_close C.Still_open;
  let retained, failure =
    match Adapter.discard_stage staged with
    | Adapter.Stage_discard_retained { live_stage; failure } ->
        (live_stage, failure)
    | _ -> Alcotest.fail "partial foreign cleanup lost live stage"
  in
  let retry =
    match retry_tokens_of_failure failure with
    | retry :: _ -> retry
    | [] -> Alcotest.fail "partial stage cleanup lost retry"
  in
  expect_teardown "retry foreign local cleanup" (Adapter.retry_cleanup retry);
  (match Adapter.discard_stage retained with
  | Adapter.Stage_discard_local_only _ -> ()
  | _ -> Alcotest.fail "completed foreign cleanup was not local-only");
  Alcotest.(check bool)
    "foreign namespace retained" true
    (Fake.Control.exists "/cache/partial");
  Fake.Control.set_owner 1;
  expect_teardown "close root" (Adapter.close_root root);

  reset_cache ();
  let root = adapter_existing_root () in
  let staged =
    match
      Adapter.stage_file root
        (adapter_relative [ "terminal" ])
        ~contents:"draft"
    with
    | Adapter.Staged staged -> staged
    | Adapter.Staging_not_created failure
    | Adapter.Staging_incomplete_actionable { failure; _ }
    | Adapter.Staging_incomplete_audit_only { failure; _ } ->
        adapter_fail "stage terminal" failure
  in
  Fake.Control.set_owner 2;
  Fake.Control.fail_next_close C.Invalidated_unknown;
  (match Adapter.discard_stage staged with
  | Adapter.Stage_discard_incomplete_audit_only _ -> ()
  | _ -> Alcotest.fail "terminal cleanup ambiguity lost audit residual");
  Alcotest.(check bool)
    "terminal residual retained" true
    (Fake.Control.exists "/cache/terminal");
  Fake.Control.set_owner 1;
  expect_teardown "close terminal root" (Adapter.close_root root)

let test_adapter_published_advisory () =
  reset_cache ();
  let root = adapter_existing_root () in
  Fake.Control.set_advisory
    (C.make_error ~operation:C.Atomic_rename ~class_:C.Other
       ~native_domain:C.In_memory ~native_code:"directory-flush-failed" ());
  (match
     Adapter.replace_file root
       (adapter_relative [ "report.json" ])
       ~contents:"final"
   with
  | Adapter.Replacement_not_published { failure; _ } ->
      adapter_fail "replace not published" failure
  | Adapter.Replaced { advisories } ->
      Alcotest.(check (list string))
        "post-commit advisory"
        [ "directory-flush-failed" ]
        (List.map
           (fun issue -> (Cache.issue_error issue).native_code)
           advisories));
  Alcotest.(check (option string))
    "destination committed" (Some "final")
    (Fake.Control.content "/cache/report.json");
  expect_teardown "close root" (Adapter.close_root root)

let test_adapter_nested_replace_stages_with_target_parent () =
  reset_cache ();
  ignore (Fake.Control.mkdir "/cache/scopes/workspace");
  ignore (Fake.Control.put_file "/cache/scopes/workspace/latest" "old");
  let root = adapter_existing_root () in
  (match
     Adapter.replace_file root
       (adapter_relative [ "scopes"; "workspace"; "latest" ])
       ~contents:"run-2"
   with
  | Adapter.Replacement_not_published { failure; _ } ->
      adapter_fail "nested replace" failure
  | Adapter.Replaced _ -> ());
  Alcotest.(check (option string))
    "nested destination replaced" (Some "run-2")
    (Fake.Control.content "/cache/scopes/workspace/latest");
  expect_teardown "close root" (Adapter.close_root root)

let test_adapter_publication_rejects_parent_binding_swap () =
  reset_cache ();
  let original_parent = Fake.Control.mkdir "/cache/scope" in
  ignore (Fake.Control.mkdir "/outside");
  let root = adapter_existing_root () in
  let staged =
    match
      Adapter.stage_file root
        (adapter_relative [ "scope"; "pending" ])
        ~contents:"draft"
    with
    | Adapter.Staged staged -> staged
    | Adapter.Staging_not_created failure
    | Adapter.Staging_incomplete_actionable { failure; _ }
    | Adapter.Staging_incomplete_audit_only { failure; _ } ->
        adapter_fail "stage before parent swap" failure
  in
  ignore (Fake.Control.swap_to_directory "/cache/scope" "/outside");
  let retained =
    match
      Adapter.publish_no_replace staged
        ~target:(adapter_relative [ "scope"; "final" ])
    with
    | Adapter.Stage_not_published { live_stage; failure } ->
        let error = adapter_failure_error failure in
        Alcotest.(check bool)
          "parent identity mismatch" true
          (error.class_ = Cache.Unsupported);
        Alcotest.(check string)
          "same-parent proof" "cross-directory-publication-not-supported"
          error.native_code;
        live_stage
    | Adapter.Stage_published _ ->
        Alcotest.fail "publication crossed a replaced parent binding"
    | Adapter.Stage_publish_rejected failure ->
        adapter_fail "publication rejected before parent check" failure
  in
  Alcotest.(check (option string))
    "replacement parent untouched" None
    (Fake.Control.content "/outside/final");
  (match Adapter.discard_stage retained with
  | Adapter.Stage_discarded _ -> ()
  | Adapter.Stage_discard_local_only _ | Adapter.Stage_discard_retained _
  | Adapter.Stage_discard_incomplete_audit_only _ ->
      Alcotest.fail "retained stage was not actionable after parent swap");
  Alcotest.(check (option string))
    "original stage removed through retained parent" None
    (Fake.Control.content_under original_parent [ "pending" ]);
  expect_teardown "close root" (Adapter.close_root root)

let () =
  Alcotest.run "dir-cap-contract"
    [
      ( "in-memory",
        [
          Alcotest.test_case "capture and native names" `Quick
            test_capture_and_native_names;
          Alcotest.test_case "create file commit boundary" `Quick
            test_create_file_commit_boundary_and_exact_bytes;
          Alcotest.test_case "captured deletion evidence" `Quick
            test_captured_deletion_commit_and_cleanup_evidence;
          Alcotest.test_case "conditional marker unlink" `Quick
            test_changed_marker_is_retained;
          Alcotest.test_case "direct link cleanup" `Quick
            test_link_cleanup_is_direct;
          Alcotest.test_case "published rename advisory" `Quick
            test_published_rename_retains_advisory;
          Alcotest.test_case "foreign owner local close" `Quick
            test_foreign_owner_cleanup_is_local;
          Alcotest.test_case "exact enumeration budget" `Quick
            test_enumeration_budget_is_exact;
          Alcotest.test_case "raced component exclusion" `Quick
            test_separation_rechecks_raced_component;
          Alcotest.test_case "audit-only creation evidence" `Quick
            test_creation_evidence_has_no_persisted_identity;
          Alcotest.test_case "materialized root exact marker" `Quick
            test_materialized_root_uses_exact_marker_capability;
          Alcotest.test_case "duplicate and close progress" `Quick
            test_duplicate_and_typed_close_progress;
          Alcotest.test_case "permission validation" `Quick
            test_permission_validation;
          Alcotest.test_case "deterministic cleanup order" `Quick
            test_cleanup_order;
        ] );
      ( "cache-adapter",
        [
          Alcotest.test_case "separation witness" `Quick
            test_adapter_separation_witness;
          Alcotest.test_case "retained caps are duplicates" `Quick
            test_adapter_retained_caps_are_duplicates;
          Alcotest.test_case "aggregate cleanup retry" `Quick
            test_adapter_cleanup_aggregate_keeps_retry;
          Alcotest.test_case "partial inspection close" `Quick
            test_adapter_partial_inspection_close_is_live;
          Alcotest.test_case "fresh listing selection" `Quick
            test_adapter_listing_selection_is_fresh;
          Alcotest.test_case "swap refusal" `Quick
            test_adapter_swap_after_inspection_is_refused;
          Alcotest.test_case "continuous authority" `Quick
            test_adapter_continuity_cannot_be_reactivated;
          Alcotest.test_case "budget boundaries" `Quick
            test_adapter_budget_boundaries;
          Alcotest.test_case "reject backend underreporting" `Quick
            test_adapter_rejects_backend_underreporting;
          Alcotest.test_case "recursive link cleanup" `Quick
            test_adapter_recursive_link_cleanup_is_direct;
          Alcotest.test_case "captured unlink commit boundaries" `Quick
            test_adapter_captured_unlink_commit_boundaries;
          Alcotest.test_case "captured unlink retryable cleanup" `Quick
            test_adapter_captured_unlink_retryable_cleanup;
          Alcotest.test_case "captured unlink swap safety" `Quick
            test_adapter_captured_unlink_swap_safety;
          Alcotest.test_case "stage discard exact outcomes" `Quick
            test_adapter_stage_discard_exact_outcomes;
          Alcotest.test_case "live unpublished stage" `Quick
            test_adapter_not_published_returns_live_stage;
          Alcotest.test_case "stage cleanup progress" `Quick
            test_adapter_stage_partial_and_terminal_cleanup;
          Alcotest.test_case "published advisory" `Quick
            test_adapter_published_advisory;
          Alcotest.test_case "nested replace sibling stage" `Quick
            test_adapter_nested_replace_stages_with_target_parent;
          Alcotest.test_case "publication parent binding swap" `Quick
            test_adapter_publication_rejects_parent_binding_swap;
        ] );
    ]
