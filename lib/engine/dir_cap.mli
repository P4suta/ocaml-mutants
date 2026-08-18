(** Directory-handle filesystem authority shared by cache and snapshot
    implementations.

    Ambient path strings are accepted only by [probe_path]. Once a path has been
    probed, every descendant operation is relative to a captured capability and
    must not follow a link-like entry.

    Handle-relative lookup prevents a namespace swap from redirecting an
    operation outside the captured directory. It does not, by itself, prove
    integrity against an arbitrary same-principal process that can mutate
    entries inside that directory. Operations which require compare-and-act
    atomicity must therefore use a real backend primitive, rely on separately
    proven exclusive namespace authority, or return [Unsupported].

    POSIX proves that separate authority as an owner-exclusive envelope verified
    at capture time: the captured parent is a directory owned by the current
    effective user with mode exactly [0o700], and the addressed child lives on
    the parent's device. A device comparison does not prove a bind-mount
    boundary; that caveat is documented on [System]. The envelope excludes every
    other principal; a process with the same effective user remains unexcluded,
    so conditional operations inside the envelope detect and report such
    interference rather than prevent it. Where the envelope cannot be proven the
    operation fails closed with [Unsupported]. *)

module type NATIVE_NAME = sig
  type t

  val equal : t -> t -> bool

  val encode : t -> string
  (** [encode] is a lossless representation of the name returned by native
      enumeration. It is not a portable-filename policy. *)

  val pp : Format.formatter -> t -> unit
end

module type IDENTITY = sig
  type t

  val equal : t -> t -> bool

  val encode : t -> string
  (** [encode] is diagnostic data for a transient native identity observation.
      It is never persisted authority. Equality alone authorizes no action:
      identifiers may be reused, and a lookup-only observation (for example
      [probe_entry_no_follow]) does not pin the observed child. An API must also
      retain the live target capability or compare identity atomically. *)

  val pp : Format.formatter -> t -> unit
end

type operation =
  | Name_of_component
  | Probe_path
  | Relate_paths
  | Establish_separation
  | Materialize
  | Enumerate
  | Probe_entry
  | Open_directory
  | Duplicate_directory
  | Open_file
  | Open_file_for_publish
  | Read_file
  | Read_link
  | Create_directory
  | Create_file
  | Create_symlink
  | Chmod_directory
  | Chmod_file
  | Atomic_rename
  | Conditional_unlink
  | Unlink_link
  | Remove_empty_directory
  | Try_lock
  | Close_probe
  | Close_directory
  | Close_file
  | Close_separation
  | Release_lock

val operation_name : operation -> string

type error_class =
  | Missing
  | Already_exists
  | Not_directory
  | Not_regular
  | Not_link
  | Link_like
  | Too_large
  | Invalid_name
  | Access_denied
  | Busy
  | Unsupported
  | Wrong_process
  | Closed_capability
  | Other

type native_domain = Posix_errno | Win32 | Ntstatus | In_memory | Contract

type error = {
  operation : operation;
  class_ : error_class;
  native_domain : native_domain;
  native_code : string;
  component : string option;
}

val make_error :
  operation:operation ->
  class_:error_class ->
  native_domain:native_domain ->
  native_code:string ->
  ?component:string ->
  unit ->
  error

type relationship =
  | Same
  | Left_contains_right
  | Right_contains_left
  | Separate

type entry_kind = Directory | Regular | Symbolic_link | Other_entry
type materialization = Already_present | Newly_created
type replacement = No_replace | Replace
type lock_mode = Shared | Exclusive

type atomic_rename_result =
  | Not_published of error
  | Published of { advisories : error list }

type conditional_unlink_result = Unlinked | Absent | Identity_changed

type local_handle_state =
  | Still_open
  | Closed
  | Invalidated_unknown
      (** State of the caller-visible local capability after a cleanup attempt.
          [Still_open] means that the exact capability (or, for a multi-handle
          capability, the corresponding retained handle) remains live and may be
          passed to the same explicit close operation again. [Closed] is
          terminal and proves the local handles were released.
          [Invalidated_unknown] is terminal: the backend deliberately
          invalidated the local capability because the native close result did
          not prove whether the kernel resource was released; it must never be
          retried. States are recorded per native handle and are never merged
          across a multi-handle capability. *)

type cleanup_problem = {
  error : error;
  local_handle_state : local_handle_state;
  namespace_released : bool;
}

type cleanup_failure = {
  primary : cleanup_problem;
  suppressed : cleanup_problem list;
}

type cleanup_result =
  | Cleanup_complete
  | Cleanup_local_only
  | Cleanup_failed of cleanup_failure
      (** [Cleanup_local_only] is the successful fork-child case: the inherited
          local handle was closed, while the parent's lock or owned namespace
          entry was deliberately left untouched. *)

type issue = Operation_error of error | Cleanup_error of cleanup_failure
type failure = { primary : issue; suppressed : issue list }

type deletion_progress = {
  local_handle_state : local_handle_state;
  namespace_released : bool;
}

type conditional_delete_outcome =
  | Deletion_not_committed of error
  | Deletion_complete of conditional_unlink_result
  | Deletion_incomplete of { progress : deletion_progress; failure : failure }
      (** A captured deletion has an explicit native commit boundary.
          [Deletion_not_committed] leaves the target capability live and proves
          that no delete disposition was installed. [Deletion_complete Unlinked]
          consumes the target handle and proves that its pinned namespace entry
          was released. [Deletion_incomplete] reports the exact
          terminal/retryable local-handle state and whether namespace release
          was proven; operation failure remains primary and cleanup failure
          remains ordered in [failure]. *)

val issue_error : issue -> error
val failure_of_error : error -> failure

type permission_error = Permission_out_of_range of int

(** Native permission bits validated to the inclusive range [0..0o7777]. *)
module Permissions : sig
  type t

  val of_int : int -> (t, permission_error) result
  val to_int : t -> int

  val owner_read_write : t
  (** Private-file creation policy. This is [0o600] on POSIX. An ACL-based
      backend must express this as an owner-only policy in the atomic create
      operation, rather than inheriting broader grants and repairing them
      afterward. *)

  val owner_private_directory : t
  (** Private-directory creation policy. This is [0o700] on POSIX. An ACL-based
      backend must express this as an owner-only policy at the atomic create
      boundary, not as a best-effort chmod after creation. *)
end

type enumeration_budget_error =
  | Negative_max_entries of int64
  | Negative_max_native_name_bytes of int64

val run_cleanup_in_order : (unit -> cleanup_result) list -> cleanup_result list
(** Executes thunks strictly from the head of the list to the tail. *)

val resolve_cleanup :
  ('a, error) result -> cleanup_result list -> ('a, failure) result
(** An action error remains primary. Otherwise the first cleanup failure is
    primary; later cleanup failures are suppressed in execution order.
    [Cleanup_local_only] is intentional and is not an error. *)

module type S = sig
  module Native_name : NATIVE_NAME
  module Identity : IDENTITY

  type owner
  type dir
  type file
  type path_probe
  type separation_witness
  type lock
  type enumeration_budget

  type stat = {
    identity : Identity.t;
    kind : entry_kind;
    size : int64;
    permissions : Permissions.t;
    mtime_ns : int64;
  }

  type captured_read = { contents : string; stat : stat }

  type creation_evidence =
    | Creation_observed of Native_name.t
    | Creation_may_have_committed of Native_name.t
        (** Audit evidence only. Neither constructor is cleanup authority and
            neither claims that the current named entry is the object created by
            this operation. *)

  type materialized = {
    directory : dir;
    disposition : materialization;
    created : creation_evidence list;
    advisories : issue list;
  }

  type enumeration_consumption = { entries : int64; native_name_bytes : int64 }

  type enumeration_outcome =
    | Enumerated of {
        names : Native_name.t list;
        consumption : enumeration_consumption;
      }
    | Enumeration_incomplete of {
        consumption : enumeration_consumption;
        failure : failure;
      }

  type materialization_outcome =
    | Materialized of materialized
    | Materialization_incomplete of {
        created : creation_evidence list;
        failure : failure;
      }

  type 'cap creation_residual =
    | Captured of 'cap
    | Uncaptured of creation_evidence

  type 'cap creation_outcome =
    | Not_created of error
    | Created of 'cap
    | Creation_incomplete of {
        residual : 'cap creation_residual;
        failure : failure;
      }

  val current_owner : unit -> owner
  val owner_equal : owner -> owner -> bool
  val probe_owner : path_probe -> owner
  val dir_owner : dir -> owner
  val file_owner : file -> owner
  val lock_owner : lock -> owner
  val dir_identity : dir -> Identity.t
  val file_identity : file -> Identity.t
  val lock_directory_identity : lock -> Identity.t
  val lock_file_identity : lock -> Identity.t

  val probe_display : path_probe -> string
  (** A diagnostic display captured while probing. It is never descendant I/O
      authority. *)

  val name_of_component : string -> (Native_name.t, error) result
  (** Converts one native leaf name. It rejects only names that the backend
      cannot address safely (for example separators, NUL, [.], or [..]); it must
      not impose a cross-platform portable-name allowlist. *)

  val probe_path : string -> (path_probe, failure) result
  (** Captures the existing directory chain without creating anything. *)

  val relationship : path_probe -> path_probe -> (relationship, error) result

  val establish_separation :
    forbidden:path_probe ->
    candidate:path_probe ->
    (separation_witness, failure) result
  (** On success, consumes both probes and transfers their live handle chains
      into the witness. On failure neither probe is consumed and the caller
      retains responsibility for closing both probes.

      [System] has one materialization-specific proof in addition to a generic
      [Separate] relationship: a candidate with exactly one missing leaf may
      share its deepest captured parent with the complete forbidden chain when
      that parent is a proper forbidden ancestor. This does not make
      [relationship] report [Separate]. The resulting witness is usable only by
      the exclusive, collision-never-join leaf creation described below. *)

  val materialize :
    separation_witness -> permissions:Permissions.t -> materialization_outcome
  (** Consumes the witness's one-shot materialization authority. Every opened
      component must be checked against the still-live forbidden identity chain
      before creation may continue below it. The witness remains the exact retry
      authority for any [Still_open] teardown issue returned in a success
      advisory or incomplete failure; call [close_separation] again. After the
      call it has close-only authority and materialization cannot be retried,
      regardless of outcome. Other non-close operations never return
      [Still_open] for an internal temporary handle.

      [System] can materialize exactly one missing component. It creates that
      leaf relative to the retained deepest parent handle with the same
      owner-private, no-link-traversal contract as [create_directory], checks
      the new identity against the live forbidden chain, and returns the live
      child capability. The captured parent may be a proper ancestor in that
      chain only when [establish_separation] recorded the exclusive missing-leaf
      proof. It never joins a colliding entry. A longer missing suffix fails
      before commit with [Unsupported]. *)

  val enumeration_budget :
    max_entries:int64 ->
    max_native_name_bytes:int64 ->
    (enumeration_budget, enumeration_budget_error) result

  val enumerate_no_follow :
    dir -> budget:enumeration_budget -> enumeration_outcome
  (** Returns lossless native names and never traverses an entry. Repeated calls
      restart from the beginning. Operations using the same capability must not
      overlap; in particular, enumeration and close are affine. Consumption is
      exact even for [Enumeration_incomplete] and counts names accepted by the
      native enumerator. An incomplete prefix is deliberately not returned as
      authority. [native_name_bytes] is the sum of raw component byte lengths on
      POSIX and canonical WTF-8 byte lengths on Windows; separators and
      terminators are not counted. Budget exhaustion returns
      [Enumeration_incomplete], never a silently truncated successful listing.
  *)

  val probe_entry_no_follow :
    dir -> Native_name.t -> (stat option, failure) result

  val open_directory_no_follow : dir -> Native_name.t -> (dir, failure) result

  val open_directory_for_delete_no_follow :
    dir -> Native_name.t -> (dir, failure) result
  (** Captures one existing directory child with the delete authority required
      by [remove_captured_empty_directory_if_identity]. It never follows a
      link-like leaf. Windows pins the captured entry by denying delete/rename
      sharing. POSIX proves the owner-exclusive envelope of the captured parent
      and retains the capture name for the later descriptor-verified delete; it
      does not pin the name against a same-effective-user writer. A backend that
      can prove neither authority returns [Unsupported]. *)

  val duplicate_directory : dir -> (dir, failure) result
  val open_file_no_follow : dir -> Native_name.t -> (file, failure) result

  val open_file_for_delete_no_follow :
    dir -> Native_name.t -> (file, failure) result
  (** Captures one existing regular file with the delete authority required by
      [unlink_captured_file_if_identity]. It never follows a link-like leaf.
      Windows refuses new write/delete sharing for the captured handle. POSIX
      proves the owner-exclusive envelope of the captured parent and retains the
      capture name for the later descriptor-verified delete. The returned
      capability, rather than its former name, is the deletion authority. *)

  val open_file_for_publish_no_follow :
    dir -> Native_name.t -> (file, failure) result
  (** Captures an existing regular file for immutable publication. Success
      proves that the live handle carries the native authority required by
      [atomic_rename]. Windows additionally denies concurrent content mutation
      by refusing write/delete sharing. POSIX proves a kernel descriptor
      binding, verified against the process file-descriptor namespace at
      capture, that [atomic_rename] can publish without re-resolving a source
      name; it does not deny a same-effective-user writer. A backend that cannot
      prove the publication authority must return [Unsupported]. The operation
      never creates, replaces, or follows the named entry. *)

  val read_captured : file -> limit:int64 -> (captured_read, error) result
  (** Reads and stats the object represented by [file]. A namespace lookup must
      not be repeated after the file handle has been captured. Reads use
      explicit offsets and are repeatable; close must not overlap an in-flight
      read. *)

  val create_directory :
    dir -> Native_name.t -> permissions:Permissions.t -> dir creation_outcome
  (** Creates exactly one child relative to the captured parent. [System]
      accepts only [Permissions.owner_private_directory]. On Windows it applies
      a protected owner-only inheritable DACL in the same [FILE_CREATE] request
      and returns a live child handle that refuses delete sharing. On POSIX it
      commits with an exclusive [mkdirat], captures the committed child with
      [openat(O_DIRECTORY|O_NOFOLLOW|O_CLOEXEC)], and verifies through that
      descriptor the directory kind, exactly [0o700] after any umask
      restriction, effective-user ownership, same-device placement, and the
      name/descriptor identity binding; that one-lookup capture window and the
      returned handle do not exclude a same-effective-user process. A name that
      appeared first, including a link-like entry, is never opened or traversed.
      A post-commit capture or verification failure is reported as
      [Creation_may_have_committed], with terminal handle-cleanup evidence kept
      in the failure. Creation evidence is not rollback authority. The retained
      child handle can be used with
      [remove_captured_empty_directory_if_identity] only while the caller also
      retains the matching captured parent capability; callers must never turn
      evidence or a diagnostic identity into an ambient-path delete. *)

  val create_file :
    dir ->
    Native_name.t ->
    permissions:Permissions.t ->
    contents:string ->
    file creation_outcome
  (** Creates exactly one regular file relative to the captured parent and
      writes all [contents] before returning [Created]. It never replaces an
      entry. A name that appeared first, including a link-like entry, is not
      opened or traversed. Windows applies a protected effective-user-only DACL
      in [FILE_CREATE] and refuses write/delete sharing on the returned handle.
      POSIX uses [openat] with [O_CREAT], [O_EXCL], [O_NOFOLLOW], [O_CLOEXEC],
      and an owner-only mode, then verifies the captured regular inode.

      A post-commit write or capture failure is [Creation_may_have_committed],
      with terminal handle-cleanup evidence retained. Success proves the bytes
      were written completely and matched a read-back through the live handle at
      verification time. Windows retains its write/delete-sharing exclusion for
      the lifetime of that handle. POSIX does not exclude another process with
      the same effective user and independent namespace authority. Neither
      backend claims file or parent-directory crash durability. Creation
      evidence is not conditional unlink authority. *)

  val create_symlink :
    dir -> Native_name.t -> target:string -> (unit, error) result

  val chmod_directory : dir -> permissions:Permissions.t -> (unit, error) result
  val chmod_file : file -> permissions:Permissions.t -> (unit, error) result

  val atomic_rename :
    file ->
    into:dir ->
    as_:Native_name.t ->
    replacement:replacement ->
    atomic_rename_result
  (** [Not_published] guarantees that the destination was not committed.
      [Published] guarantees that it was committed; later durability or
      handle-cleanup failures appear only as ordered [advisories]. [No_replace]
      is one indivisible commit attempt; an existence probe followed by a
      replacing rename does not satisfy this contract. On Windows, [System]
      implements both policies from the captured source and same-parent handles
      with [FileRenameInformationEx]; [Replace] selects the native
      replace-if-present flag in that one commit attempt. POSIX [No_replace]
      publishes from the captured descriptor binding with one indivisible
      [linkat] that never replaces an existing name; the source is the
      descriptor itself, never a re-resolved name. POSIX [Replace] requires the
      destination directory's owner-exclusive envelope: it stages the descriptor
      under a fresh verified temporary name and commits with one [renameat2];
      the temporary window is open only to a same-effective-user process. After
      either POSIX commit the staged source name is consumed only while it still
      binds the published identity; a failed or skipped consumption is an
      ordered advisory, is never a conditional-unlink implementation, and never
      affects the committed destination. A POSIX backend without the
      descriptor-binding primitive returns [Unsupported]. *)

  val unlink_if_identity :
    dir ->
    Native_name.t ->
    expected:Identity.t ->
    (conditional_unlink_result, error) result
  (** Atomically refuses to unlink when the named entry no longer has
      [expected]'s identity. A backend without a true conditional primitive or
      separately proven exclusive namespace-mutation authority must return
      [Unsupported]. In particular, an [fstatat] followed by [unlinkat] is not
      an implementation of this operation. *)

  val unlink_captured_file_if_identity :
    parent:dir -> file -> expected:Identity.t -> conditional_delete_outcome
  (** Conditionally deletes the entry pinned by [file]. [parent] and [expected]
      must match the parent/target identities bound when the delete-authority
      handle was captured. Windows installs a POSIX-style delete disposition on
      that exact handle, never resolving a name, and terminally closes it. POSIX
      revalidates the owner-exclusive envelope and the live parent/target
      identities through descriptors, refuses to commit unless the retained
      capture name still binds the target descriptor's identity, and then
      unlinks that name; the window between that check and the unlink is open
      only to a same-effective-user process inside the envelope. A rebound entry
      reports [Identity_changed] and is never deleted. A backend without either
      authority returns [Deletion_not_committed Unsupported] and must not
      emulate this operation with an unverified [fstatat] followed by
      [unlinkat]. *)

  val unlink_link_no_follow :
    dir ->
    Native_name.t ->
    expected:Identity.t ->
    (conditional_unlink_result, error) result
  (** Removes the link entry itself and never opens or traverses its target. *)

  val remove_empty_directory_if_identity :
    dir ->
    Native_name.t ->
    expected:Identity.t ->
    (conditional_unlink_result, error) result

  val remove_captured_empty_directory_if_identity :
    parent:dir -> dir -> expected:Identity.t -> conditional_delete_outcome
  (** The directory analogue of [unlink_captured_file_if_identity]. The target
      is the captured directory handle itself; emptiness and deletion are one
      native disposition attempt. A failed non-empty check is pre-commit and
      leaves the target live. Link-like deletion remains outside this API. *)

  val try_lock :
    dir ->
    Native_name.t ->
    lock_mode ->
    ([ `Acquired of lock | `Busy ], failure) result
  (** Opens or creates one persistent regular lock file relative to the captured
      directory, without following a link-like leaf, then attempts a nonblocking
      whole-file lock. [Busy] is not an error and no timeout or retry policy is
      hidden in this operation. The acquired capability binds the live lock-file
      handle to the directory and file identities observed during that
      root-relative acquisition. The identity accessors remain diagnostic: they
      are not persisted ownership authority.

      All cooperating participants must keep the lock-file name persistent and
      must never unlink, rename, or replace it. Windows acquisition refuses
      delete/rename sharing for the held handle. POSIX OFD locks are inode locks
      and cannot prove continuous name authority against an arbitrary
      same-principal namespace writer; this cooperative primitive must not be
      treated as deletion authority. A POSIX backend without OFD locks returns
      [Unsupported] and never falls back to process-owned record locks. *)

  val close_probe : path_probe -> cleanup_result
  val close_directory : dir -> cleanup_result
  val close_file : file -> cleanup_result
  val close_separation : separation_witness -> cleanup_result

  (** Only these explicit close operations, [release_lock], and materialization
      teardown may report [Still_open]. The returned state always refers to
      handles retained inside the exact caller-visible value supplied to that
      operation. Failures from all other operations must contain no [Still_open]
      cleanup issue. *)

  val release_lock : lock -> cleanup_result
  (** Releases the native lock before closing its handle and preserves every
      unlock/close problem in execution order. If unlock succeeds but close
      fails with [Still_open], a retry closes only; if unlock did not succeed
      and the exact handle remains [Still_open], a retry attempts both again.
      The persistent lock-file entry is never removed. All I/O and namespace
      mutation by a non-owning process must fail with [Wrong_process]. Cleanup
      in such a process may close its inherited local handle, and reports
      [Cleanup_local_only]. *)
end

module System : S
(** The native directory-capability backend. Descendant operations use captured
    OS handles. Operations that have neither a race-free native primitive nor a
    proven owner-exclusive envelope fail closed with [Unsupported].
    [materialize] can create exactly one missing leaf below the deepest retained
    candidate handle. This includes a missing sibling of a complete forbidden
    path under their shared captured parent; the generic [relationship] remains
    ambiguous, while [establish_separation] records the narrower
    exclusive-create proof. It never traverses or joins a collision, and longer
    missing suffixes remain unsupported. Both platforms also support one
    owner-private [create_directory] below an already captured parent. On
    Windows both creation paths use root-relative [FILE_CREATE], a protected
    DACL granting the effective token user [FILE_ALL_ACCESS] with
    container/object inheritance, and no delete sharing on the returned handle.
    On POSIX both creation paths commit with an exclusive [mkdirat] and verify
    the captured child as documented on [create_directory]; the returned handle
    does not pin the child's name. POSIX [O_NOFOLLOW] rejects symbolic-link
    traversal but does not prove a mount boundary (including bind mounts);
    production recursive traversal additionally requires native mount identity
    and no-cross-device resolution proof.

    Both platforms support one owner-private [create_file] below an already
    captured parent, without replacement. Windows supplies and then verifies a
    protected DACL with one [FILE_ALL_ACCESS] ACE for the effective token user;
    it rejects reparse points and keeps write/delete sharing denied while the
    returned handle is live. POSIX uses
    [openat(O_CREAT|O_EXCL|O_NOFOLLOW|O_CLOEXEC, 0o600)], restores exactly
    [0o600] after any umask restriction, and verifies the regular inode, mode,
    identity, size, and bytes through that descriptor. A build lacking these
    flags returns [Unsupported]. Neither implementation calls [fsync] or claims
    crash-durability authority. Creation evidence alone is not rollback
    authority; the live Windows handle is usable with the separate bounded
    captured-deletion operation described below.

    The protected DACL excludes other ordinary principals, but does not claim
    integrity against an arbitrary process running with the same effective token
    and independent authority over the parent. Refusing delete sharing pins the
    created child's name while its handle is live; it is not recursive cleanup
    or conditional rollback authority.

    [read_captured] addresses one captured inode/handle but its returned
    contents and final stat are not an atomic same-version snapshot under
    concurrent writers. Enumeration accumulation always uses the caller's typed
    budget. Windows query buffers and accumulated names are held by rooted
    finalizer-backed owners across OCaml allocation. On POSIX, enumeration opens
    an independent handle-relative directory description; action failure remains
    primary and a failing terminal [closedir] is retained in order. A POSIX
    build without the required [openat]/[fdopendir] primitives returns
    [Unsupported].

    Lock acquisition is nonblocking and root-relative. Windows uses an
    exact-case native open, rejects reparse points, refuses delete sharing, and
    uses [LockFileEx]. POSIX uses [openat] with [O_NOFOLLOW] and OFD locks only;
    it returns [Unsupported] rather than falling back to process-owned locks.
    The lock file is persistent and release never unlinks it. POSIX lock
    identity remains cooperative inode authority, not proof against a
    same-principal writer that renames or replaces the lock-file name, and is
    therefore not deletion or traversal authority.

    Windows delete capture uses a root-relative [NtCreateFile] request with
    [DELETE] access, rejects a reparse leaf, and denies delete sharing for the
    lifetime of the returned target handle. Captured deletion revalidates the
    live parent/target identities and kinds, then installs a POSIX-style delete
    disposition on that exact target handle and terminally closes it. It never
    reconstructs or resolves the former target name. POSIX delete capture proves
    the parent's owner-exclusive envelope, opens the leaf without following
    links, and retains the capture name; captured deletion revalidates the
    envelope and the live parent/target identities through descriptors and
    commits only while the retained name still binds the target descriptor's
    identity. A nonempty directory is pre-commit on both platforms and leaves
    the capability live. The outcome preserves whether the commit may have
    happened, the terminal/retryable local-handle state, namespace release
    proof, and ordered action/cleanup errors; a committed POSIX unlink itself
    proves namespace release, while Windows proves it at terminal close.
    Link-like deletion remains outside this bounded primitive.

    Windows immutable publication capture records the captured parent identity
    and opens an exact-case root-relative handle with write/delete sharing
    denied. [atomic_rename] accepts only that same live directory identity, then
    calls native [NtSetInformationFile] with [FileRenameInformationEx], that
    directory handle as [RootDirectory], and one native component. [No_replace]
    uses flags zero; [Replace] uses the documented
    [FILE_RENAME_REPLACE_IF_EXISTS] flag. Both are single exact-handle commit
    attempts, and destination resolution cannot fall back to the process current
    directory. POSIX publication capture verifies the descriptor binding through
    the process file-descriptor namespace; [atomic_rename] commits [No_replace]
    with one indivisible non-replacing [linkat] from that binding and [Replace]
    through a fresh verified temporary name under the destination's
    owner-exclusive envelope, as documented on [atomic_rename]. A POSIX build
    without that binding, including one without a [/proc] file-descriptor
    namespace, returns [Unsupported].

    Root and child acquisition failures, and Windows probe-entry temporary
    handle failures, preserve the action as primary and append every terminal
    internal-close failure in order. This bounded result does not establish the
    remaining mount, mutation, or rollback authorities. [Run_store] uses a
    bounded internal [Cache_fs] adapter only for existing-root acquisition,
    immutable stage capture, same-directory atomic publication, and explicit
    handle teardown. This backend is not general traversal/deletion authority
    for [Cache_fs] or [Workspace_snapshot]. *)
