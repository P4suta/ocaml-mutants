#ifndef _WIN32
#define _GNU_SOURCE
#endif
#define _POSIX_C_SOURCE 200809L
/* Strict POSIX hides Darwin's BSD stat members (st_mtimespec); opt back in. */
#ifdef __APPLE__
#define _DARWIN_C_SOURCE 1
#endif

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stddef.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
/* GetCurrentThreadEffectiveToken provides a cleanup-free pseudo-handle and is
   available from the Windows 8 API floor onward. */
#define DC_WINDOWS_API_FLOOR 0x0602
#if !defined(_WIN32_WINNT) || _WIN32_WINNT < DC_WINDOWS_API_FLOOR
#undef _WIN32_WINNT
#define _WIN32_WINNT DC_WINDOWS_API_FLOOR
#endif
#include <windows.h>
#include <winioctl.h>
#include <winternl.h>
typedef struct dc_reparse_data_buffer {
  ULONG ReparseTag;
  USHORT ReparseDataLength;
  USHORT Reserved;
  union {
    struct {
      USHORT SubstituteNameOffset;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      ULONG Flags;
      WCHAR PathBuffer[1];
    } SymbolicLinkReparseBuffer;
    struct {
      USHORT SubstituteNameOffset;
      USHORT SubstituteNameLength;
      USHORT PrintNameOffset;
      USHORT PrintNameLength;
      WCHAR PathBuffer[1];
    } MountPointReparseBuffer;
  } payload;
} dc_reparse_data_buffer;
#else
#include <dirent.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#endif

#if !defined(_WIN32) && defined(O_DIRECTORY) && defined(O_NOFOLLOW) && \
    defined(O_CLOEXEC)
#define DC_HAVE_POSIX_ENUMERATION 1
#endif

#if !defined(_WIN32) && defined(O_NOFOLLOW) && defined(O_CLOEXEC) && \
    defined(F_OFD_SETLK)
#define DC_HAVE_POSIX_OFD_LOCK 1
#endif

#if !defined(_WIN32) && defined(O_DIRECTORY) && defined(O_NOFOLLOW) && \
    defined(O_CLOEXEC)
#define DC_HAVE_POSIX_DIR_CREATE 1
#endif

/* The publication source binding links the captured descriptor through the
   process file-descriptor namespace; only Linux proves that namespace. */
#if defined(__linux__) && defined(O_NOFOLLOW) && defined(O_CLOEXEC) && \
    defined(AT_SYMLINK_FOLLOW)
#define DC_HAVE_POSIX_FD_LINK_PUBLISH 1
#endif

/* Conditional captured deletion requires the owner-exclusive envelope proof
   plus the openat/fstatat/unlinkat family. */
#if !defined(_WIN32) && defined(O_NOFOLLOW) && defined(O_CLOEXEC) && \
    defined(AT_SYMLINK_NOFOLLOW) && defined(AT_REMOVEDIR)
#define DC_HAVE_POSIX_ENVELOPE_DELETE 1
#endif

enum dc_domain {
  DC_POSIX = 0,
  DC_WIN32 = 1,
  DC_NTSTATUS = 2,
  DC_CONTRACT = 3
};
#ifdef _WIN32
#define DC_NATIVE_DOMAIN DC_WIN32
#else
#define DC_NATIVE_DOMAIN DC_POSIX
#endif
#define DC_IDENTITY_CAPACITY 96
#define DC_OWNER_PRIVATE_DIRECTORY_PERMISSIONS 0700
#define DC_OWNER_PRIVATE_FILE_PERMISSIONS 0600
enum dc_class {
  DC_MISSING = 0,
  DC_ALREADY_EXISTS = 1,
  DC_NOT_DIRECTORY = 2,
  DC_NOT_REGULAR = 3,
  DC_NOT_LINK = 4,
  DC_LINK_LIKE = 5,
  DC_TOO_LARGE = 6,
  DC_INVALID_NAME = 7,
  DC_ACCESS_DENIED = 8,
  DC_BUSY = 9,
  DC_UNSUPPORTED = 10,
  DC_WRONG_PROCESS = 11,
  DC_CLOSED = 12,
  DC_OTHER = 13
};

struct dc_handle {
#ifdef _WIN32
  HANDLE os;
#else
  int os;
#endif
  uint64_t owner;
  int state;
  int invalidated_error;
};

enum dc_handle_state {
  DC_HANDLE_OPEN = 0,
  DC_HANDLE_CLOSED = 1,
  DC_HANDLE_INVALIDATED_UNKNOWN = 2
};

enum dc_injection_site {
  DC_INJECT_ROOT = 0,
  DC_INJECT_CHILD = 1,
  DC_INJECT_PROBE = 2,
  DC_INJECT_ENUMERATE = 3,
  DC_INJECT_LOCK_OPEN = 4,
  DC_INJECT_LOCK_ACQUIRE = 5,
  DC_INJECT_LOCK_RELEASE = 6,
  DC_INJECT_PUBLISH = 7,
  DC_INJECT_CREATE_BEFORE_COMMIT = 8,
  DC_INJECT_CREATE_AFTER_COMMIT = 9,
  DC_INJECT_CREATE_FILE_BEFORE_COMMIT = 10,
  DC_INJECT_CREATE_FILE_AFTER_COMMIT = 11,
  DC_INJECT_DELETE_BEFORE_COMMIT = 12,
  DC_INJECT_DELETE_AFTER_COMMIT = 13
};

/* Test-only, process-local, and one-shot.  Keeping the whole request in one
   atomic word prevents a concurrent native call from observing a partially
   installed injection. */
static atomic_uint dc_test_injection = ATOMIC_VAR_INIT(0);
#define DC_INJECT_ACTION 1u
#define DC_INJECT_CLOSE 2u
#define DC_INJECT_SITE_SHIFT 2u

struct dc_strings {
  char **items;
  size_t length;
  size_t capacity;
};

struct dc_strings_guard {
  struct dc_strings strings;
};

struct dc_buffer_guard {
  void *buffer;
};

#ifdef DC_HAVE_POSIX_ENUMERATION
struct dc_directory_stream_guard {
  DIR *stream;
};
#endif

static uint64_t dc_owner(void);
static value dc_ok(value payload);
static value dc_error(int domain, int class_, const char *code);
static value dc_raw_operation_failure(value error_result);
static value dc_raw_operation_cleanup_failure(value operation_error_result,
                                              value cleanup_error_result);
static value dc_raw_cleanup_failure(value cleanup_error_result);
static value dc_raw_create_not_committed(value error_result);
static value dc_raw_create_may_have_committed(value failure_result);
static value dc_raw_delete_not_committed(value error_result);
static value dc_raw_delete_may_have_committed(int, int, value);
static value dc_lock_cleanup_failure(value, int, int, int, value, int, int);
static value dc_internal_close_terminal(value handle_value,
                                        int inject_failure);
static value dc_internal_action_failure(value handle_value,
                                        value operation_error_result,
                                        int inject_close_failure);
static int dc_take_test_injection(enum dc_injection_site site,
                                  int *inject_action,
                                  int *inject_close);
static value dc_injected_action_error(enum dc_injection_site site);
static value dc_close_error(value error_result, int local_handle_state);
static value dc_enumeration_failure(value failure_result, uint64_t entries,
                                    uint64_t native_name_bytes);
static int dc_strings_push(struct dc_strings *strings, char *owned);
static void dc_strings_free(struct dc_strings *strings);
static int dc_strings_guard_push(value guard_value, char *owned);
static void dc_strings_guard_free(value guard_value);
static value dc_strings_guard_to_list(value guard_value);
static value dc_alloc_strings_guard(void);
static struct dc_strings *dc_guarded_strings(value);
static value dc_alloc_buffer_guard(void);
#ifdef _WIN32
static void dc_buffer_guard_arm(value, void *);
#endif
static void dc_buffer_guard_release(value);
#ifdef DC_HAVE_POSIX_ENUMERATION
static value dc_alloc_directory_stream_guard(void);
static void dc_directory_stream_guard_arm(value, DIR *);
static value dc_directory_stream_close_terminal(value, int);
static void dc_handle_transfer_to_stream(value);
#endif
static int dc_handle_problem(struct dc_handle *handle, int *class_);
#ifdef _WIN32
static value dc_alloc_empty_handle(void);
static void dc_arm_handle(value, HANDLE);
static int dc_windows_component(const char *, size_t, wchar_t **, size_t *);
static int dc_wtf8_decode(const char *, size_t, wchar_t **, size_t *);
static char *dc_wtf8_encode(const wchar_t *, size_t);
static value dc_win32_error(DWORD code);
static value dc_nt_error(NTSTATUS status);
static int dc_reparse(HANDLE handle, DWORD *tag);
static int dc_windows_stat(HANDLE, int, char *, size_t, int *, int64_t *, int *,
                           int64_t *, DWORD *);
static value dc_alloc_raw_stat(const char *, int, int64_t, int, int64_t);
static NTSTATUS dc_windows_open_directory(HANDLE, const wchar_t *, size_t,
                                          ULONG, HANDLE *);
static NTSTATUS dc_windows_open_relative(HANDLE, const wchar_t *, size_t, int,
                                         ULONG, HANDLE *);
static NTSTATUS dc_windows_open_lock(HANDLE, const wchar_t *, size_t, HANDLE *);
enum dc_windows_private_entry_kind {
  DC_WINDOWS_PRIVATE_DIRECTORY = 0,
  DC_WINDOWS_PRIVATE_FILE = 1
};
static NTSTATUS dc_windows_create_private_entry(
    HANDLE, const wchar_t *, size_t, PSECURITY_DESCRIPTOR,
    enum dc_windows_private_entry_kind, HANDLE *);
static int dc_windows_write_all(HANDLE, const char *, size_t, DWORD *);
static NTSTATUS dc_windows_read_at(HANDLE, void *, ULONG, uint64_t, ULONG *);
#else
static value dc_alloc_empty_handle(void);
static void dc_arm_handle(value, int);
static value dc_posix_error(int code);
static int dc_posix_identity(const struct stat *, char *, size_t);
static int dc_posix_owner_exclusive(const struct stat *);
static int dc_posix_stat_value(const struct stat *, char *, size_t, int *,
                               int64_t *, int *, int64_t *);
static value dc_alloc_raw_stat(const char *, int, int64_t, int, int64_t);
static int dc_posix_write_all(int, const char *, size_t, int *);
#endif

CAMLprim value ocaml_mutants_dircap_owner(value unit) {
  CAMLparam1(unit);
  CAMLreturn(caml_copy_int64((int64_t)dc_owner()));
}

CAMLprim value ocaml_mutants_dircap_name_valid(value name_value) {
  CAMLparam1(name_value);
  CAMLlocal3(normalized_value, result, normalized_guard);
  normalized_guard = dc_alloc_strings_guard();
  const char *name = String_val(name_value);
  size_t length = caml_string_length(name_value);
#ifdef _WIN32
  wchar_t *wide = NULL;
  size_t wide_length = 0;
  char *normalized;
  if (!dc_windows_component(name, length, &wide, &wide_length))
    CAMLreturn(dc_error(DC_WIN32, DC_INVALID_NAME, "invalid-native-component"));
  normalized = dc_wtf8_encode(wide, wide_length);
  free(wide);
  if (normalized == NULL)
    CAMLreturn(dc_win32_error(ERROR_NOT_ENOUGH_MEMORY));
  if (!dc_strings_guard_push(normalized_guard, normalized)) {
    free(normalized);
    CAMLreturn(dc_win32_error(ERROR_NOT_ENOUGH_MEMORY));
  }
  normalized_value = caml_copy_string(normalized);
  dc_strings_guard_free(normalized_guard);
#else
  if (length == 0 || (length == 1 && name[0] == '.') ||
      (length == 2 && name[0] == '.' && name[1] == '.') ||
      memchr(name, '/', length) != NULL || memchr(name, '\0', length) != NULL)
    CAMLreturn(dc_error(DC_POSIX, DC_INVALID_NAME,
                        "invalid-native-component"));
  normalized_value = caml_alloc_initialized_string(length, name);
#endif
  result = dc_ok(normalized_value);
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_dircap_open_root(value path_value) {
  CAMLparam1(path_value);
  CAMLlocal5(handle_value, components_value, display_value, payload, result);
  CAMLlocal3(components_guard, display_guard, error_result);
  int inject_action = 0, inject_close = 0;
  components_guard = dc_alloc_strings_guard();
  display_guard = dc_alloc_strings_guard();
  handle_value = dc_alloc_empty_handle();
#ifdef _WIN32
  wchar_t *input = NULL, *absolute = NULL;
  size_t input_length = 0;
  DWORD required;
  size_t root_length = 0, index, start;
  HANDLE root = INVALID_HANDLE_VALUE;
  DWORD reparse_tag = 0;
  char *display = NULL;
  if (!dc_wtf8_decode(String_val(path_value), caml_string_length(path_value),
                       &input, &input_length))
    CAMLreturn(dc_raw_operation_failure(
        dc_error(DC_WIN32, DC_INVALID_NAME, "invalid-wtf8-path")));
  for (index = 0; index < input_length; ++index)
    if (input[index] == L'/') input[index] = L'\\';
  if (!((input_length >= 3 &&
          ((input[0] >= L'A' && input[0] <= L'Z') ||
          (input[0] >= L'a' && input[0] <= L'z')) &&
          input[1] == L':' && input[2] == L'\\') ||
        (input_length >= 5 && input[0] == L'\\' && input[1] == L'\\' &&
         input[2] != L'?' && input[2] != L'.'))) {
    free(input);
    CAMLreturn(dc_raw_operation_failure(dc_error(
        DC_WIN32, DC_INVALID_NAME, "path-is-not-absolute")));
  }
  if (input_length > MAXDWORD - 1) {
    free(input);
    CAMLreturn(dc_raw_operation_failure(
        dc_error(DC_WIN32, DC_INVALID_NAME, "path-too-large")));
  }
  required = (DWORD)input_length + 1;
  absolute = input;
  input = NULL;
  if (((absolute[0] >= L'A' && absolute[0] <= L'Z') ||
       (absolute[0] >= L'a' && absolute[0] <= L'z')) &&
      absolute[1] == L':' && absolute[2] == L'\\') {
    root_length = 3;
  } else if (absolute[0] == L'\\' && absolute[1] == L'\\' &&
             absolute[2] != L'?' && absolute[2] != L'.') {
    size_t server_start = 2, server_end, share_start, share_end;
    wchar_t *validated = NULL;
    size_t validated_length = 0;
    char *encoded = NULL;
    index = server_start;
    while (absolute[index] != L'\0' && absolute[index] != L'\\') ++index;
    server_end = index;
    if (server_end == server_start || absolute[index] != L'\\')
      goto invalid_unc_root;
    share_start = ++index;
    while (absolute[index] != L'\0' && absolute[index] != L'\\') ++index;
    share_end = index;
    if (share_end == share_start) goto invalid_unc_root;
    encoded = dc_wtf8_encode(absolute + server_start, server_end - server_start);
    if (encoded == NULL ||
        !dc_windows_component(encoded, strlen(encoded), &validated,
                              &validated_length)) {
      free(encoded);
      goto invalid_unc_root;
    }
    free(encoded);
    free(validated);
    encoded = dc_wtf8_encode(absolute + share_start, share_end - share_start);
    validated = NULL;
    if (encoded == NULL ||
        !dc_windows_component(encoded, strlen(encoded), &validated,
                              &validated_length)) {
      free(encoded);
      goto invalid_unc_root;
    }
    free(encoded);
    free(validated);
    if (absolute[index] == L'\\') ++index;
    root_length = index;
  } else {
  invalid_unc_root:
    free(absolute);
    CAMLreturn(dc_raw_operation_failure(dc_error(
        DC_WIN32, DC_INVALID_NAME, "unsupported-windows-path-root")));
  }
  if (root_length == 0 || root_length >= (size_t)required) {
    free(absolute);
    CAMLreturn(dc_raw_operation_failure(
        dc_error(DC_WIN32, DC_INVALID_NAME, "invalid-windows-root")));
  }
  {
    wchar_t saved = absolute[root_length];
    absolute[root_length] = L'\0';
    root = CreateFileW(
        absolute, FILE_LIST_DIRECTORY | FILE_READ_ATTRIBUTES,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL,
        OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
        NULL);
    absolute[root_length] = saved;
  }
  if (root == INVALID_HANDLE_VALUE) {
    DWORD error = GetLastError();
    free(absolute);
    CAMLreturn(dc_raw_operation_failure(dc_win32_error(error)));
  }
  dc_arm_handle(handle_value, root);
  (void)dc_take_test_injection(DC_INJECT_ROOT, &inject_action,
                               &inject_close);
  if (inject_action) {
    error_result = dc_injected_action_error(DC_INJECT_ROOT);
    free(absolute);
    CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                          inject_close));
  }
  display = dc_wtf8_encode(absolute, root_length);
  if (display == NULL) {
    error_result = dc_win32_error(ERROR_NOT_ENOUGH_MEMORY);
    free(absolute);
    CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                          inject_close));
  }
  if (!dc_strings_guard_push(display_guard, display)) {
    error_result = dc_win32_error(ERROR_NOT_ENOUGH_MEMORY);
    free(display);
    free(absolute);
    CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                          inject_close));
  }
  {
    int root_reparse = dc_reparse(root, &reparse_tag);
    if (root_reparse < 0) {
      DWORD error = GetLastError();
      error_result = dc_win32_error(error);
      dc_strings_guard_free(display_guard);
      free(absolute);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    if (root_reparse > 0) {
      error_result =
          dc_error(DC_WIN32, DC_LINK_LIKE, "root-is-reparse-point");
      dc_strings_guard_free(display_guard);
      free(absolute);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
  }
  start = root_length;
  while (absolute[start] == L'\\') ++start;
  while (absolute[start] != L'\0') {
    wchar_t *component;
    size_t component_length;
    char *encoded;
    index = start;
    while (absolute[index] != L'\0' && absolute[index] != L'\\') ++index;
    component_length = index - start;
    component = (wchar_t *)malloc((component_length + 1) * sizeof(wchar_t));
    if (component == NULL) goto windows_root_memory_error;
    memcpy(component, absolute + start, component_length * sizeof(wchar_t));
    component[component_length] = L'\0';
    encoded = dc_wtf8_encode(component, component_length);
    free(component);
    if (encoded == NULL || !dc_strings_guard_push(components_guard, encoded)) {
      free(encoded);
      goto windows_root_memory_error;
    }
    start = index;
    while (absolute[start] == L'\\') ++start;
  }
  free(absolute);
  components_value = dc_strings_guard_to_list(components_guard);
  display_value = caml_copy_string(display);
  payload = caml_alloc_tuple(3);
  Store_field(payload, 0, handle_value);
  Store_field(payload, 1, components_value);
  Store_field(payload, 2, display_value);
  dc_strings_guard_free(components_guard);
  dc_strings_guard_free(display_guard);
  result = dc_ok(payload);
  CAMLreturn(result);
windows_root_memory_error:
  error_result = dc_win32_error(ERROR_NOT_ENOUGH_MEMORY);
  dc_strings_guard_free(components_guard);
  dc_strings_guard_free(display_guard);
  free(absolute);
  CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                        inject_close));
#else
#if defined(O_DIRECTORY) && defined(O_NOFOLLOW) && defined(O_CLOEXEC)
  const char *path = String_val(path_value);
  size_t length = caml_string_length(path_value), index = 1, start;
  int root;
  if (length == 0 || path[0] != '/' || memchr(path, '\0', length) != NULL)
    CAMLreturn(dc_raw_operation_failure(
        dc_error(DC_POSIX, DC_INVALID_NAME, "path-is-not-absolute")));
  while (index < length) {
    char *component;
    while (index < length && path[index] == '/') ++index;
    if (index == length) break;
    start = index;
    while (index < length && path[index] != '/') ++index;
    if ((index - start == 1 && path[start] == '.') ||
        (index - start == 2 && path[start] == '.' && path[start + 1] == '.')) {
      dc_strings_guard_free(components_guard);
      CAMLreturn(dc_raw_operation_failure(dc_error(
          DC_POSIX, DC_INVALID_NAME, "dot-component-in-absolute-path")));
    }
    component = (char *)malloc(index - start + 1);
    if (component == NULL) {
      dc_strings_guard_free(components_guard);
      CAMLreturn(dc_raw_operation_failure(dc_posix_error(ENOMEM)));
    }
    memcpy(component, path + start, index - start);
    component[index - start] = '\0';
    if (!dc_strings_guard_push(components_guard, component)) {
      free(component);
      dc_strings_guard_free(components_guard);
      CAMLreturn(dc_raw_operation_failure(dc_posix_error(ENOMEM)));
    }
  }
  root = open("/", O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
  if (root < 0) {
    int error = errno;
    dc_strings_guard_free(components_guard);
    CAMLreturn(dc_raw_operation_failure(dc_posix_error(error)));
  }
  dc_arm_handle(handle_value, root);
  (void)dc_take_test_injection(DC_INJECT_ROOT, &inject_action,
                               &inject_close);
  if (inject_action) {
    error_result = dc_injected_action_error(DC_INJECT_ROOT);
    dc_strings_guard_free(components_guard);
    CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                          inject_close));
  }
  components_value = dc_strings_guard_to_list(components_guard);
  display_value = caml_copy_string("/");
  payload = caml_alloc_tuple(3);
  Store_field(payload, 0, handle_value);
  Store_field(payload, 1, components_value);
  Store_field(payload, 2, display_value);
  dc_strings_guard_free(components_guard);
  result = dc_ok(payload);
  CAMLreturn(result);
#else
  CAMLreturn(dc_raw_operation_failure(dc_error(
      DC_POSIX, DC_UNSUPPORTED, "openat-no-follow-primitives-unavailable")));
#endif
#endif
}

CAMLprim value ocaml_mutants_dircap_stat_handle(value handle_value) {
  CAMLparam1(handle_value);
  CAMLlocal2(stat_value, result);
  struct dc_handle *handle = (struct dc_handle *)Data_custom_val(handle_value);
  int problem;
  if (dc_handle_problem(handle, &problem))
    CAMLreturn(dc_error(
        DC_NATIVE_DOMAIN, problem, problem == DC_WRONG_PROCESS ? "wrong-process"
                                            : "closed-capability"));
#ifdef _WIN32
  {
    char identity[DC_IDENTITY_CAPACITY];
    int kind, permissions;
    int64_t size, mtime_ns;
    DWORD error = ERROR_SUCCESS;
    if (!dc_windows_stat(handle->os, 0, identity, sizeof(identity), &kind,
                         &size, &permissions, &mtime_ns, &error))
      CAMLreturn(dc_win32_error(error));
    stat_value =
        dc_alloc_raw_stat(identity, kind, size, permissions, mtime_ns);
  }
#else
  {
    struct stat stat;
    char identity[DC_IDENTITY_CAPACITY];
    int kind, permissions;
    int64_t size, mtime_ns;
    if (fstat(handle->os, &stat) != 0) CAMLreturn(dc_posix_error(errno));
    if (!dc_posix_stat_value(&stat, identity, sizeof(identity), &kind, &size,
                             &permissions, &mtime_ns))
      CAMLreturn(dc_posix_error(errno));
    stat_value =
        dc_alloc_raw_stat(identity, kind, size, permissions, mtime_ns);
  }
#endif
  result = dc_ok(stat_value);
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_dircap_open_child(value parent_value,
                                               value name_value,
                                               value mode_value) {
  CAMLparam3(parent_value, name_value, mode_value);
  CAMLlocal5(handle_value, stat_value, payload, result, error_result);
  struct dc_handle *parent = (struct dc_handle *)Data_custom_val(parent_value);
  int problem, mode = Int_val(mode_value);
  int inject_action = 0, inject_close = 0;
#ifdef _WIN32
  HANDLE parent_os;
#else
  int parent_os;
#endif
  if (dc_handle_problem(parent, &problem))
    CAMLreturn(dc_raw_operation_failure(dc_error(
        DC_NATIVE_DOMAIN, problem, problem == DC_WRONG_PROCESS ? "wrong-process"
                                            : "closed-capability")));
  if (mode < 0 || mode > 5)
    CAMLreturn(dc_raw_operation_failure(
        dc_error(DC_CONTRACT, DC_INVALID_NAME, "invalid-open-mode")));
  parent_os = parent->os;
  handle_value = dc_alloc_empty_handle();
#ifdef _WIN32
  {
    wchar_t *name = NULL;
    size_t name_length = 0;
    HANDLE child = INVALID_HANDLE_VALUE;
    NTSTATUS status;
    DWORD tag = 0, error = ERROR_SUCCESS;
    char identity[DC_IDENTITY_CAPACITY];
    int kind, permissions;
    int64_t size, mtime_ns;
    if (!dc_windows_component(String_val(name_value),
                              caml_string_length(name_value), &name,
                              &name_length))
      CAMLreturn(dc_raw_operation_failure(dc_error(
          DC_WIN32, DC_INVALID_NAME, "invalid-native-component")));
    status = mode == 0
                 ? dc_windows_open_directory(parent_os, name, name_length,
                                             FILE_OPEN, &child)
                 : dc_windows_open_relative(parent_os, name, name_length, mode,
                                            FILE_OPEN, &child);
    if (status < 0) {
      free(name);
      CAMLreturn(dc_raw_operation_failure(dc_nt_error(status)));
    }
    dc_arm_handle(handle_value, child);
    free(name);
    (void)dc_take_test_injection(DC_INJECT_CHILD, &inject_action,
                                 &inject_close);
    if (inject_action) {
      error_result = dc_injected_action_error(DC_INJECT_CHILD);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    problem = dc_reparse(child, &tag);
    if (problem < 0) {
      error = GetLastError();
      error_result = dc_win32_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    if (problem) {
      error_result = dc_error(DC_WIN32, DC_LINK_LIKE, "reparse-point");
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    if (!dc_windows_stat(child, 0, identity, sizeof(identity), &kind, &size,
                         &permissions, &mtime_ns, &error)) {
      error_result = dc_win32_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    if (((mode == 0 || mode == 5) && kind != 0) ||
        ((mode == 1 || mode == 3 || mode == 4) && kind != 1)) {
      error_result = dc_error(
          DC_WIN32, (mode == 0 || mode == 5) ? DC_NOT_DIRECTORY
                                             : DC_NOT_REGULAR,
          "entry-kind-mismatch");
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    stat_value =
        dc_alloc_raw_stat(identity, kind, size, permissions, mtime_ns);
  }
#else
#if defined(O_NOFOLLOW) && defined(O_CLOEXEC) && defined(O_DIRECTORY)
  {
    /* O_DIRECTORY is deliberately absent even for the directory modes: with
       O_NOFOLLOW it surfaces a link-like leaf as ENOTDIR on Linux, while the
       descriptor-relative kind check below classifies race-free. */
    int flags = O_RDONLY | O_NOFOLLOW | O_CLOEXEC | O_NONBLOCK;
    int child, current_flags;
    struct stat stat_buffer;
    struct stat parent_stat;
    char identity[DC_IDENTITY_CAPACITY];
    int kind, permissions;
    int64_t size, mtime_ns;
    (void)parent_stat;
#ifndef DC_HAVE_POSIX_FD_LINK_PUBLISH
    if (mode == 3)
      CAMLreturn(dc_raw_operation_failure(dc_error(
          DC_POSIX, DC_UNSUPPORTED, "immutable-publish-handle-unavailable")));
#endif
#ifndef DC_HAVE_POSIX_ENVELOPE_DELETE
    if (mode == 4 || mode == 5)
      CAMLreturn(dc_raw_operation_failure(dc_error(
          DC_POSIX, DC_UNSUPPORTED, "captured-delete-handle-unavailable")));
#endif
#ifdef DC_HAVE_POSIX_ENVELOPE_DELETE
    if (mode == 4 || mode == 5) {
      if (fstat(parent_os, &parent_stat) != 0)
        CAMLreturn(dc_raw_operation_failure(dc_posix_error(errno)));
      if (!dc_posix_owner_exclusive(&parent_stat))
        CAMLreturn(dc_raw_operation_failure(dc_error(
            DC_POSIX, DC_UNSUPPORTED, "owner-exclusive-parent-unproven")));
    }
#endif
    child = openat(parent_os, String_val(name_value), flags);
    if (child < 0)
      CAMLreturn(dc_raw_operation_failure(dc_posix_error(errno)));
    dc_arm_handle(handle_value, child);
    (void)dc_take_test_injection(DC_INJECT_CHILD, &inject_action,
                                 &inject_close);
    if (inject_action) {
      error_result = dc_injected_action_error(DC_INJECT_CHILD);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    if (fstat(child, &stat_buffer) != 0) {
      int error = errno;
      error_result = dc_posix_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    if (((mode == 0 || mode == 5) && !S_ISDIR(stat_buffer.st_mode)) ||
        ((mode == 1 || mode == 3 || mode == 4) &&
         !S_ISREG(stat_buffer.st_mode))) {
      error_result = dc_error(
          DC_POSIX, (mode == 0 || mode == 5) ? DC_NOT_DIRECTORY
                                             : DC_NOT_REGULAR,
          "entry-kind-mismatch");
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    current_flags = fcntl(child, F_GETFL);
    if (current_flags < 0 ||
        fcntl(child, F_SETFL, current_flags & ~O_NONBLOCK) < 0) {
      int error = errno;
      error_result = dc_posix_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
#ifdef DC_HAVE_POSIX_ENVELOPE_DELETE
    if ((mode == 4 || mode == 5) &&
        stat_buffer.st_dev != parent_stat.st_dev) {
      error_result = dc_error(DC_POSIX, DC_UNSUPPORTED,
                              "owner-exclusive-device-unproven");
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
#endif
#ifdef DC_HAVE_POSIX_FD_LINK_PUBLISH
    if (mode == 3) {
      char binding_path[64];
      struct stat binding_stat;
      int formatted = snprintf(binding_path, sizeof(binding_path),
                               "/proc/self/fd/%d", child);
      if (formatted < 0 || (size_t)formatted >= sizeof(binding_path)) {
        error_result = dc_posix_error(EOVERFLOW);
        CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                              inject_close));
      }
      if (fstatat(AT_FDCWD, binding_path, &binding_stat, 0) != 0 ||
          binding_stat.st_dev != stat_buffer.st_dev ||
          binding_stat.st_ino != stat_buffer.st_ino) {
        error_result = dc_error(DC_POSIX, DC_UNSUPPORTED,
                                "proc-fd-binding-unavailable");
        CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                              inject_close));
      }
    }
#endif
    if (!dc_posix_stat_value(&stat_buffer, identity, sizeof(identity), &kind,
                             &size, &permissions, &mtime_ns)) {
      int error = errno;
      error_result = dc_posix_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    stat_value =
        dc_alloc_raw_stat(identity, kind, size, permissions, mtime_ns);
  }
#else
  CAMLreturn(dc_raw_operation_failure(dc_error(
      DC_POSIX, DC_UNSUPPORTED, "openat-no-follow-primitives-unavailable")));
#endif
#endif
  payload = caml_alloc_tuple(2);
  Store_field(payload, 0, handle_value);
  Store_field(payload, 1, stat_value);
  result = dc_ok(payload);
  CAMLreturn(result);
}

static uint64_t dc_owner(void) {
#ifdef _WIN32
  return (uint64_t)GetCurrentProcessId();
#else
  return (uint64_t)getpid();
#endif
}

static int dc_close_os(struct dc_handle *handle) {
  if (handle->state != DC_HANDLE_OPEN) return 1;
#ifdef _WIN32
  if (!CloseHandle(handle->os)) return 0;
  handle->os = INVALID_HANDLE_VALUE;
  handle->state = DC_HANDLE_CLOSED;
#else
  if (close(handle->os) != 0) {
    handle->invalidated_error = errno;
    handle->os = -1;
    handle->state = DC_HANDLE_INVALIDATED_UNKNOWN;
    return 0;
  }
  handle->os = -1;
  handle->state = DC_HANDLE_CLOSED;
#endif
  return 1;
}

static void dc_finalize(value value_handle) {
  struct dc_handle *handle = (struct dc_handle *)Data_custom_val(value_handle);
  (void)dc_close_os(handle);
}

static struct custom_operations dc_handle_operations = {
    .identifier = "ocaml-mutants.dir-cap.handle.v1",
    .finalize = dc_finalize,
    .compare = custom_compare_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .compare_ext = custom_compare_ext_default,
    .fixed_length = custom_fixed_length_default};

static value dc_alloc_empty_handle(void) {
  value result = caml_alloc_custom(&dc_handle_operations, sizeof(struct dc_handle),
                                   0, 1);
  struct dc_handle *handle = (struct dc_handle *)Data_custom_val(result);
#ifdef _WIN32
  handle->os = INVALID_HANDLE_VALUE;
#else
  handle->os = -1;
#endif
  handle->owner = dc_owner();
  handle->state = DC_HANDLE_CLOSED;
  handle->invalidated_error = 0;
  return result;
}

static void dc_arm_handle(
    value handle_value,
#ifdef _WIN32
    HANDLE os
#else
    int os
#endif
) {
  struct dc_handle *handle =
      (struct dc_handle *)Data_custom_val(handle_value);
  handle->os = os;
  handle->owner = dc_owner();
  handle->state = DC_HANDLE_OPEN;
  handle->invalidated_error = 0;
}

static value dc_ok(value payload) {
  CAMLparam1(payload);
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, payload);
  CAMLreturn(result);
}

static value dc_error(int domain, int class_, const char *code) {
  CAMLparam0();
  CAMLlocal3(tuple, text, result);
  tuple = caml_alloc_tuple(3);
  Store_field(tuple, 0, Val_int(domain));
  Store_field(tuple, 1, Val_int(class_));
  text = caml_copy_string(code);
  Store_field(tuple, 2, text);
  result = caml_alloc(1, 1);
  Store_field(result, 0, tuple);
  CAMLreturn(result);
}

/* The three acquisition primitives use a richer error channel than the other
   raw calls.  OCaml representation:

     Raw_operation_error error                    tag 0, size 1
     Raw_cleanup_error (error, Invalidated_unknown) tag 1, size 2
     { raw_primary; raw_suppressed }               tuple, size 2

   An internal close is terminal: once attempted, the custom owner is never
   armed again and its finalizer cannot retry an ambiguous native close. */
static value dc_raw_operation_failure(value error_result) {
  CAMLparam1(error_result);
  CAMLlocal3(issue, failure, result);
  issue = caml_alloc(1, 0);
  Store_field(issue, 0, Field(error_result, 0));
  failure = caml_alloc_tuple(2);
  Store_field(failure, 0, issue);
  Store_field(failure, 1, Val_emptylist);
  result = caml_alloc(1, 1);
  Store_field(result, 0, failure);
  CAMLreturn(result);
}

static value dc_raw_operation_cleanup_failure(value operation_error_result,
                                              value cleanup_error_result) {
  CAMLparam2(operation_error_result, cleanup_error_result);
  CAMLlocal5(operation_issue, cleanup_issue, suppressed, failure, result);
  operation_issue = caml_alloc(1, 0);
  Store_field(operation_issue, 0, Field(operation_error_result, 0));
  cleanup_issue = caml_alloc(2, 1);
  Store_field(cleanup_issue, 0, Field(cleanup_error_result, 0));
  Store_field(cleanup_issue, 1, Val_int(DC_HANDLE_INVALIDATED_UNKNOWN));
  suppressed = caml_alloc(2, 0);
  Store_field(suppressed, 0, cleanup_issue);
  Store_field(suppressed, 1, Val_emptylist);
  failure = caml_alloc_tuple(2);
  Store_field(failure, 0, operation_issue);
  Store_field(failure, 1, suppressed);
  result = caml_alloc(1, 1);
  Store_field(result, 0, failure);
  CAMLreturn(result);
}

static value dc_raw_cleanup_failure(value cleanup_error_result) {
  CAMLparam1(cleanup_error_result);
  CAMLlocal3(issue, failure, result);
  issue = caml_alloc(2, 1);
  Store_field(issue, 0, Field(cleanup_error_result, 0));
  Store_field(issue, 1, Val_int(DC_HANDLE_INVALIDATED_UNKNOWN));
  failure = caml_alloc_tuple(2);
  Store_field(failure, 0, issue);
  Store_field(failure, 1, Val_emptylist);
  result = caml_alloc(1, 1);
  Store_field(result, 0, failure);
  CAMLreturn(result);
}

/* Creation has an explicit commit boundary.  Before that boundary the error
   channel contains only a raw operation error.  After it, the richer failure
   keeps terminal cleanup evidence and the OCaml layer reports
   Creation_may_have_committed. */
static value dc_raw_create_not_committed(value error_result) {
  CAMLparam1(error_result);
  CAMLlocal2(commit_evidence, result);
  commit_evidence = caml_alloc(1, 0);
  Store_field(commit_evidence, 0, Field(error_result, 0));
  result = caml_alloc(1, 1);
  Store_field(result, 0, commit_evidence);
  CAMLreturn(result);
}

static value dc_raw_create_may_have_committed(value failure_result) {
  CAMLparam1(failure_result);
  CAMLlocal2(commit_evidence, result);
  commit_evidence = caml_alloc(1, 1);
  Store_field(commit_evidence, 0, Field(failure_result, 0));
  result = caml_alloc(1, 1);
  Store_field(result, 0, commit_evidence);
  CAMLreturn(result);
}

/* Captured deletion also has an explicit commit boundary.  Once the native
   delete disposition is installed, every failure additionally carries the
   caller-visible target-handle state and whether closing that exact handle
   proved the pinned namespace entry was released. */
static value dc_raw_delete_not_committed(value error_result) {
  CAMLparam1(error_result);
  CAMLlocal2(commit_evidence, result);
  commit_evidence = caml_alloc(1, 0);
  Store_field(commit_evidence, 0, Field(error_result, 0));
  result = caml_alloc(1, 1);
  Store_field(result, 0, commit_evidence);
  CAMLreturn(result);
}

static value dc_raw_delete_may_have_committed(
    int local_handle_state, int namespace_released, value failure_result) {
  CAMLparam1(failure_result);
  CAMLlocal2(commit_evidence, result);
  commit_evidence = caml_alloc(3, 1);
  Store_field(commit_evidence, 0, Val_int(local_handle_state));
  Store_field(commit_evidence, 1, Val_bool(namespace_released));
  Store_field(commit_evidence, 2, Field(failure_result, 0));
  result = caml_alloc(1, 1);
  Store_field(result, 0, commit_evidence);
  CAMLreturn(result);
}

/* Lock release cleanup problems additionally carry whether the lock
   namespace was proven released.  The result payload is
   ((error, state, namespace_released), suppressed-problems). */
static value dc_lock_cleanup_failure(
    value primary_error_result, int primary_state,
    int primary_namespace_released, int has_suppressed,
    value suppressed_error_result, int suppressed_state,
    int suppressed_namespace_released) {
  CAMLparam2(primary_error_result, suppressed_error_result);
  CAMLlocal5(primary_problem, suppressed_problem, suppressed, failure, result);
  primary_problem = caml_alloc_tuple(3);
  Store_field(primary_problem, 0, Field(primary_error_result, 0));
  Store_field(primary_problem, 1, Val_int(primary_state));
  Store_field(primary_problem, 2, Val_bool(primary_namespace_released));
  suppressed = Val_emptylist;
  if (has_suppressed) {
    suppressed_problem = caml_alloc_tuple(3);
    Store_field(suppressed_problem, 0, Field(suppressed_error_result, 0));
    Store_field(suppressed_problem, 1, Val_int(suppressed_state));
    Store_field(suppressed_problem, 2,
                Val_bool(suppressed_namespace_released));
    suppressed = caml_alloc(2, 0);
    Store_field(suppressed, 0, suppressed_problem);
    Store_field(suppressed, 1, Val_emptylist);
  }
  failure = caml_alloc_tuple(2);
  Store_field(failure, 0, primary_problem);
  Store_field(failure, 1, suppressed);
  result = caml_alloc(1, 1);
  Store_field(result, 0, failure);
  CAMLreturn(result);
}

static value dc_internal_close_terminal(value handle_value,
                                        int inject_failure) {
  CAMLparam1(handle_value);
  CAMLlocal1(result);
  struct dc_handle *handle =
      (struct dc_handle *)Data_custom_val(handle_value);
  int close_succeeded;
#ifdef _WIN32
  DWORD native_error = ERROR_SUCCESS;
  close_succeeded = CloseHandle(handle->os) != 0;
  if (!close_succeeded) native_error = GetLastError();
  handle->os = INVALID_HANDLE_VALUE;
#else
  int native_error = 0;
  close_succeeded = close(handle->os) == 0;
  if (!close_succeeded) native_error = errno;
  handle->os = -1;
#endif
  if (inject_failure) {
    handle->state = DC_HANDLE_INVALIDATED_UNKNOWN;
#ifdef _WIN32
    handle->invalidated_error = ERROR_GEN_FAILURE;
#else
    handle->invalidated_error = EIO;
#endif
    CAMLreturn(dc_error(DC_CONTRACT, DC_OTHER,
                        "injected-internal-close-failure"));
  }
  if (!close_succeeded) {
    handle->state = DC_HANDLE_INVALIDATED_UNKNOWN;
    handle->invalidated_error = (int)native_error;
#ifdef _WIN32
    CAMLreturn(dc_win32_error(native_error));
#else
    CAMLreturn(dc_posix_error(native_error));
#endif
  }
  handle->state = DC_HANDLE_CLOSED;
  handle->invalidated_error = 0;
  result = dc_ok(Val_unit);
  CAMLreturn(result);
}

static value dc_internal_action_failure(value handle_value,
                                        value operation_error_result,
                                        int inject_close_failure) {
  CAMLparam2(handle_value, operation_error_result);
  CAMLlocal1(cleanup_result);
  cleanup_result =
      dc_internal_close_terminal(handle_value, inject_close_failure);
  if (Tag_val(cleanup_result) == 0)
    CAMLreturn(dc_raw_operation_failure(operation_error_result));
  CAMLreturn(dc_raw_operation_cleanup_failure(operation_error_result,
                                              cleanup_result));
}

static int dc_take_test_injection(enum dc_injection_site site,
                                  int *inject_action,
                                  int *inject_close) {
  unsigned request;
  unsigned expected_site = ((unsigned)site + 1u) << DC_INJECT_SITE_SHIFT;
  for (;;) {
    request = atomic_load_explicit(&dc_test_injection, memory_order_acquire);
    if ((request & ~(DC_INJECT_ACTION | DC_INJECT_CLOSE)) != expected_site)
      return 0;
    if (atomic_compare_exchange_weak_explicit(
            &dc_test_injection, &request, 0u, memory_order_acq_rel,
            memory_order_acquire)) {
      *inject_action = (request & DC_INJECT_ACTION) != 0;
      *inject_close = (request & DC_INJECT_CLOSE) != 0;
      return 1;
    }
  }
}

static value dc_injected_action_error(enum dc_injection_site site) {
  switch (site) {
    case DC_INJECT_ROOT:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-post-acquire-root-failure");
    case DC_INJECT_CHILD:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-post-acquire-child-failure");
    case DC_INJECT_PROBE:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-post-acquire-probe-failure");
    case DC_INJECT_ENUMERATE:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-enumeration-action-failure");
    case DC_INJECT_LOCK_OPEN:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-lock-open-failure");
    case DC_INJECT_LOCK_ACQUIRE:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-lock-acquire-failure");
    case DC_INJECT_LOCK_RELEASE:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-lock-unlock-failure");
    case DC_INJECT_PUBLISH:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-publish-before-commit");
    case DC_INJECT_CREATE_BEFORE_COMMIT:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-create-before-commit");
    case DC_INJECT_CREATE_AFTER_COMMIT:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-create-after-commit");
    case DC_INJECT_CREATE_FILE_BEFORE_COMMIT:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-create-file-before-commit");
    case DC_INJECT_CREATE_FILE_AFTER_COMMIT:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-create-file-after-commit");
    case DC_INJECT_DELETE_BEFORE_COMMIT:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-delete-before-commit");
    case DC_INJECT_DELETE_AFTER_COMMIT:
      return dc_error(DC_CONTRACT, DC_OTHER,
                      "injected-delete-after-commit");
  }
  return dc_error(DC_CONTRACT, DC_OTHER, "invalid-injection-site");
}

static int dc_injection_site_of_name(const char *name, size_t length,
                                     enum dc_injection_site *result) {
  static const struct {
    const char *name;
    size_t length;
    enum dc_injection_site site;
  } sites[] = {
      {"root", sizeof("root") - 1, DC_INJECT_ROOT},
      {"child", sizeof("child") - 1, DC_INJECT_CHILD},
      {"probe", sizeof("probe") - 1, DC_INJECT_PROBE},
      {"enumerate", sizeof("enumerate") - 1, DC_INJECT_ENUMERATE},
      {"lock-open", sizeof("lock-open") - 1, DC_INJECT_LOCK_OPEN},
      {"lock-acquire", sizeof("lock-acquire") - 1,
       DC_INJECT_LOCK_ACQUIRE},
      {"lock-release", sizeof("lock-release") - 1,
       DC_INJECT_LOCK_RELEASE},
      {"publish", sizeof("publish") - 1, DC_INJECT_PUBLISH},
      {"create-directory-before-commit",
       sizeof("create-directory-before-commit") - 1,
       DC_INJECT_CREATE_BEFORE_COMMIT},
      {"create-directory-after-commit",
       sizeof("create-directory-after-commit") - 1,
       DC_INJECT_CREATE_AFTER_COMMIT},
      {"create-file-before-commit", sizeof("create-file-before-commit") - 1,
       DC_INJECT_CREATE_FILE_BEFORE_COMMIT},
      {"create-file-after-commit", sizeof("create-file-after-commit") - 1,
       DC_INJECT_CREATE_FILE_AFTER_COMMIT},
      {"delete-before-commit", sizeof("delete-before-commit") - 1,
       DC_INJECT_DELETE_BEFORE_COMMIT},
      {"delete-after-commit", sizeof("delete-after-commit") - 1,
       DC_INJECT_DELETE_AFTER_COMMIT},
  };
  size_t index;
  for (index = 0; index < sizeof(sites) / sizeof(sites[0]); ++index) {
    if (length == sites[index].length &&
        memcmp(name, sites[index].name, length) == 0) {
      *result = sites[index].site;
      return 1;
    }
  }
  return 0;
}

CAMLprim value ocaml_mutants_dircap_test_inject(value site_value,
                                                value action_value,
                                                value close_value) {
  CAMLparam3(site_value, action_value, close_value);
  enum dc_injection_site site;
  unsigned request;
  if (!Bool_val(action_value) && !Bool_val(close_value)) {
    atomic_store_explicit(&dc_test_injection, 0u, memory_order_release);
    CAMLreturn(Val_unit);
  }
  if (!dc_injection_site_of_name(String_val(site_value),
                                 caml_string_length(site_value), &site))
    caml_invalid_argument("dir-cap injection site");
  request = ((unsigned)site + 1u) << DC_INJECT_SITE_SHIFT;
  if (Bool_val(action_value)) request |= DC_INJECT_ACTION;
  if (Bool_val(close_value)) request |= DC_INJECT_CLOSE;
  atomic_store_explicit(&dc_test_injection, request, memory_order_release);
  CAMLreturn(Val_unit);
}

static value dc_error_number(int domain, int class_, uint64_t code) {
  char buffer[48];
  if (domain == DC_NTSTATUS)
    snprintf(buffer, sizeof(buffer), "ntstatus:0x%08" PRIx64, code);
  else if (domain == DC_WIN32)
    snprintf(buffer, sizeof(buffer), "win32:%" PRIu64, code);
  else
    snprintf(buffer, sizeof(buffer), "errno:%" PRIu64, code);
  return dc_error(domain, class_, buffer);
}

static value dc_close_error(value error_result, int local_handle_state) {
  CAMLparam1(error_result);
  CAMLlocal2(payload, result);
  payload = caml_alloc_tuple(2);
  Store_field(payload, 0, Field(error_result, 0));
  Store_field(payload, 1, Val_int(local_handle_state));
  result = caml_alloc(1, 1);
  Store_field(result, 0, payload);
  CAMLreturn(result);
}

static value dc_enumeration_failure(value failure_result, uint64_t entries,
                                    uint64_t native_name_bytes) {
  CAMLparam1(failure_result);
  CAMLlocal4(payload, entries_value, bytes_value, result);
  entries_value = caml_copy_int64((int64_t)entries);
  bytes_value = caml_copy_int64((int64_t)native_name_bytes);
  payload = caml_alloc_tuple(3);
  Store_field(payload, 0, Field(failure_result, 0));
  Store_field(payload, 1, entries_value);
  Store_field(payload, 2, bytes_value);
  result = caml_alloc(1, 1);
  Store_field(result, 0, payload);
  CAMLreturn(result);
}

static int dc_strings_push(struct dc_strings *strings, char *owned) {
  if (strings->length == strings->capacity) {
    size_t next = strings->capacity == 0 ? 8 : strings->capacity * 2;
    if (next < strings->capacity || next > SIZE_MAX / sizeof(char *)) return 0;
    char **items = (char **)realloc(strings->items, next * sizeof(char *));
    if (items == NULL) return 0;
    strings->items = items;
    strings->capacity = next;
  }
  strings->items[strings->length++] = owned;
  return 1;
}

static void dc_strings_free(struct dc_strings *strings) {
  size_t index;
  for (index = 0; index < strings->length; ++index) free(strings->items[index]);
  free(strings->items);
  strings->items = NULL;
  strings->length = 0;
  strings->capacity = 0;
}

static void dc_strings_guard_finalize(value guard_value) {
  struct dc_strings_guard *guard =
      (struct dc_strings_guard *)Data_custom_val(guard_value);
  dc_strings_free(&guard->strings);
}

static struct custom_operations dc_strings_guard_operations = {
    .identifier = "ocaml-mutants.dir-cap.strings-guard.v1",
    .finalize = dc_strings_guard_finalize,
    .compare = custom_compare_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .compare_ext = custom_compare_ext_default,
    .fixed_length = custom_fixed_length_default};

static value dc_alloc_strings_guard(void) {
  value result =
      caml_alloc_custom(&dc_strings_guard_operations,
                        sizeof(struct dc_strings_guard), 0, 1);
  struct dc_strings_guard *guard =
      (struct dc_strings_guard *)Data_custom_val(result);
  memset(&guard->strings, 0, sizeof(guard->strings));
  return result;
}

static struct dc_strings *dc_guarded_strings(value guard_value) {
  struct dc_strings_guard *guard =
      (struct dc_strings_guard *)Data_custom_val(guard_value);
  return &guard->strings;
}

/* Pointers returned by Data_custom_val/String_val point into the movable OCaml
   heap.  They are valid only until the next OCaml allocation, callback, or
   runtime release.  Helpers that can allocate therefore take rooted values and
   re-derive custom-data pointers; callers copy native handles or C-heap
   pointers before crossing such a boundary. */
static int dc_strings_guard_push(value guard_value, char *owned) {
  return dc_strings_push(dc_guarded_strings(guard_value), owned);
}

static void dc_strings_guard_free(value guard_value) {
  dc_strings_free(dc_guarded_strings(guard_value));
}

static void dc_buffer_guard_finalize(value guard_value) {
  struct dc_buffer_guard *guard =
      (struct dc_buffer_guard *)Data_custom_val(guard_value);
  free(guard->buffer);
  guard->buffer = NULL;
}

static struct custom_operations dc_buffer_guard_operations = {
    .identifier = "ocaml-mutants.dir-cap.buffer-guard.v1",
    .finalize = dc_buffer_guard_finalize,
    .compare = custom_compare_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .compare_ext = custom_compare_ext_default,
    .fixed_length = custom_fixed_length_default};

static value dc_alloc_buffer_guard(void) {
  value result = caml_alloc_custom(&dc_buffer_guard_operations,
                                   sizeof(struct dc_buffer_guard), 0, 1);
  struct dc_buffer_guard *guard =
      (struct dc_buffer_guard *)Data_custom_val(result);
  guard->buffer = NULL;
  return result;
}

#ifdef _WIN32
static void dc_buffer_guard_arm(value guard_value, void *buffer) {
  struct dc_buffer_guard *guard =
      (struct dc_buffer_guard *)Data_custom_val(guard_value);
  guard->buffer = buffer;
}
#endif

static void dc_buffer_guard_release(value guard_value) {
  struct dc_buffer_guard *guard =
      (struct dc_buffer_guard *)Data_custom_val(guard_value);
  free(guard->buffer);
  guard->buffer = NULL;
}

#ifdef DC_HAVE_POSIX_ENUMERATION
static void dc_directory_stream_guard_finalize(value guard_value) {
  struct dc_directory_stream_guard *guard =
      (struct dc_directory_stream_guard *)Data_custom_val(guard_value);
  if (guard->stream != NULL) (void)closedir(guard->stream);
  guard->stream = NULL;
}

static struct custom_operations dc_directory_stream_guard_operations = {
    .identifier = "ocaml-mutants.dir-cap.directory-stream-guard.v1",
    .finalize = dc_directory_stream_guard_finalize,
    .compare = custom_compare_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .compare_ext = custom_compare_ext_default,
    .fixed_length = custom_fixed_length_default};

static value dc_alloc_directory_stream_guard(void) {
  value result =
      caml_alloc_custom(&dc_directory_stream_guard_operations,
                        sizeof(struct dc_directory_stream_guard), 0, 1);
  struct dc_directory_stream_guard *guard =
      (struct dc_directory_stream_guard *)Data_custom_val(result);
  guard->stream = NULL;
  return result;
}

static void dc_directory_stream_guard_arm(value guard_value, DIR *stream) {
  struct dc_directory_stream_guard *guard =
      (struct dc_directory_stream_guard *)Data_custom_val(guard_value);
  guard->stream = stream;
}

static value dc_directory_stream_close_terminal(value guard_value,
                                                int inject_failure) {
  CAMLparam1(guard_value);
  CAMLlocal1(result);
  struct dc_directory_stream_guard *guard =
      (struct dc_directory_stream_guard *)Data_custom_val(guard_value);
  DIR *stream = guard->stream;
  int close_succeeded;
  int native_error = 0;
  if (stream == NULL) CAMLreturn(dc_ok(Val_unit));
  close_succeeded = closedir(stream) == 0;
  if (!close_succeeded) native_error = errno;
  guard->stream = NULL;
  if (inject_failure)
    CAMLreturn(dc_error(DC_CONTRACT, DC_OTHER,
                        "injected-enumeration-close-failure"));
  if (!close_succeeded) CAMLreturn(dc_posix_error(native_error));
  result = dc_ok(Val_unit);
  CAMLreturn(result);
}

static void dc_handle_transfer_to_stream(value handle_value) {
  struct dc_handle *handle =
      (struct dc_handle *)Data_custom_val(handle_value);
  handle->os = -1;
  handle->state = DC_HANDLE_CLOSED;
  handle->invalidated_error = 0;
}
#endif

static value dc_strings_guard_to_list(value guard_value) {
  CAMLparam1(guard_value);
  CAMLlocal3(list, cell, item);
  size_t index = dc_guarded_strings(guard_value)->length;
  list = Val_emptylist;
  while (index > 0) {
    char *owned;
    --index;
    owned = dc_guarded_strings(guard_value)->items[index];
    item = caml_copy_string(owned);
    cell = caml_alloc(2, 0);
    Store_field(cell, 0, item);
    Store_field(cell, 1, list);
    list = cell;
  }
  CAMLreturn(list);
}

static value dc_some(value payload) {
  CAMLparam1(payload);
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, payload);
  CAMLreturn(result);
}

static int dc_handle_problem(struct dc_handle *handle, int *class_) {
  if (handle->owner != dc_owner()) {
    *class_ = DC_WRONG_PROCESS;
    return 1;
  }
  if (handle->state != DC_HANDLE_OPEN) {
    *class_ = DC_CLOSED;
    return 1;
  }
  return 0;
}

#ifdef _WIN32

typedef NTSTATUS(NTAPI *dc_nt_create_file_fn)(
    PHANDLE, ACCESS_MASK, POBJECT_ATTRIBUTES, PIO_STATUS_BLOCK, PLARGE_INTEGER,
    ULONG, ULONG, ULONG, ULONG, PVOID, ULONG);
typedef NTSTATUS(NTAPI *dc_nt_read_file_fn)(
    HANDLE, HANDLE, PIO_APC_ROUTINE, PVOID, PIO_STATUS_BLOCK, PVOID, ULONG,
    PLARGE_INTEGER, PULONG);
typedef NTSTATUS(NTAPI *dc_nt_set_information_file_fn)(
    HANDLE, PIO_STATUS_BLOCK, PVOID, ULONG, FILE_INFORMATION_CLASS);
typedef ULONG(WINAPI *dc_rtl_nt_status_to_dos_error_fn)(NTSTATUS);

/* MinGW's user-mode winternl.h stops its FILE_INFORMATION_CLASS enum before
   the native FileRenameInformationEx member.  The current DDK enum assigns
   that documented native member value 65.  Keep the compatibility value named
   and isolated here rather than leaking a bare ABI number into the call. */
#define DC_FILE_RENAME_INFORMATION_EX_CLASS ((FILE_INFORMATION_CLASS)65)

/* FILE_RENAME_INFORMATION_EX uses the native FILE_RENAME_* flag names.  The
   public Win32 headers expose the same ABI value under FILE_RENAME_FLAG_* on
   some SDK versions, so keep that compatibility mapping named and isolated. */
#if defined(FILE_RENAME_REPLACE_IF_EXISTS)
#define DC_FILE_RENAME_REPLACE_IF_EXISTS FILE_RENAME_REPLACE_IF_EXISTS
#elif defined(FILE_RENAME_FLAG_REPLACE_IF_EXISTS)
#define DC_FILE_RENAME_REPLACE_IF_EXISTS FILE_RENAME_FLAG_REPLACE_IF_EXISTS
#else
#define DC_FILE_RENAME_REPLACE_IF_EXISTS ((ULONG)0x00000001u)
#endif

static INIT_ONCE dc_nt_create_file_once = INIT_ONCE_STATIC_INIT;
static dc_nt_create_file_fn dc_nt_create_file_pointer = NULL;
static dc_nt_read_file_fn dc_nt_read_file_pointer = NULL;
static dc_nt_set_information_file_fn dc_nt_set_information_file_pointer = NULL;
static dc_rtl_nt_status_to_dos_error_fn
    dc_rtl_nt_status_to_dos_error_pointer = NULL;

static BOOL CALLBACK dc_load_nt_create_file(PINIT_ONCE once, PVOID parameter,
                                            PVOID *context) {
  HMODULE ntdll = GetModuleHandleW(L"ntdll.dll");
  FARPROC address = NULL;
  FARPROC read_address = NULL;
  FARPROC set_information_address = NULL;
  FARPROC rtl_address = NULL;
  (void)once;
  (void)parameter;
  (void)context;
  if (ntdll != NULL) address = GetProcAddress(ntdll, "NtCreateFile");
  if (ntdll != NULL) read_address = GetProcAddress(ntdll, "NtReadFile");
  if (ntdll != NULL)
    set_information_address =
        GetProcAddress(ntdll, "NtSetInformationFile");
  if (ntdll != NULL)
    rtl_address = GetProcAddress(ntdll, "RtlNtStatusToDosError");
  if (address != NULL) {
    _Static_assert(sizeof(address) == sizeof(dc_nt_create_file_pointer),
                   "function pointer sizes differ");
    memcpy(&dc_nt_create_file_pointer, &address, sizeof(address));
  }
  if (read_address != NULL) {
    _Static_assert(sizeof(read_address) == sizeof(dc_nt_read_file_pointer),
                   "function pointer sizes differ");
    memcpy(&dc_nt_read_file_pointer, &read_address, sizeof(read_address));
  }
  if (set_information_address != NULL) {
    _Static_assert(
        sizeof(set_information_address) ==
            sizeof(dc_nt_set_information_file_pointer),
        "function pointer sizes differ");
    memcpy(&dc_nt_set_information_file_pointer, &set_information_address,
           sizeof(set_information_address));
  }
  if (rtl_address != NULL) {
    _Static_assert(
        sizeof(rtl_address) == sizeof(dc_rtl_nt_status_to_dos_error_pointer),
        "function pointer sizes differ");
    memcpy(&dc_rtl_nt_status_to_dos_error_pointer, &rtl_address,
           sizeof(rtl_address));
  }
  return TRUE;
}

static dc_nt_create_file_fn dc_nt_create_file(void) {
  if (!InitOnceExecuteOnce(&dc_nt_create_file_once, dc_load_nt_create_file,
                           NULL, NULL))
    return NULL;
  return dc_nt_create_file_pointer;
}

static dc_nt_read_file_fn dc_nt_read_file(void) {
  if (!InitOnceExecuteOnce(&dc_nt_create_file_once, dc_load_nt_create_file,
                           NULL, NULL))
    return NULL;
  return dc_nt_read_file_pointer;
}

static dc_nt_set_information_file_fn dc_nt_set_information_file(void) {
  if (!InitOnceExecuteOnce(&dc_nt_create_file_once, dc_load_nt_create_file,
                           NULL, NULL))
    return NULL;
  return dc_nt_set_information_file_pointer;
}

static NTSTATUS dc_windows_read_at(HANDLE handle, void *buffer, ULONG amount,
                                   uint64_t offset, ULONG *read_count) {
  dc_nt_read_file_fn read_file = dc_nt_read_file();
  IO_STATUS_BLOCK io;
  LARGE_INTEGER byte_offset;
  NTSTATUS status;
  if (read_file == NULL || offset > INT64_MAX)
    return (NTSTATUS)0xC00000BBU; /* STATUS_NOT_SUPPORTED */
  byte_offset.QuadPart = (LONGLONG)offset;
  io.Status = 0;
  io.Information = 0;
  status = read_file(handle, NULL, NULL, NULL, &io, buffer, amount,
                     &byte_offset, NULL);
  if ((uint32_t)status == 0xC0000011U) { /* STATUS_END_OF_FILE */
    *read_count = 0;
    return 0;
  }
  if (status != 0) return status;
  if (io.Information > amount)
    return (NTSTATUS)0xC00000E5U; /* STATUS_INTERNAL_ERROR */
  *read_count = (ULONG)io.Information;
  return 0;
}

static int dc_win32_class(DWORD code) {
  switch (code) {
    case ERROR_FILE_NOT_FOUND:
    case ERROR_PATH_NOT_FOUND:
      return DC_MISSING;
    case ERROR_ALREADY_EXISTS:
    case ERROR_FILE_EXISTS:
      return DC_ALREADY_EXISTS;
    case ERROR_DIRECTORY:
      return DC_NOT_DIRECTORY;
    case ERROR_ACCESS_DENIED:
    case ERROR_PRIVILEGE_NOT_HELD:
      return DC_ACCESS_DENIED;
    case ERROR_SHARING_VIOLATION:
    case ERROR_LOCK_VIOLATION:
    case ERROR_DIR_NOT_EMPTY:
      return DC_BUSY;
    case ERROR_NOT_SUPPORTED:
    case ERROR_CALL_NOT_IMPLEMENTED:
      return DC_UNSUPPORTED;
    case ERROR_INVALID_NAME:
    case ERROR_BAD_PATHNAME:
    case ERROR_FILENAME_EXCED_RANGE:
      return DC_INVALID_NAME;
    default:
      return DC_OTHER;
  }
}

static int dc_nt_class(NTSTATUS status) {
  uint32_t code = (uint32_t)status;
  switch (code) {
    case 0xC0000034U: /* STATUS_OBJECT_NAME_NOT_FOUND */
    case 0xC000003AU: /* STATUS_OBJECT_PATH_NOT_FOUND */
      return DC_MISSING;
    case 0xC0000035U: /* STATUS_OBJECT_NAME_COLLISION */
      return DC_ALREADY_EXISTS;
    case 0xC0000103U: /* STATUS_NOT_A_DIRECTORY */
      return DC_NOT_DIRECTORY;
    case 0xC00000BAU: /* STATUS_FILE_IS_A_DIRECTORY */
      return DC_NOT_REGULAR;
    case 0xC0000022U: /* STATUS_ACCESS_DENIED */
      return DC_ACCESS_DENIED;
    case 0xC0000275U: /* STATUS_IO_REPARSE_TAG_NOT_HANDLED */
    case 0xC000050BU: /* STATUS_REPARSE_POINT_ENCOUNTERED */
      return DC_LINK_LIKE;
    default:
      if (dc_nt_create_file() != NULL &&
          dc_rtl_nt_status_to_dos_error_pointer != NULL)
        return dc_win32_class(dc_rtl_nt_status_to_dos_error_pointer(status));
      return DC_OTHER;
  }
}

static value dc_win32_error(DWORD code) {
  return dc_error_number(DC_WIN32, dc_win32_class(code), (uint64_t)code);
}

static value dc_nt_error(NTSTATUS status) {
  return dc_error_number(DC_NTSTATUS, dc_nt_class(status),
                         (uint64_t)(uint32_t)status);
}

/* Windows permits unpaired UTF-16 code units in directory entries. WTF-8
   preserves those names while remaining ordinary UTF-8 for scalar values. */
static int dc_wtf8_decode(const char *source, size_t source_length,
                          wchar_t **destination, size_t *destination_length) {
  size_t input = 0, output = 0, capacity;
  if (source_length > SIZE_MAX / sizeof(wchar_t) - 1) return 0;
  capacity = source_length + 1;
  wchar_t *wide = (wchar_t *)malloc(capacity * sizeof(wchar_t));
  if (wide == NULL) return 0;
  while (input < source_length) {
    uint32_t scalar;
    unsigned char first = (unsigned char)source[input++];
    if (first < 0x80) {
      scalar = first;
    } else if (first >= 0xC2 && first <= 0xDF && input < source_length) {
      unsigned char second = (unsigned char)source[input++];
      if ((second & 0xC0) != 0x80) goto invalid;
      scalar = ((uint32_t)(first & 0x1F) << 6) | (second & 0x3F);
    } else if (first >= 0xE0 && first <= 0xEF && input + 1 < source_length) {
      unsigned char second = (unsigned char)source[input++];
      unsigned char third = (unsigned char)source[input++];
      if ((second & 0xC0) != 0x80 || (third & 0xC0) != 0x80 ||
          (first == 0xE0 && second < 0xA0))
        goto invalid;
      scalar = ((uint32_t)(first & 0x0F) << 12) |
               ((uint32_t)(second & 0x3F) << 6) | (third & 0x3F);
    } else if (first >= 0xF0 && first <= 0xF4 && input + 2 < source_length) {
      unsigned char second = (unsigned char)source[input++];
      unsigned char third = (unsigned char)source[input++];
      unsigned char fourth = (unsigned char)source[input++];
      if ((second & 0xC0) != 0x80 || (third & 0xC0) != 0x80 ||
          (fourth & 0xC0) != 0x80 || (first == 0xF0 && second < 0x90) ||
          (first == 0xF4 && second > 0x8F))
        goto invalid;
      scalar = ((uint32_t)(first & 0x07) << 18) |
               ((uint32_t)(second & 0x3F) << 12) |
               ((uint32_t)(third & 0x3F) << 6) | (fourth & 0x3F);
    } else {
      goto invalid;
    }
    if (scalar == 0) goto invalid;
    if (scalar <= 0xFFFF) {
      wide[output++] = (wchar_t)scalar;
    } else {
      scalar -= 0x10000;
      wide[output++] = (wchar_t)(0xD800 + (scalar >> 10));
      wide[output++] = (wchar_t)(0xDC00 + (scalar & 0x3FF));
    }
  }
  wide[output] = L'\0';
  *destination = wide;
  *destination_length = output;
  return 1;
invalid:
  free(wide);
  return 0;
}

static char *dc_wtf8_encode(const wchar_t *source, size_t length) {
  size_t index = 0, output = 0;
  if (length > (SIZE_MAX - 1) / 3) return NULL;
  char *result = (char *)malloc(length * 3 + 1);
  if (result == NULL) return NULL;
  while (index < length) {
    uint32_t scalar = (uint16_t)source[index++];
    if (scalar >= 0xD800 && scalar <= 0xDBFF && index < length) {
      uint32_t low = (uint16_t)source[index];
      if (low >= 0xDC00 && low <= 0xDFFF) {
        ++index;
        scalar = 0x10000 + ((scalar - 0xD800) << 10) + (low - 0xDC00);
      }
    }
    if (scalar < 0x80) {
      result[output++] = (char)scalar;
    } else if (scalar < 0x800) {
      result[output++] = (char)(0xC0 | (scalar >> 6));
      result[output++] = (char)(0x80 | (scalar & 0x3F));
    } else if (scalar < 0x10000) {
      result[output++] = (char)(0xE0 | (scalar >> 12));
      result[output++] = (char)(0x80 | ((scalar >> 6) & 0x3F));
      result[output++] = (char)(0x80 | (scalar & 0x3F));
    } else {
      result[output++] = (char)(0xF0 | (scalar >> 18));
      result[output++] = (char)(0x80 | ((scalar >> 12) & 0x3F));
      result[output++] = (char)(0x80 | ((scalar >> 6) & 0x3F));
      result[output++] = (char)(0x80 | (scalar & 0x3F));
    }
  }
  result[output] = '\0';
  return result;
}

static int dc_windows_component(const char *bytes, size_t length,
                                wchar_t **wide, size_t *wide_length) {
  size_t index;
  if (length == 0 || !dc_wtf8_decode(bytes, length, wide, wide_length)) return 0;
  if (*wide_length > USHRT_MAX / sizeof(wchar_t)) {
    free(*wide);
    return 0;
  }
  if ((*wide_length == 1 && (*wide)[0] == L'.') ||
      (*wide_length == 2 && (*wide)[0] == L'.' && (*wide)[1] == L'.')) {
    free(*wide);
    return 0;
  }
  for (index = 0; index < *wide_length; ++index) {
    wchar_t character = (*wide)[index];
    if (character == L'\\' || character == L'/' || character == L':' ||
        character < 0x20) {
      free(*wide);
      return 0;
    }
  }
  return 1;
}

struct dc_windows_private_security {
  SECURITY_DESCRIPTOR descriptor;
  PACL dacl;
  PTOKEN_USER user;
  DWORD ace_flags;
  DWORD access_mask;
};

#define DC_PRIVATE_DIRECTORY_ACE_FLAGS \
  (OBJECT_INHERIT_ACE | CONTAINER_INHERIT_ACE)

static void dc_windows_private_security_release(
    struct dc_windows_private_security *security) {
  free(security->dacl);
  free(security->user);
  security->dacl = NULL;
  security->user = NULL;
}

static int dc_windows_private_security_init(
    struct dc_windows_private_security *security,
    enum dc_windows_private_entry_kind kind, DWORD *error) {
  HANDLE token = GetCurrentThreadEffectiveToken();
  DWORD token_bytes = 0, sid_bytes, dacl_bytes;
  PSID sid;
  memset(security, 0, sizeof(*security));
  security->ace_flags = kind == DC_WINDOWS_PRIVATE_DIRECTORY
                            ? DC_PRIVATE_DIRECTORY_ACE_FLAGS
                            : 0;
  security->access_mask = FILE_ALL_ACCESS;
  if (GetTokenInformation(token, TokenUser, NULL, 0, &token_bytes) ||
      GetLastError() != ERROR_INSUFFICIENT_BUFFER || token_bytes == 0) {
    *error = GetLastError();
    if (*error == ERROR_SUCCESS) *error = ERROR_INVALID_DATA;
    return 0;
  }
  security->user = (PTOKEN_USER)malloc(token_bytes);
  if (security->user == NULL) {
    *error = ERROR_NOT_ENOUGH_MEMORY;
    return 0;
  }
  if (!GetTokenInformation(token, TokenUser, security->user, token_bytes,
                           &token_bytes)) {
    *error = GetLastError();
    dc_windows_private_security_release(security);
    return 0;
  }
  sid = security->user->User.Sid;
  if (!IsValidSid(sid)) {
    *error = ERROR_INVALID_SID;
    dc_windows_private_security_release(security);
    return 0;
  }
  sid_bytes = GetLengthSid(sid);
  if (sid_bytes > MAXDWORD - sizeof(ACL) -
                      (sizeof(ACCESS_ALLOWED_ACE) - sizeof(DWORD))) {
    *error = ERROR_ARITHMETIC_OVERFLOW;
    dc_windows_private_security_release(security);
    return 0;
  }
  dacl_bytes = (DWORD)(sizeof(ACL) +
                       (sizeof(ACCESS_ALLOWED_ACE) - sizeof(DWORD)) +
                       sid_bytes);
  dacl_bytes = (dacl_bytes + sizeof(DWORD) - 1) & ~(sizeof(DWORD) - 1);
  security->dacl = (PACL)malloc(dacl_bytes);
  if (security->dacl == NULL) {
    *error = ERROR_NOT_ENOUGH_MEMORY;
    dc_windows_private_security_release(security);
    return 0;
  }
  if (!InitializeSecurityDescriptor(&security->descriptor,
                                    SECURITY_DESCRIPTOR_REVISION) ||
      !InitializeAcl(security->dacl, dacl_bytes, ACL_REVISION) ||
      !AddAccessAllowedAceEx(security->dacl, ACL_REVISION,
                             security->ace_flags, security->access_mask, sid) ||
      !SetSecurityDescriptorOwner(&security->descriptor, sid, FALSE) ||
      !SetSecurityDescriptorDacl(&security->descriptor, TRUE, security->dacl,
                                 FALSE) ||
      !SetSecurityDescriptorControl(&security->descriptor, SE_DACL_PROTECTED,
                                    SE_DACL_PROTECTED) ||
      !IsValidSecurityDescriptor(&security->descriptor)) {
    *error = GetLastError();
    if (*error == ERROR_SUCCESS) *error = ERROR_INVALID_SECURITY_DESCR;
    dc_windows_private_security_release(security);
    return 0;
  }
  *error = ERROR_SUCCESS;
  return 1;
}

static int dc_windows_verify_private_security(
    HANDLE handle, const struct dc_windows_private_security *expected,
    DWORD *error) {
  DWORD bytes = 0;
  PSECURITY_DESCRIPTOR descriptor = NULL;
  SECURITY_DESCRIPTOR_CONTROL control = 0;
  DWORD revision = 0;
  BOOL dacl_present = FALSE, dacl_defaulted = TRUE, owner_defaulted = TRUE;
  PACL dacl = NULL;
  PSID owner = NULL;
  PVOID raw_ace = NULL;
  ACCESS_ALLOWED_ACE *ace;
  PSID expected_sid = expected->user->User.Sid;
  DWORD requested = OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION;
  if (GetKernelObjectSecurity(handle, requested, NULL, 0, &bytes) ||
      GetLastError() != ERROR_INSUFFICIENT_BUFFER || bytes == 0) {
    *error = GetLastError();
    if (*error == ERROR_SUCCESS) *error = ERROR_INVALID_SECURITY_DESCR;
    return 0;
  }
  descriptor = (PSECURITY_DESCRIPTOR)malloc(bytes);
  if (descriptor == NULL) {
    *error = ERROR_NOT_ENOUGH_MEMORY;
    return 0;
  }
  if (!GetKernelObjectSecurity(handle, requested, descriptor, bytes, &bytes)) {
    *error = GetLastError();
    free(descriptor);
    return 0;
  }
  if (!IsValidSecurityDescriptor(descriptor) ||
      !GetSecurityDescriptorControl(descriptor, &control, &revision) ||
      revision != SECURITY_DESCRIPTOR_REVISION ||
      (control & SE_DACL_PROTECTED) == 0 ||
      !GetSecurityDescriptorOwner(descriptor, &owner, &owner_defaulted) ||
      owner == NULL || owner_defaulted || !EqualSid(owner, expected_sid) ||
      !GetSecurityDescriptorDacl(descriptor, &dacl_present, &dacl,
                                 &dacl_defaulted) ||
      !dacl_present || dacl == NULL || dacl_defaulted || !IsValidAcl(dacl) ||
      dacl->AceCount != 1 || !GetAce(dacl, 0, &raw_ace)) {
    *error = ERROR_INVALID_SECURITY_DESCR;
    free(descriptor);
    return 0;
  }
  ace = (ACCESS_ALLOWED_ACE *)raw_ace;
  if (ace->Header.AceType != ACCESS_ALLOWED_ACE_TYPE ||
      ace->Header.AceFlags != expected->ace_flags ||
      ace->Mask != expected->access_mask ||
      !EqualSid(&ace->SidStart, expected_sid)) {
    *error = ERROR_INVALID_ACL;
    free(descriptor);
    return 0;
  }
  free(descriptor);
  *error = ERROR_SUCCESS;
  return 1;
}

static int dc_reparse(HANDLE handle, DWORD *tag) {
  FILE_ATTRIBUTE_TAG_INFO info;
  if (!GetFileInformationByHandleEx(handle, FileAttributeTagInfo, &info,
                                    sizeof(info)))
    return -1;
  *tag = info.ReparseTag;
  return (info.FileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) != 0;
}

static int dc_windows_identity(HANDLE handle, char *identity,
                               size_t identity_capacity, DWORD *error) {
  FILE_ID_INFO info;
  size_t offset;
  if (!GetFileInformationByHandleEx(handle, FileIdInfo, &info, sizeof(info))) {
    *error = GetLastError();
    return 0;
  }
  if (identity_capacity < 6 + 16 + 1 + 32 + 1) {
    *error = ERROR_INSUFFICIENT_BUFFER;
    return 0;
  }
  offset = (size_t)snprintf(identity, 24, "win32:%016" PRIx64 ":",
                            (uint64_t)info.VolumeSerialNumber);
  for (size_t index = 0; index < sizeof(info.FileId.Identifier); ++index)
    offset += (size_t)snprintf(identity + offset, 3, "%02x",
                              (unsigned int)info.FileId.Identifier[index]);
  identity[offset] = '\0';
  *error = ERROR_SUCCESS;
  return 1;
}

static int dc_windows_mtime(FILETIME time, int64_t *result) {
  ULARGE_INTEGER ticks;
  const uint64_t unix_epoch_ticks = UINT64_C(116444736000000000);
  uint64_t difference;
  ticks.LowPart = time.dwLowDateTime;
  ticks.HighPart = time.dwHighDateTime;
  if (ticks.QuadPart >= unix_epoch_ticks) {
    difference = ticks.QuadPart - unix_epoch_ticks;
    if (difference > (uint64_t)INT64_MAX / 100) return 0;
    *result = (int64_t)(difference * UINT64_C(100));
  } else {
    difference = unix_epoch_ticks - ticks.QuadPart;
    if (difference > (uint64_t)INT64_MAX / 100) return 0;
    *result = -(int64_t)(difference * UINT64_C(100));
  }
  return 1;
}

static int dc_windows_stat(HANDLE handle, int forced_link, char *identity,
                           size_t identity_capacity, int *kind, int64_t *size,
                           int *permissions, int64_t *mtime_ns, DWORD *error) {
  BY_HANDLE_FILE_INFORMATION info;
  DWORD tag = 0;
  int reparse = dc_reparse(handle, &tag);
  (void)tag;
  if (reparse < 0) {
    *error = GetLastError();
    return 0;
  }
  if (!GetFileInformationByHandle(handle, &info)) {
    *error = GetLastError();
    return 0;
  }
  if (!dc_windows_identity(handle, identity, identity_capacity, error)) return 0;
  *kind = forced_link || reparse
              ? 2
              : ((info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) ? 0 : 1);
  {
    uint64_t native_size =
        ((uint64_t)info.nFileSizeHigh << 32) | info.nFileSizeLow;
    if (native_size > (uint64_t)INT64_MAX) {
      *error = ERROR_ARITHMETIC_OVERFLOW;
      return 0;
    }
    *size = (int64_t)native_size;
  }
  *permissions =
      (info.dwFileAttributes & FILE_ATTRIBUTE_READONLY) ? 0444 : 0666;
  if (*kind == 0) *permissions |= 0111;
  if (!dc_windows_mtime(info.ftLastWriteTime, mtime_ns)) {
    *error = ERROR_ARITHMETIC_OVERFLOW;
    return 0;
  }
  return 1;
}

static value dc_alloc_raw_stat(const char *identity, int kind, int64_t size,
                               int permissions, int64_t mtime_ns) {
  CAMLparam0();
  CAMLlocal4(result, identity_value, size_value, mtime_value);
  result = caml_alloc_tuple(5);
  identity_value = caml_copy_string(identity);
  size_value = caml_copy_int64(size);
  mtime_value = caml_copy_int64(mtime_ns);
  Store_field(result, 0, identity_value);
  Store_field(result, 1, Val_int(kind));
  Store_field(result, 2, size_value);
  Store_field(result, 3, Val_int(permissions));
  Store_field(result, 4, mtime_value);
  CAMLreturn(result);
}

static NTSTATUS dc_windows_open_relative(HANDLE parent, const wchar_t *name,
                                         size_t name_length, int mode,
                                         ULONG disposition, HANDLE *result) {
  dc_nt_create_file_fn create_file = dc_nt_create_file();
  UNICODE_STRING native_name;
  OBJECT_ATTRIBUTES attributes;
  IO_STATUS_BLOCK io;
  ACCESS_MASK access = FILE_READ_ATTRIBUTES | SYNCHRONIZE;
  ULONG options = FILE_OPEN_REPARSE_POINT | FILE_SYNCHRONOUS_IO_NONALERT;
  ULONG sharing = FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE;
  if (create_file == NULL) return (NTSTATUS)0xC00000BBU;
  if (mode == 0) {
    access |= FILE_LIST_DIRECTORY | FILE_TRAVERSE | FILE_ADD_SUBDIRECTORY;
    options |= FILE_DIRECTORY_FILE;
  } else if (mode == 1) {
    access |= FILE_READ_DATA;
    options |= FILE_NON_DIRECTORY_FILE;
  } else if (mode == 3 || mode == 4) {
    access |= FILE_READ_DATA | DELETE;
    options |= FILE_NON_DIRECTORY_FILE;
    /* Publication and captured deletion both pin file bytes and the namespace
       entry. Existing writers/deleters make the open fail; new write/delete
       opens are refused until the handle is closed. */
    sharing = FILE_SHARE_READ;
  } else if (mode == 5) {
    access |= FILE_LIST_DIRECTORY | FILE_TRAVERSE | DELETE;
    options |= FILE_DIRECTORY_FILE;
    /* Child creation/removal may race the atomic empty-directory disposition,
       but no handle can rename/delete the captured directory entry. */
    sharing = FILE_SHARE_READ | FILE_SHARE_WRITE;
  }
  native_name.Length = (USHORT)(name_length * sizeof(wchar_t));
  native_name.MaximumLength = native_name.Length;
  native_name.Buffer = (PWSTR)name;
  InitializeObjectAttributes(&attributes, &native_name, 0, parent, NULL);
  return create_file(result, access, &attributes, &io, NULL,
                     (mode == 0 || mode == 5) ? FILE_ATTRIBUTE_DIRECTORY
                                              : FILE_ATTRIBUTE_NORMAL,
                     sharing, disposition, options, NULL, 0);
}

static NTSTATUS dc_windows_open_lock(HANDLE parent, const wchar_t *name,
                                     size_t name_length, HANDLE *result) {
  dc_nt_create_file_fn create_file = dc_nt_create_file();
  UNICODE_STRING native_name;
  OBJECT_ATTRIBUTES attributes;
  IO_STATUS_BLOCK io;
  if (create_file == NULL) return (NTSTATUS)0xC00000BBU;
  native_name.Length = (USHORT)(name_length * sizeof(wchar_t));
  native_name.MaximumLength = native_name.Length;
  native_name.Buffer = (PWSTR)name;
  /* Exact-case root-relative lookup.  Omitting FILE_SHARE_DELETE prevents new
     delete/rename opens for the lifetime of the held lock handle. */
  InitializeObjectAttributes(&attributes, &native_name, 0, parent, NULL);
  return create_file(
      result,
      FILE_READ_DATA | FILE_WRITE_DATA | FILE_READ_ATTRIBUTES | SYNCHRONIZE,
      &attributes, &io, NULL, FILE_ATTRIBUTE_NORMAL,
      FILE_SHARE_READ | FILE_SHARE_WRITE, FILE_OPEN_IF,
      FILE_NON_DIRECTORY_FILE | FILE_OPEN_REPARSE_POINT |
          FILE_SYNCHRONOUS_IO_NONALERT,
      NULL, 0);
}

static NTSTATUS dc_windows_create_private_entry(
    HANDLE parent, const wchar_t *name, size_t name_length,
    PSECURITY_DESCRIPTOR security, enum dc_windows_private_entry_kind kind,
    HANDLE *result) {
  dc_nt_create_file_fn create_file = dc_nt_create_file();
  UNICODE_STRING native_name;
  OBJECT_ATTRIBUTES object_attributes;
  IO_STATUS_BLOCK io;
  ULONG file_attributes;
  ULONG sharing;
  ULONG options = FILE_OPEN_REPARSE_POINT | FILE_SYNCHRONOUS_IO_NONALERT;
  if (create_file == NULL) return (NTSTATUS)0xC00000BBU;
  native_name.Length = (USHORT)(name_length * sizeof(wchar_t));
  native_name.MaximumLength = native_name.Length;
  native_name.Buffer = (PWSTR)name;
  InitializeObjectAttributes(&object_attributes, &native_name, 0, parent,
                             security);
  /* FILE_CREATE is the commit point: an entry that wins the name first is not
     opened, and FILE_OPEN_REPARSE_POINT prevents leaf traversal.  Omitting
     FILE_SHARE_DELETE pins the created child name while the returned handle is
     live. */
  if (kind == DC_WINDOWS_PRIVATE_DIRECTORY) {
    file_attributes = FILE_ATTRIBUTE_DIRECTORY;
    sharing = FILE_SHARE_READ | FILE_SHARE_WRITE;
    options |= FILE_DIRECTORY_FILE;
  } else {
    file_attributes = FILE_ATTRIBUTE_NORMAL;
    sharing = FILE_SHARE_READ;
    options |= FILE_NON_DIRECTORY_FILE;
  }
  return create_file(
      result, FILE_ALL_ACCESS, &object_attributes, &io, NULL, file_attributes,
      sharing, FILE_CREATE, options, NULL, 0);
}

static int dc_windows_write_all(HANDLE handle, const char *contents,
                                size_t length, DWORD *error) {
  size_t offset = 0;
  while (offset < length) {
    size_t remaining = length - offset;
    DWORD amount = remaining > MAXDWORD ? MAXDWORD : (DWORD)remaining;
    DWORD written = 0;
    if (!WriteFile(handle, contents + offset, amount, &written, NULL)) {
      *error = GetLastError();
      return 0;
    }
    if (written == 0 || written > amount) {
      *error = ERROR_WRITE_FAULT;
      return 0;
    }
    offset += written;
  }
  *error = ERROR_SUCCESS;
  return 1;
}

static NTSTATUS dc_windows_open_directory(HANDLE parent, const wchar_t *name,
                                          size_t length, ULONG disposition,
                                          HANDLE *result) {
  NTSTATUS status = dc_windows_open_relative(parent, name, length, 0,
                                             disposition, result);
  if ((uint32_t)status == 0xC0000022U && disposition == FILE_OPEN) {
    dc_nt_create_file_fn create_file = dc_nt_create_file();
    UNICODE_STRING native_name;
    OBJECT_ATTRIBUTES attributes;
    IO_STATUS_BLOCK io;
    if (create_file == NULL) return status;
    native_name.Length = (USHORT)(length * sizeof(wchar_t));
    native_name.MaximumLength = native_name.Length;
    native_name.Buffer = (PWSTR)name;
    InitializeObjectAttributes(&attributes, &native_name, 0, parent, NULL);
    status = create_file(
        result, FILE_READ_ATTRIBUTES | FILE_LIST_DIRECTORY | FILE_TRAVERSE |
                    SYNCHRONIZE,
        &attributes, &io, NULL, FILE_ATTRIBUTE_DIRECTORY,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, FILE_OPEN,
        FILE_DIRECTORY_FILE | FILE_OPEN_REPARSE_POINT |
            FILE_SYNCHRONOUS_IO_NONALERT,
        NULL, 0);
  }
  return status;
}

#else

static int dc_errno_class(int code) {
  switch (code) {
    case ENOENT:
      return DC_MISSING;
    case EEXIST:
      return DC_ALREADY_EXISTS;
    case ENOTDIR:
      return DC_NOT_DIRECTORY;
#ifdef EISDIR
    case EISDIR:
      return DC_NOT_REGULAR;
#endif
    case ELOOP:
      return DC_LINK_LIKE;
#if defined(EFBIG) && EFBIG != EOVERFLOW
    case EFBIG:
#endif
    case EOVERFLOW:
      return DC_TOO_LARGE;
    case EACCES:
    case EPERM:
      return DC_ACCESS_DENIED;
    case EBUSY:
#ifdef ENOTEMPTY
    case ENOTEMPTY:
#endif
      return DC_BUSY;
    case EINVAL:
    case ENAMETOOLONG:
      return DC_INVALID_NAME;
#ifdef ENOTSUP
    case ENOTSUP:
      return DC_UNSUPPORTED;
#endif
#if defined(EOPNOTSUPP) && EOPNOTSUPP != ENOTSUP
    case EOPNOTSUPP:
      return DC_UNSUPPORTED;
#endif
    case ENOSYS:
      return DC_UNSUPPORTED;
    default:
      return DC_OTHER;
  }
}

static value dc_posix_error(int code) {
  return dc_error_number(DC_POSIX, dc_errno_class(code), (uint64_t)code);
}

static int dc_posix_identity(const struct stat *stat, char *identity,
                             size_t identity_capacity) {
  int length = snprintf(identity, identity_capacity,
                        "posix:%" PRIuMAX ":%" PRIuMAX,
                        (uintmax_t)stat->st_dev, (uintmax_t)stat->st_ino);
  return length >= 0 && (size_t)length < identity_capacity;
}

/* The owner-exclusive envelope: only the current effective user can mutate
   entries below a directory it owns with mode exactly 0700.  This is the
   POSIX form of separately proven exclusive namespace-mutation authority; a
   parent that cannot prove it makes the conditional operations fail closed
   with DC_UNSUPPORTED. */
static int dc_posix_owner_exclusive(const struct stat *stat) {
  return S_ISDIR(stat->st_mode) &&
         (stat->st_mode & 07777) ==
             DC_OWNER_PRIVATE_DIRECTORY_PERMISSIONS &&
         stat->st_uid == geteuid();
}

static int dc_posix_mtime(const struct stat *stat, int64_t *result) {
  int64_t seconds;
  long nanoseconds;
#if defined(__APPLE__)
  seconds = (int64_t)stat->st_mtimespec.tv_sec;
  nanoseconds = stat->st_mtimespec.tv_nsec;
#elif defined(__linux__) || defined(__FreeBSD__) || defined(__NetBSD__) || \
    defined(__OpenBSD__)
  seconds = (int64_t)stat->st_mtim.tv_sec;
  nanoseconds = stat->st_mtim.tv_nsec;
#else
  seconds = (int64_t)stat->st_mtime;
  nanoseconds = 0;
#endif
  if (nanoseconds < 0 || nanoseconds >= 1000000000L ||
      seconds > INT64_MAX / INT64_C(1000000000) ||
      seconds < INT64_MIN / INT64_C(1000000000))
    return 0;
  *result = seconds * INT64_C(1000000000);
  if (*result > INT64_MAX - nanoseconds) return 0;
  *result += nanoseconds;
  return 1;
}

static int dc_posix_stat_value(const struct stat *stat, char *identity,
                               size_t identity_capacity, int *kind,
                               int64_t *size, int *permissions,
                               int64_t *mtime_ns) {
  if (!dc_posix_identity(stat, identity, identity_capacity)) {
    errno = EOVERFLOW;
    return 0;
  }
  if (S_ISDIR(stat->st_mode))
    *kind = 0;
  else if (S_ISREG(stat->st_mode))
    *kind = 1;
  else if (S_ISLNK(stat->st_mode))
    *kind = 2;
  else
    *kind = 3;
  *size = (int64_t)stat->st_size;
  *permissions = stat->st_mode & 07777;
  if (!dc_posix_mtime(stat, mtime_ns)) {
    errno = EOVERFLOW;
    return 0;
  }
  return 1;
}

static int dc_posix_write_all(int descriptor, const char *contents,
                              size_t length, int *error) {
  size_t offset = 0;
  while (offset < length) {
    size_t remaining = length - offset;
    size_t amount = remaining > (size_t)SSIZE_MAX ? (size_t)SSIZE_MAX
                                                   : remaining;
    ssize_t written = pwrite(descriptor, contents + offset, amount,
                             (off_t)offset);
    if (written < 0) {
      if (errno == EINTR) continue;
      *error = errno;
      return 0;
    }
    if (written == 0 || (size_t)written > amount) {
      *error = EIO;
      return 0;
    }
    offset += (size_t)written;
  }
  *error = 0;
  return 1;
}

static value dc_alloc_raw_stat(const char *identity, int kind, int64_t size,
                               int permissions, int64_t mtime_ns) {
  CAMLparam0();
  CAMLlocal4(result, identity_value, size_value, mtime_value);
  result = caml_alloc_tuple(5);
  identity_value = caml_copy_string(identity);
  size_value = caml_copy_int64(size);
  mtime_value = caml_copy_int64(mtime_ns);
  Store_field(result, 0, identity_value);
  Store_field(result, 1, Val_int(kind));
  Store_field(result, 2, size_value);
  Store_field(result, 3, Val_int(permissions));
  Store_field(result, 4, mtime_value);
  CAMLreturn(result);
}

#endif

CAMLprim value ocaml_mutants_dircap_duplicate(value handle_value) {
  CAMLparam1(handle_value);
  CAMLlocal1(copy_value);
  struct dc_handle *handle = (struct dc_handle *)Data_custom_val(handle_value);
  int problem;
#ifdef _WIN32
  HANDLE source_os;
#else
  int source_os;
#endif
  if (dc_handle_problem(handle, &problem))
    CAMLreturn(dc_error(
        DC_NATIVE_DOMAIN, problem, problem == DC_WRONG_PROCESS ? "wrong-process"
                                            : "closed-capability"));
  source_os = handle->os;
  copy_value = dc_alloc_empty_handle();
#ifdef _WIN32
  {
    HANDLE copy = INVALID_HANDLE_VALUE;
    if (!DuplicateHandle(GetCurrentProcess(), source_os, GetCurrentProcess(),
                         &copy, 0, FALSE, DUPLICATE_SAME_ACCESS))
      CAMLreturn(dc_win32_error(GetLastError()));
    dc_arm_handle(copy_value, copy);
  }
#else
#ifdef F_DUPFD_CLOEXEC
  {
    int copy = fcntl(source_os, F_DUPFD_CLOEXEC, 0);
    if (copy < 0) CAMLreturn(dc_posix_error(errno));
    dc_arm_handle(copy_value, copy);
  }
#else
  CAMLreturn(dc_error(DC_POSIX, DC_UNSUPPORTED,
                      "atomic-close-on-exec-dup-unavailable"));
#endif
#endif
  CAMLreturn(dc_ok(copy_value));
}

CAMLprim value ocaml_mutants_dircap_probe_entry(value parent_value,
                                                value name_value) {
  CAMLparam2(parent_value, name_value);
  CAMLlocal5(stat_value, option_value, result, handle_value, error_result);
  struct dc_handle *parent = (struct dc_handle *)Data_custom_val(parent_value);
  int problem;
  int inject_action = 0, inject_close = 0;
#ifdef _WIN32
  HANDLE parent_os;
#endif
  if (dc_handle_problem(parent, &problem))
    CAMLreturn(dc_raw_operation_failure(dc_error(
        DC_NATIVE_DOMAIN, problem, problem == DC_WRONG_PROCESS ? "wrong-process"
                                            : "closed-capability")));
#ifdef _WIN32
  parent_os = parent->os;
  {
    wchar_t *name = NULL;
    size_t name_length = 0;
    HANDLE child = INVALID_HANDLE_VALUE;
    NTSTATUS status;
    DWORD error = ERROR_SUCCESS;
    char identity[DC_IDENTITY_CAPACITY];
    int kind, permissions;
    int64_t size, mtime_ns;
    handle_value = dc_alloc_empty_handle();
    if (!dc_windows_component(String_val(name_value),
                              caml_string_length(name_value), &name,
                              &name_length))
      CAMLreturn(dc_raw_operation_failure(dc_error(
          DC_WIN32, DC_INVALID_NAME, "invalid-native-component")));
    status = dc_windows_open_relative(parent_os, name, name_length, 2,
                                      FILE_OPEN, &child);
    if (status < 0) {
      free(name);
      if (dc_nt_class(status) == DC_MISSING) CAMLreturn(dc_ok(Val_none));
      CAMLreturn(dc_raw_operation_failure(dc_nt_error(status)));
    }
    dc_arm_handle(handle_value, child);
    free(name);
    (void)dc_take_test_injection(DC_INJECT_PROBE, &inject_action,
                                 &inject_close);
    if (inject_action) {
      error_result = dc_injected_action_error(DC_INJECT_PROBE);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    if (!dc_windows_stat(child, 0, identity, sizeof(identity), &kind, &size,
                         &permissions, &mtime_ns, &error)) {
      error_result = dc_win32_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    error_result = dc_internal_close_terminal(handle_value, inject_close);
    if (Tag_val(error_result) == 1)
      CAMLreturn(dc_raw_cleanup_failure(error_result));
    stat_value =
        dc_alloc_raw_stat(identity, kind, size, permissions, mtime_ns);
  }
#else
#ifdef AT_SYMLINK_NOFOLLOW
  {
    struct stat stat;
    char identity[DC_IDENTITY_CAPACITY];
    int kind, permissions;
    int64_t size, mtime_ns;
    if (fstatat(parent->os, String_val(name_value), &stat,
                AT_SYMLINK_NOFOLLOW) != 0) {
      if (errno == ENOENT) CAMLreturn(dc_ok(Val_none));
      CAMLreturn(dc_raw_operation_failure(dc_posix_error(errno)));
    }
    if (!dc_posix_stat_value(&stat, identity, sizeof(identity), &kind, &size,
                             &permissions, &mtime_ns))
      CAMLreturn(dc_raw_operation_failure(dc_posix_error(errno)));
    stat_value =
        dc_alloc_raw_stat(identity, kind, size, permissions, mtime_ns);
  }
#else
  CAMLreturn(dc_raw_operation_failure(dc_error(
      DC_POSIX, DC_UNSUPPORTED, "fstatat-no-follow-unavailable")));
#endif
#endif
  option_value = dc_some(stat_value);
  result = dc_ok(option_value);
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_dircap_enumerate(value handle_value,
                                               value max_entries_value,
                                               value max_bytes_value) {
  CAMLparam3(handle_value, max_entries_value, max_bytes_value);
  CAMLlocal5(list_value, payload, result, entries_value, bytes_value);
  CAMLlocal5(names_guard, buffer_guard, failure_result, cleanup_result,
             duplicate_handle);
  CAMLlocal1(stream_guard);
  struct dc_handle *handle = (struct dc_handle *)Data_custom_val(handle_value);
  int inject_action = 0, inject_close = 0;
  int64_t signed_max_entries = Int64_val(max_entries_value);
  int64_t signed_max_bytes = Int64_val(max_bytes_value);
  uint64_t max_entries = signed_max_entries < 0 ? 0 : (uint64_t)signed_max_entries;
  uint64_t max_bytes = signed_max_bytes < 0 ? 0 : (uint64_t)signed_max_bytes;
  uint64_t entries = 0, native_name_bytes = 0;
  int problem;
#ifdef _WIN32
  HANDLE handle_os;
#else
  int handle_os;
#endif
  if (dc_handle_problem(handle, &problem))
    CAMLreturn(dc_enumeration_failure(
        dc_raw_operation_failure(dc_error(
            DC_NATIVE_DOMAIN, problem,
            problem == DC_WRONG_PROCESS ? "wrong-process"
                                        : "closed-capability")),
        entries, native_name_bytes));
  handle_os = handle->os;
  names_guard = dc_alloc_strings_guard();
  buffer_guard = dc_alloc_buffer_guard();
#define DC_ENUMERATION_FAILURE(expression)                                    \
  do {                                                                        \
    dc_buffer_guard_release(buffer_guard);                                    \
    dc_strings_guard_free(names_guard);                                       \
    failure_result = (expression);                                            \
    CAMLreturn(dc_enumeration_failure(failure_result, entries,                \
                                      native_name_bytes));                    \
  } while (0)
  if (signed_max_entries < 0 || signed_max_bytes < 0)
    DC_ENUMERATION_FAILURE(dc_raw_operation_failure(dc_error(
        DC_CONTRACT, DC_INVALID_NAME, "negative-enumeration-budget")));
#ifdef _WIN32
  {
    DWORD buffer_size =
        (DWORD)(sizeof(FILE_ID_BOTH_DIR_INFO) + USHRT_MAX + sizeof(wchar_t));
    unsigned char *buffer = (unsigned char *)malloc(buffer_size);
    FILE_INFO_BY_HANDLE_CLASS info_class = FileIdBothDirectoryRestartInfo;
    if (buffer == NULL)
      DC_ENUMERATION_FAILURE(
          dc_raw_operation_failure(dc_win32_error(ERROR_NOT_ENOUGH_MEMORY)));
    dc_buffer_guard_arm(buffer_guard, buffer);
    for (;;) {
      if (!GetFileInformationByHandleEx(handle_os, info_class, buffer,
                                        buffer_size)) {
        DWORD error = GetLastError();
        if (error == ERROR_NO_MORE_FILES) break;
        DC_ENUMERATION_FAILURE(
            dc_raw_operation_failure(dc_win32_error(error)));
      }
      {
        unsigned char *cursor = buffer;
        for (;;) {
          FILE_ID_BOTH_DIR_INFO *entry;
          size_t header_size = offsetof(FILE_ID_BOTH_DIR_INFO, FileName);
          size_t remaining = (size_t)(buffer + buffer_size - cursor);
          size_t name_end;
          if (remaining < header_size ||
              ((uintptr_t)cursor % _Alignof(FILE_ID_BOTH_DIR_INFO)) != 0) {
            DC_ENUMERATION_FAILURE(dc_raw_operation_failure(dc_error(
                DC_WIN32, DC_OTHER, "truncated-directory-entry-header")));
          }
          entry = (FILE_ID_BOTH_DIR_INFO *)cursor;
          if ((entry->FileNameLength % sizeof(wchar_t)) != 0 ||
              (size_t)entry->FileNameLength > remaining - header_size) {
            DC_ENUMERATION_FAILURE(dc_raw_operation_failure(dc_error(
                DC_WIN32, DC_OTHER, "invalid-directory-entry-name-range")));
          }
          name_end = header_size + (size_t)entry->FileNameLength;
          size_t length = entry->FileNameLength / sizeof(wchar_t);
          int dot =
              (length == 1 && entry->FileName[0] == L'.') ||
              (length == 2 && entry->FileName[0] == L'.' &&
               entry->FileName[1] == L'.');
          if (!dot) {
            char *encoded = dc_wtf8_encode(entry->FileName, length);
            size_t encoded_length;
            if (encoded == NULL)
              DC_ENUMERATION_FAILURE(dc_raw_operation_failure(
                  dc_win32_error(ERROR_NOT_ENOUGH_MEMORY)));
            encoded_length = strlen(encoded);
            if (entries >= max_entries ||
                (uint64_t)encoded_length > max_bytes - native_name_bytes) {
              free(encoded);
              DC_ENUMERATION_FAILURE(dc_raw_operation_failure(dc_error(
                  DC_CONTRACT, DC_TOO_LARGE,
                  "enumeration-budget-exhausted")));
            }
            if (!dc_strings_guard_push(names_guard, encoded)) {
              free(encoded);
              DC_ENUMERATION_FAILURE(dc_raw_operation_failure(
                  dc_win32_error(ERROR_NOT_ENOUGH_MEMORY)));
            }
            ++entries;
            native_name_bytes += encoded_length;
          }
          if (entry->NextEntryOffset == 0) break;
          {
            size_t next = (size_t)entry->NextEntryOffset;
            if (next < name_end ||
                (next % _Alignof(FILE_ID_BOTH_DIR_INFO)) != 0 ||
                next > remaining - header_size) {
              DC_ENUMERATION_FAILURE(dc_raw_operation_failure(dc_error(
                  DC_WIN32, DC_OTHER,
                  "invalid-directory-enumeration-buffer")));
            }
          }
          cursor += entry->NextEntryOffset;
        }
      }
      info_class = FileIdBothDirectoryInfo;
    }
    (void)dc_take_test_injection(DC_INJECT_ENUMERATE, &inject_action,
                                 &inject_close);
    if (inject_action)
      DC_ENUMERATION_FAILURE(dc_raw_operation_failure(
          dc_injected_action_error(DC_INJECT_ENUMERATE)));
    dc_buffer_guard_release(buffer_guard);
  }
#else
#ifdef DC_HAVE_POSIX_ENUMERATION
  {
    int duplicate;
    int action_errno = 0;
    int action_class = DC_OTHER;
    const char *action_code = NULL;
    int action_is_errno = 0;
    DIR *stream;
    struct dirent *entry;
    duplicate_handle = dc_alloc_empty_handle();
    stream_guard = dc_alloc_directory_stream_guard();
    duplicate = openat(handle_os, ".",
                       O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (duplicate < 0)
      DC_ENUMERATION_FAILURE(
          dc_raw_operation_failure(dc_posix_error(errno)));
    dc_arm_handle(duplicate_handle, duplicate);
    (void)dc_take_test_injection(DC_INJECT_ENUMERATE, &inject_action,
                                 &inject_close);
    if (inject_action) {
      cleanup_result =
          dc_internal_close_terminal(duplicate_handle, inject_close);
      if (Tag_val(cleanup_result) == 1)
        DC_ENUMERATION_FAILURE(dc_raw_operation_cleanup_failure(
            dc_injected_action_error(DC_INJECT_ENUMERATE), cleanup_result));
      DC_ENUMERATION_FAILURE(dc_raw_operation_failure(
          dc_injected_action_error(DC_INJECT_ENUMERATE)));
    }
    stream = fdopendir(duplicate);
    if (stream == NULL) {
      action_errno = errno;
      cleanup_result =
          dc_internal_close_terminal(duplicate_handle, inject_close);
      if (Tag_val(cleanup_result) == 1)
        DC_ENUMERATION_FAILURE(dc_raw_operation_cleanup_failure(
            dc_posix_error(action_errno), cleanup_result));
      DC_ENUMERATION_FAILURE(
          dc_raw_operation_failure(dc_posix_error(action_errno)));
    }
    dc_directory_stream_guard_arm(stream_guard, stream);
    dc_handle_transfer_to_stream(duplicate_handle);
    for (;;) {
      size_t length;
      char *owned;
      errno = 0;
      entry = readdir(stream);
      if (entry == NULL) {
        if (errno != 0) {
          action_errno = errno;
          action_is_errno = 1;
        }
        break;
      }
      if ((entry->d_name[0] == '.' && entry->d_name[1] == '\0') ||
          (entry->d_name[0] == '.' && entry->d_name[1] == '.' &&
           entry->d_name[2] == '\0'))
        continue;
      length = strlen(entry->d_name);
      if (entries >= max_entries ||
          (uint64_t)length > max_bytes - native_name_bytes) {
        action_class = DC_TOO_LARGE;
        action_code = "enumeration-budget-exhausted";
        break;
      }
      if (length == SIZE_MAX) {
        action_class = DC_TOO_LARGE;
        action_code = "native-name-too-large";
        break;
      }
      owned = (char *)malloc(length + 1);
      if (owned == NULL) {
        action_errno = ENOMEM;
        action_is_errno = 1;
        break;
      }
      memcpy(owned, entry->d_name, length + 1);
      if (!dc_strings_guard_push(names_guard, owned)) {
        free(owned);
        action_errno = ENOMEM;
        action_is_errno = 1;
        break;
      }
      ++entries;
      native_name_bytes += length;
    }
    cleanup_result =
        dc_directory_stream_close_terminal(stream_guard, inject_close);
    if (action_is_errno || action_code != NULL) {
      if (Tag_val(cleanup_result) == 1) {
        if (action_is_errno)
          DC_ENUMERATION_FAILURE(dc_raw_operation_cleanup_failure(
              dc_posix_error(action_errno), cleanup_result));
        DC_ENUMERATION_FAILURE(dc_raw_operation_cleanup_failure(
            dc_error(DC_CONTRACT, action_class, action_code), cleanup_result));
      }
      if (action_is_errno)
        DC_ENUMERATION_FAILURE(
            dc_raw_operation_failure(dc_posix_error(action_errno)));
      DC_ENUMERATION_FAILURE(dc_raw_operation_failure(
          dc_error(DC_CONTRACT, action_class, action_code)));
    }
    if (Tag_val(cleanup_result) == 1)
      DC_ENUMERATION_FAILURE(dc_raw_cleanup_failure(cleanup_result));
  }
#else
  (void)max_entries;
  (void)max_bytes;
  (void)inject_action;
  (void)inject_close;
  DC_ENUMERATION_FAILURE(dc_raw_operation_failure(dc_error(
      DC_POSIX, DC_UNSUPPORTED, "openat-fdopendir-primitives-unavailable")));
#endif
#endif
  list_value = dc_strings_guard_to_list(names_guard);
  dc_strings_guard_free(names_guard);
  entries_value = caml_copy_int64((int64_t)entries);
  bytes_value = caml_copy_int64((int64_t)native_name_bytes);
  payload = caml_alloc_tuple(3);
  Store_field(payload, 0, list_value);
  Store_field(payload, 1, entries_value);
  Store_field(payload, 2, bytes_value);
  result = dc_ok(payload);
#undef DC_ENUMERATION_FAILURE
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_dircap_create_directory(
    value parent_value, value name_value, value permissions_value,
    value parent_identity_value) {
  CAMLparam4(parent_value, name_value, permissions_value,
             parent_identity_value);
  CAMLlocal5(handle_value, stat_value, payload, error_result, failure_result);
  CAMLlocal1(result);
  struct dc_handle *parent = (struct dc_handle *)Data_custom_val(parent_value);
  int problem;
  int permissions = Int_val(permissions_value);
  int inject_action = 0, inject_close = 0;
  if (dc_handle_problem(parent, &problem)) {
    error_result =
        dc_error(DC_NATIVE_DOMAIN, problem,
                 problem == DC_WRONG_PROCESS ? "wrong-process"
                                             : "closed-capability");
    CAMLreturn(dc_raw_create_not_committed(error_result));
  }
  if (permissions != DC_OWNER_PRIVATE_DIRECTORY_PERMISSIONS) {
    error_result = dc_error(DC_CONTRACT, DC_UNSUPPORTED,
                            "owner-private-directory-policy-required");
    CAMLreturn(dc_raw_create_not_committed(error_result));
  }
  (void)dc_take_test_injection(DC_INJECT_CREATE_BEFORE_COMMIT,
                               &inject_action, &inject_close);
  if (inject_action || inject_close) {
    error_result =
        dc_injected_action_error(DC_INJECT_CREATE_BEFORE_COMMIT);
    CAMLreturn(dc_raw_create_not_committed(error_result));
  }
#ifdef _WIN32
  {
    wchar_t *name = NULL;
    size_t name_length = 0;
    HANDLE parent_handle = parent->os;
    HANDLE child = INVALID_HANDLE_VALUE;
    NTSTATUS status;
    DWORD tag = 0, error = ERROR_SUCCESS;
    char parent_identity[DC_IDENTITY_CAPACITY];
    char child_identity[DC_IDENTITY_CAPACITY];
    int kind, native_permissions;
    int64_t size, mtime_ns;
    struct dc_windows_private_security security;
    if (!dc_windows_component(String_val(name_value),
                              caml_string_length(name_value), &name,
                              &name_length)) {
      error_result =
          dc_error(DC_WIN32, DC_INVALID_NAME, "invalid-native-component");
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (!dc_windows_identity(parent_handle, parent_identity,
                             sizeof(parent_identity), &error)) {
      free(name);
      error_result = dc_win32_error(error);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (strlen(parent_identity) != caml_string_length(parent_identity_value) ||
        memcmp(parent_identity, String_val(parent_identity_value),
               caml_string_length(parent_identity_value)) != 0) {
      free(name);
      error_result = dc_error(DC_CONTRACT, DC_OTHER,
                              "create-directory-parent-identity-changed");
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    problem = dc_reparse(parent_handle, &tag);
    if (problem < 0) {
      error = GetLastError();
      free(name);
      error_result = dc_win32_error(error);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (problem > 0) {
      free(name);
      error_result = dc_error(DC_WIN32, DC_LINK_LIKE,
                              "create-directory-parent-is-reparse-point");
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (!dc_windows_private_security_init(
            &security, DC_WINDOWS_PRIVATE_DIRECTORY, &error)) {
      free(name);
      error_result = dc_win32_error(error);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    handle_value = dc_alloc_empty_handle();
    status = dc_windows_create_private_entry(
        parent_handle, name, name_length, &security.descriptor,
        DC_WINDOWS_PRIVATE_DIRECTORY, &child);
    free(name);
    if (status < 0) {
      dc_windows_private_security_release(&security);
      error_result = dc_nt_error(status);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    dc_arm_handle(handle_value, child);
#define DC_CREATE_POST_COMMIT_FAILURE(expression, close_fault)               \
  do {                                                                        \
    error_result = (expression);                                              \
    failure_result = dc_internal_action_failure(handle_value, error_result,  \
                                                (close_fault));               \
    dc_windows_private_security_release(&security);                           \
    CAMLreturn(dc_raw_create_may_have_committed(failure_result));             \
  } while (0)
    (void)dc_take_test_injection(DC_INJECT_CREATE_AFTER_COMMIT,
                                 &inject_action, &inject_close);
    if (inject_action || inject_close)
      DC_CREATE_POST_COMMIT_FAILURE(
          dc_injected_action_error(DC_INJECT_CREATE_AFTER_COMMIT),
          inject_close);
    problem = dc_reparse(child, &tag);
    if (problem < 0)
      DC_CREATE_POST_COMMIT_FAILURE(dc_win32_error(GetLastError()), 0);
    if (problem > 0)
      DC_CREATE_POST_COMMIT_FAILURE(
          dc_error(DC_WIN32, DC_LINK_LIKE,
                   "created-directory-is-reparse-point"),
          0);
    if (!dc_windows_stat(child, 0, child_identity, sizeof(child_identity),
                         &kind, &size, &native_permissions, &mtime_ns,
                         &error))
      DC_CREATE_POST_COMMIT_FAILURE(dc_win32_error(error), 0);
    if (kind != 0)
      DC_CREATE_POST_COMMIT_FAILURE(
          dc_error(DC_WIN32, DC_NOT_DIRECTORY,
                   "created-entry-is-not-directory"),
          0);
    if (!dc_windows_verify_private_security(child, &security, &error))
      DC_CREATE_POST_COMMIT_FAILURE(dc_win32_error(error), 0);
    dc_windows_private_security_release(&security);
#undef DC_CREATE_POST_COMMIT_FAILURE
    stat_value = dc_alloc_raw_stat(child_identity, kind, size,
                                   native_permissions, mtime_ns);
  }
#else
#ifdef DC_HAVE_POSIX_DIR_CREATE
  {
    struct stat parent_stat, child_stat, binding_stat;
    char parent_identity[DC_IDENTITY_CAPACITY];
    char child_identity[DC_IDENTITY_CAPACITY];
    char binding_identity[DC_IDENTITY_CAPACITY];
    int child;
    int kind, native_permissions;
    int64_t size, mtime_ns;
    if (fstat(parent->os, &parent_stat) != 0) {
      error_result = dc_posix_error(errno);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (!S_ISDIR(parent_stat.st_mode)) {
      error_result = dc_error(DC_POSIX, DC_NOT_DIRECTORY,
                              "create-directory-parent-is-not-directory");
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (!dc_posix_identity(&parent_stat, parent_identity,
                           sizeof(parent_identity))) {
      error_result = dc_posix_error(EOVERFLOW);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (strlen(parent_identity) != caml_string_length(parent_identity_value) ||
        memcmp(parent_identity, String_val(parent_identity_value),
               caml_string_length(parent_identity_value)) != 0) {
      error_result = dc_error(DC_CONTRACT, DC_OTHER,
                              "create-directory-parent-identity-changed");
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (mkdirat(parent->os, String_val(name_value),
                DC_OWNER_PRIVATE_DIRECTORY_PERMISSIONS) != 0) {
      error_result = dc_posix_error(errno);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    child = openat(parent->os, String_val(name_value),
                   O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (child < 0) {
      error_result = dc_posix_error(errno);
      failure_result = dc_raw_operation_failure(error_result);
      CAMLreturn(dc_raw_create_may_have_committed(failure_result));
    }
    handle_value = dc_alloc_empty_handle();
    dc_arm_handle(handle_value, child);
#define DC_CREATE_POST_COMMIT_FAILURE(expression, close_fault)                \
  do {                                                                        \
    error_result = (expression);                                              \
    failure_result = dc_internal_action_failure(handle_value, error_result,  \
                                                (close_fault));               \
    CAMLreturn(dc_raw_create_may_have_committed(failure_result));             \
  } while (0)
    (void)dc_take_test_injection(DC_INJECT_CREATE_AFTER_COMMIT,
                                 &inject_action, &inject_close);
    if (inject_action || inject_close)
      DC_CREATE_POST_COMMIT_FAILURE(
          dc_injected_action_error(DC_INJECT_CREATE_AFTER_COMMIT),
          inject_close);
    if (fchmod(child, DC_OWNER_PRIVATE_DIRECTORY_PERMISSIONS) != 0)
      DC_CREATE_POST_COMMIT_FAILURE(dc_posix_error(errno), 0);
    if (fstat(child, &child_stat) != 0)
      DC_CREATE_POST_COMMIT_FAILURE(dc_posix_error(errno), 0);
    if (!S_ISDIR(child_stat.st_mode))
      DC_CREATE_POST_COMMIT_FAILURE(
          dc_error(DC_POSIX, DC_NOT_DIRECTORY,
                   "created-entry-is-not-directory"),
          0);
    if ((child_stat.st_mode & 07777) !=
        DC_OWNER_PRIVATE_DIRECTORY_PERMISSIONS)
      DC_CREATE_POST_COMMIT_FAILURE(
          dc_error(DC_CONTRACT, DC_ACCESS_DENIED,
                   "created-directory-permissions-not-owner-private"),
          0);
    if (child_stat.st_uid != geteuid())
      DC_CREATE_POST_COMMIT_FAILURE(
          dc_error(DC_CONTRACT, DC_ACCESS_DENIED,
                   "created-directory-owner-mismatch"),
          0);
    if (child_stat.st_dev != parent_stat.st_dev)
      DC_CREATE_POST_COMMIT_FAILURE(
          dc_error(DC_CONTRACT, DC_OTHER,
                   "created-directory-device-mismatch"),
          0);
    if (fstatat(parent->os, String_val(name_value), &binding_stat,
                AT_SYMLINK_NOFOLLOW) != 0)
      DC_CREATE_POST_COMMIT_FAILURE(dc_posix_error(errno), 0);
    if (!dc_posix_identity(&binding_stat, binding_identity,
                           sizeof(binding_identity)))
      DC_CREATE_POST_COMMIT_FAILURE(dc_posix_error(EOVERFLOW), 0);
    if (!dc_posix_stat_value(&child_stat, child_identity,
                             sizeof(child_identity), &kind, &size,
                             &native_permissions, &mtime_ns))
      DC_CREATE_POST_COMMIT_FAILURE(dc_posix_error(errno), 0);
    if (strcmp(binding_identity, child_identity) != 0)
      DC_CREATE_POST_COMMIT_FAILURE(
          dc_error(DC_CONTRACT, DC_OTHER,
                   "created-directory-name-binding-mismatch"),
          0);
#undef DC_CREATE_POST_COMMIT_FAILURE
    stat_value = dc_alloc_raw_stat(child_identity, kind, size,
                                   native_permissions, mtime_ns);
  }
#else
  (void)name_value;
  (void)parent_identity_value;
  error_result = dc_error(DC_POSIX, DC_UNSUPPORTED,
                          "owner-private-directory-create-unavailable");
  CAMLreturn(dc_raw_create_not_committed(error_result));
#endif
#endif
  payload = caml_alloc_tuple(2);
  Store_field(payload, 0, handle_value);
  Store_field(payload, 1, stat_value);
  result = dc_ok(payload);
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_dircap_create_file(
    value parent_value, value name_value, value permissions_value,
    value contents_value, value parent_identity_value) {
  CAMLparam5(parent_value, name_value, permissions_value, contents_value,
             parent_identity_value);
  CAMLlocal5(handle_value, stat_value, payload, error_result, failure_result);
  CAMLlocal1(result);
  struct dc_handle *parent = (struct dc_handle *)Data_custom_val(parent_value);
  size_t contents_length = caml_string_length(contents_value);
  int problem;
  int permissions = Int_val(permissions_value);
  int inject_action = 0, inject_close = 0;
  if (dc_handle_problem(parent, &problem)) {
    error_result =
        dc_error(DC_NATIVE_DOMAIN, problem,
                 problem == DC_WRONG_PROCESS ? "wrong-process"
                                             : "closed-capability");
    CAMLreturn(dc_raw_create_not_committed(error_result));
  }
  if (permissions != DC_OWNER_PRIVATE_FILE_PERMISSIONS) {
    error_result = dc_error(DC_CONTRACT, DC_UNSUPPORTED,
                            "owner-private-file-policy-required");
    CAMLreturn(dc_raw_create_not_committed(error_result));
  }
  if ((uintmax_t)contents_length > (uintmax_t)INT64_MAX) {
    error_result = dc_error(DC_CONTRACT, DC_TOO_LARGE,
                            "create-file-contents-too-large");
    CAMLreturn(dc_raw_create_not_committed(error_result));
  }
  (void)dc_take_test_injection(DC_INJECT_CREATE_FILE_BEFORE_COMMIT,
                               &inject_action, &inject_close);
  if (inject_action || inject_close) {
    error_result =
        dc_injected_action_error(DC_INJECT_CREATE_FILE_BEFORE_COMMIT);
    CAMLreturn(dc_raw_create_not_committed(error_result));
  }
#ifdef _WIN32
  {
    wchar_t *name = NULL;
    size_t name_length = 0;
    HANDLE parent_handle = parent->os;
    HANDLE child = INVALID_HANDLE_VALUE;
    NTSTATUS status;
    DWORD tag = 0, error = ERROR_SUCCESS;
    char parent_identity[DC_IDENTITY_CAPACITY];
    char initial_identity[DC_IDENTITY_CAPACITY];
    char child_identity[DC_IDENTITY_CAPACITY];
    int kind, native_permissions;
    int64_t size, mtime_ns;
    struct dc_windows_private_security security;
    if (!dc_windows_component(String_val(name_value),
                              caml_string_length(name_value), &name,
                              &name_length)) {
      error_result =
          dc_error(DC_WIN32, DC_INVALID_NAME, "invalid-native-component");
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (!dc_windows_identity(parent_handle, parent_identity,
                             sizeof(parent_identity), &error)) {
      free(name);
      error_result = dc_win32_error(error);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (strlen(parent_identity) != caml_string_length(parent_identity_value) ||
        memcmp(parent_identity, String_val(parent_identity_value),
               caml_string_length(parent_identity_value)) != 0) {
      free(name);
      error_result = dc_error(DC_CONTRACT, DC_OTHER,
                              "create-file-parent-identity-changed");
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    problem = dc_reparse(parent_handle, &tag);
    if (problem < 0) {
      error = GetLastError();
      free(name);
      error_result = dc_win32_error(error);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (problem > 0) {
      free(name);
      error_result = dc_error(DC_WIN32, DC_LINK_LIKE,
                              "create-file-parent-is-reparse-point");
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (!dc_windows_private_security_init(
            &security, DC_WINDOWS_PRIVATE_FILE, &error)) {
      free(name);
      error_result = dc_win32_error(error);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    handle_value = dc_alloc_empty_handle();
    status = dc_windows_create_private_entry(
        parent_handle, name, name_length, &security.descriptor,
        DC_WINDOWS_PRIVATE_FILE, &child);
    free(name);
    if (status < 0) {
      dc_windows_private_security_release(&security);
      error_result = dc_nt_error(status);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    dc_arm_handle(handle_value, child);
#define DC_CREATE_FILE_POST_COMMIT_FAILURE(expression, close_fault)          \
  do {                                                                        \
    error_result = (expression);                                              \
    failure_result = dc_internal_action_failure(handle_value, error_result,  \
                                                (close_fault));               \
    dc_windows_private_security_release(&security);                           \
    CAMLreturn(dc_raw_create_may_have_committed(failure_result));             \
  } while (0)
    (void)dc_take_test_injection(DC_INJECT_CREATE_FILE_AFTER_COMMIT,
                                 &inject_action, &inject_close);
    if (inject_action || inject_close)
      DC_CREATE_FILE_POST_COMMIT_FAILURE(
          dc_injected_action_error(DC_INJECT_CREATE_FILE_AFTER_COMMIT),
          inject_close);
    problem = dc_reparse(child, &tag);
    if (problem < 0)
      DC_CREATE_FILE_POST_COMMIT_FAILURE(dc_win32_error(GetLastError()), 0);
    if (problem > 0)
      DC_CREATE_FILE_POST_COMMIT_FAILURE(
          dc_error(DC_WIN32, DC_LINK_LIKE, "created-file-is-reparse-point"),
          0);
    if (!dc_windows_stat(child, 0, initial_identity,
                         sizeof(initial_identity), &kind, &size,
                         &native_permissions, &mtime_ns, &error))
      DC_CREATE_FILE_POST_COMMIT_FAILURE(dc_win32_error(error), 0);
    if (kind != 1)
      DC_CREATE_FILE_POST_COMMIT_FAILURE(
          dc_error(DC_WIN32, DC_NOT_REGULAR,
                   "created-entry-is-not-regular-file"),
          0);
    if (!dc_windows_verify_private_security(child, &security, &error))
      DC_CREATE_FILE_POST_COMMIT_FAILURE(dc_win32_error(error), 0);
    if (!dc_windows_write_all(child, String_val(contents_value),
                              contents_length, &error))
      DC_CREATE_FILE_POST_COMMIT_FAILURE(dc_win32_error(error), 0);
    if (!dc_windows_stat(child, 0, child_identity, sizeof(child_identity),
                         &kind, &size, &native_permissions, &mtime_ns,
                         &error))
      DC_CREATE_FILE_POST_COMMIT_FAILURE(dc_win32_error(error), 0);
    if (kind != 1 || strcmp(initial_identity, child_identity) != 0 ||
        size != (int64_t)contents_length)
      DC_CREATE_FILE_POST_COMMIT_FAILURE(
          dc_error(DC_CONTRACT, DC_OTHER,
                   "created-file-post-write-stat-mismatch"),
          0);
    {
      unsigned char buffer[8192];
      size_t offset = 0;
      while (offset < contents_length) {
        size_t remaining = contents_length - offset;
        ULONG amount = remaining > sizeof(buffer) ? (ULONG)sizeof(buffer)
                                                   : (ULONG)remaining;
        ULONG read_count = 0;
        status = dc_windows_read_at(child, buffer, amount, (uint64_t)offset,
                                    &read_count);
        if (status != 0)
          DC_CREATE_FILE_POST_COMMIT_FAILURE(dc_nt_error(status), 0);
        if (read_count != amount ||
            memcmp(buffer, String_val(contents_value) + offset, amount) != 0)
          DC_CREATE_FILE_POST_COMMIT_FAILURE(
              dc_error(DC_CONTRACT, DC_OTHER,
                       "created-file-contents-mismatch"),
              0);
        offset += read_count;
      }
    }
    dc_windows_private_security_release(&security);
#undef DC_CREATE_FILE_POST_COMMIT_FAILURE
    stat_value = dc_alloc_raw_stat(child_identity, kind, size,
                                   native_permissions, mtime_ns);
  }
#else
#if defined(O_NOFOLLOW) && defined(O_CLOEXEC)
  {
    struct stat parent_stat, initial_stat, child_stat;
    char parent_identity[DC_IDENTITY_CAPACITY];
    char initial_identity[DC_IDENTITY_CAPACITY];
    char child_identity[DC_IDENTITY_CAPACITY];
    int child, error = 0;
    int kind, native_permissions;
    int64_t size, mtime_ns;
    off_t final_offset = (off_t)contents_length;
    if (final_offset < 0 ||
        (uintmax_t)final_offset != (uintmax_t)contents_length) {
      error_result = dc_error(DC_POSIX, DC_TOO_LARGE,
                              "create-file-contents-too-large-for-off-t");
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (fstat(parent->os, &parent_stat) != 0) {
      error_result = dc_posix_error(errno);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (!S_ISDIR(parent_stat.st_mode)) {
      error_result = dc_error(DC_POSIX, DC_NOT_DIRECTORY,
                              "create-file-parent-is-not-directory");
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (!dc_posix_identity(&parent_stat, parent_identity,
                           sizeof(parent_identity))) {
      error_result = dc_posix_error(EOVERFLOW);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    if (strlen(parent_identity) != caml_string_length(parent_identity_value) ||
        memcmp(parent_identity, String_val(parent_identity_value),
               caml_string_length(parent_identity_value)) != 0) {
      error_result = dc_error(DC_CONTRACT, DC_OTHER,
                              "create-file-parent-identity-changed");
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    child = openat(parent->os, String_val(name_value),
                   O_RDWR | O_CREAT | O_EXCL | O_NOFOLLOW | O_CLOEXEC,
                   DC_OWNER_PRIVATE_FILE_PERMISSIONS);
    if (child < 0) {
      error_result = dc_posix_error(errno);
      CAMLreturn(dc_raw_create_not_committed(error_result));
    }
    handle_value = dc_alloc_empty_handle();
    dc_arm_handle(handle_value, child);
#define DC_CREATE_FILE_POST_COMMIT_FAILURE(expression, close_fault)          \
  do {                                                                        \
    error_result = (expression);                                              \
    failure_result = dc_internal_action_failure(handle_value, error_result,  \
                                                (close_fault));               \
    CAMLreturn(dc_raw_create_may_have_committed(failure_result));             \
  } while (0)
    (void)dc_take_test_injection(DC_INJECT_CREATE_FILE_AFTER_COMMIT,
                                 &inject_action, &inject_close);
    if (inject_action || inject_close)
      DC_CREATE_FILE_POST_COMMIT_FAILURE(
          dc_injected_action_error(DC_INJECT_CREATE_FILE_AFTER_COMMIT),
          inject_close);
    if (fchmod(child, DC_OWNER_PRIVATE_FILE_PERMISSIONS) != 0)
      DC_CREATE_FILE_POST_COMMIT_FAILURE(dc_posix_error(errno), 0);
    if (fstat(child, &initial_stat) != 0)
      DC_CREATE_FILE_POST_COMMIT_FAILURE(dc_posix_error(errno), 0);
    if (!S_ISREG(initial_stat.st_mode))
      DC_CREATE_FILE_POST_COMMIT_FAILURE(
          dc_error(DC_POSIX, DC_NOT_REGULAR,
                   "created-entry-is-not-regular-file"),
          0);
    if ((initial_stat.st_mode & 07777) !=
        DC_OWNER_PRIVATE_FILE_PERMISSIONS)
      DC_CREATE_FILE_POST_COMMIT_FAILURE(
          dc_error(DC_CONTRACT, DC_ACCESS_DENIED,
                   "created-file-permissions-not-owner-private"),
          0);
    if (!dc_posix_identity(&initial_stat, initial_identity,
                           sizeof(initial_identity)))
      DC_CREATE_FILE_POST_COMMIT_FAILURE(dc_posix_error(EOVERFLOW), 0);
    if (!dc_posix_write_all(child, String_val(contents_value), contents_length,
                            &error))
      DC_CREATE_FILE_POST_COMMIT_FAILURE(dc_posix_error(error), 0);
    if (fstat(child, &child_stat) != 0)
      DC_CREATE_FILE_POST_COMMIT_FAILURE(dc_posix_error(errno), 0);
    if (!dc_posix_stat_value(&child_stat, child_identity,
                             sizeof(child_identity), &kind, &size,
                             &native_permissions, &mtime_ns))
      DC_CREATE_FILE_POST_COMMIT_FAILURE(dc_posix_error(errno), 0);
    if (kind != 1 || strcmp(initial_identity, child_identity) != 0 ||
        size != (int64_t)contents_length ||
        native_permissions != DC_OWNER_PRIVATE_FILE_PERMISSIONS)
      DC_CREATE_FILE_POST_COMMIT_FAILURE(
          dc_error(DC_CONTRACT, DC_OTHER,
                   "created-file-post-write-stat-mismatch"),
          0);
    {
      unsigned char buffer[8192];
      size_t offset = 0;
      while (offset < contents_length) {
        size_t remaining = contents_length - offset;
        size_t amount = remaining > sizeof(buffer) ? sizeof(buffer) : remaining;
        off_t native_offset = (off_t)offset;
        ssize_t read_count;
        do {
          read_count = pread(child, buffer, amount, native_offset);
        } while (read_count < 0 && errno == EINTR);
        if (read_count < 0)
          DC_CREATE_FILE_POST_COMMIT_FAILURE(dc_posix_error(errno), 0);
        if ((size_t)read_count != amount ||
            memcmp(buffer, String_val(contents_value) + offset, amount) != 0)
          DC_CREATE_FILE_POST_COMMIT_FAILURE(
              dc_error(DC_CONTRACT, DC_OTHER,
                       "created-file-contents-mismatch"),
              0);
        offset += (size_t)read_count;
      }
    }
#undef DC_CREATE_FILE_POST_COMMIT_FAILURE
    stat_value = dc_alloc_raw_stat(child_identity, kind, size,
                                   native_permissions, mtime_ns);
  }
#else
  (void)name_value;
  (void)contents_value;
  (void)parent_identity_value;
  error_result = dc_error(DC_POSIX, DC_UNSUPPORTED,
                          "openat-exclusive-private-file-unavailable");
  CAMLreturn(dc_raw_create_not_committed(error_result));
#endif
#endif
  payload = caml_alloc_tuple(2);
  Store_field(payload, 0, handle_value);
  Store_field(payload, 1, stat_value);
  result = dc_ok(payload);
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_dircap_delete_captured(
    value parent_value, value target_value, value parent_identity_value,
    value target_identity_value, value target_leaf_value,
    value target_kind_value) {
  CAMLparam5(parent_value, target_value, parent_identity_value,
             target_identity_value, target_leaf_value);
  CAMLxparam1(target_kind_value);
  CAMLlocal4(error_result, failure_result, cleanup_result, result);
  struct dc_handle *parent = (struct dc_handle *)Data_custom_val(parent_value);
  struct dc_handle *target = (struct dc_handle *)Data_custom_val(target_value);
  int problem;
  int target_kind = Int_val(target_kind_value);
  int inject_action = 0, inject_close = 0;
  if (dc_handle_problem(parent, &problem)) {
    error_result =
        dc_error(DC_NATIVE_DOMAIN, problem,
                 problem == DC_WRONG_PROCESS ? "wrong-process"
                                             : "closed-parent-capability");
    CAMLreturn(dc_raw_delete_not_committed(error_result));
  }
  if (dc_handle_problem(target, &problem)) {
    error_result =
        dc_error(DC_NATIVE_DOMAIN, problem,
                 problem == DC_WRONG_PROCESS ? "wrong-process"
                                             : "closed-target-capability");
    CAMLreturn(dc_raw_delete_not_committed(error_result));
  }
  if (target_kind != 0 && target_kind != 1) {
    error_result =
        dc_error(DC_CONTRACT, DC_INVALID_NAME, "invalid-delete-target-kind");
    CAMLreturn(dc_raw_delete_not_committed(error_result));
  }
#ifdef _WIN32
#if defined(FILE_DISPOSITION_FLAG_DELETE) && \
    defined(FILE_DISPOSITION_FLAG_POSIX_SEMANTICS)
  {
    HANDLE parent_handle = parent->os;
    HANDLE target_handle = target->os;
    FILE_DISPOSITION_INFO_EX disposition;
    DWORD error = ERROR_SUCCESS;
    char parent_identity[DC_IDENTITY_CAPACITY];
    char target_identity[DC_IDENTITY_CAPACITY];
    int parent_kind, observed_kind, native_permissions;
    int64_t size, mtime_ns;
    if (!dc_windows_stat(parent_handle, 0, parent_identity,
                         sizeof(parent_identity), &parent_kind, &size,
                         &native_permissions, &mtime_ns, &error)) {
      error_result = dc_win32_error(error);
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (parent_kind != 0) {
      error_result = dc_error(DC_WIN32, DC_NOT_DIRECTORY,
                              "captured-delete-parent-is-not-directory");
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (strlen(parent_identity) != caml_string_length(parent_identity_value) ||
        memcmp(parent_identity, String_val(parent_identity_value),
               caml_string_length(parent_identity_value)) != 0) {
      error_result = dc_error(DC_CONTRACT, DC_OTHER,
                              "captured-delete-parent-identity-changed");
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (!dc_windows_stat(target_handle, 0, target_identity,
                         sizeof(target_identity), &observed_kind, &size,
                         &native_permissions, &mtime_ns, &error)) {
      error_result = dc_win32_error(error);
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if ((target_kind == 0 && observed_kind != 1) ||
        (target_kind == 1 && observed_kind != 0)) {
      error_result = dc_error(
          DC_WIN32, target_kind == 0 ? DC_NOT_REGULAR : DC_NOT_DIRECTORY,
          "captured-delete-target-kind-mismatch");
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (strlen(target_identity) != caml_string_length(target_identity_value) ||
        memcmp(target_identity, String_val(target_identity_value),
               caml_string_length(target_identity_value)) != 0)
      CAMLreturn(dc_ok(Val_false));
    if (strcmp(parent_identity, target_identity) == 0) {
      error_result = dc_error(DC_CONTRACT, DC_OTHER,
                              "captured-delete-target-is-parent");
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    (void)dc_take_test_injection(DC_INJECT_DELETE_BEFORE_COMMIT,
                                 &inject_action, &inject_close);
    if (inject_action || inject_close) {
      error_result =
          dc_injected_action_error(DC_INJECT_DELETE_BEFORE_COMMIT);
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    disposition.Flags = FILE_DISPOSITION_FLAG_DELETE |
                        FILE_DISPOSITION_FLAG_POSIX_SEMANTICS;
    if (!SetFileInformationByHandle(target_handle, FileDispositionInfoEx,
                                    &disposition, sizeof(disposition))) {
      error = GetLastError();
      if (error == ERROR_INVALID_PARAMETER || error == ERROR_INVALID_FUNCTION ||
          error == ERROR_NOT_SUPPORTED)
        error_result =
            dc_error(DC_WIN32, DC_UNSUPPORTED,
                     "posix-handle-delete-disposition-unavailable");
      else
        error_result = dc_win32_error(error);
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    (void)dc_take_test_injection(DC_INJECT_DELETE_AFTER_COMMIT,
                                 &inject_action, &inject_close);
    if (inject_action) {
      error_result = dc_injected_action_error(DC_INJECT_DELETE_AFTER_COMMIT);
      failure_result = dc_internal_action_failure(target_value, error_result,
                                                  inject_close);
      target = (struct dc_handle *)Data_custom_val(target_value);
      CAMLreturn(dc_raw_delete_may_have_committed(
          target->state, target->state == DC_HANDLE_CLOSED, failure_result));
    }
    cleanup_result = dc_internal_close_terminal(target_value, inject_close);
    if (Tag_val(cleanup_result) == 1) {
      failure_result = dc_raw_cleanup_failure(cleanup_result);
      target = (struct dc_handle *)Data_custom_val(target_value);
      CAMLreturn(dc_raw_delete_may_have_committed(
          target->state, 0, failure_result));
    }
  }
#else
  error_result = dc_error(DC_WIN32, DC_UNSUPPORTED,
                          "posix-handle-delete-disposition-unavailable");
  CAMLreturn(dc_raw_delete_not_committed(error_result));
#endif
#else
#ifdef DC_HAVE_POSIX_ENVELOPE_DELETE
  {
    struct stat parent_stat, target_stat, binding_stat;
    char parent_identity[DC_IDENTITY_CAPACITY];
    char target_identity[DC_IDENTITY_CAPACITY];
    char binding_identity[DC_IDENTITY_CAPACITY];
    if (caml_string_length(target_leaf_value) == 0) {
      error_result = dc_error(DC_CONTRACT, DC_UNSUPPORTED,
                              "captured-delete-name-not-retained");
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (fstat(parent->os, &parent_stat) != 0) {
      error_result = dc_posix_error(errno);
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (!S_ISDIR(parent_stat.st_mode)) {
      error_result = dc_error(DC_POSIX, DC_NOT_DIRECTORY,
                              "captured-delete-parent-is-not-directory");
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (!dc_posix_owner_exclusive(&parent_stat)) {
      error_result = dc_error(DC_POSIX, DC_UNSUPPORTED,
                              "owner-exclusive-parent-unproven");
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (!dc_posix_identity(&parent_stat, parent_identity,
                           sizeof(parent_identity))) {
      error_result = dc_posix_error(EOVERFLOW);
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (strlen(parent_identity) != caml_string_length(parent_identity_value) ||
        memcmp(parent_identity, String_val(parent_identity_value),
               caml_string_length(parent_identity_value)) != 0) {
      error_result = dc_error(DC_CONTRACT, DC_OTHER,
                              "captured-delete-parent-identity-changed");
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (fstat(target->os, &target_stat) != 0) {
      error_result = dc_posix_error(errno);
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if ((target_kind == 0 && !S_ISREG(target_stat.st_mode)) ||
        (target_kind == 1 && !S_ISDIR(target_stat.st_mode))) {
      error_result = dc_error(
          DC_POSIX, target_kind == 0 ? DC_NOT_REGULAR : DC_NOT_DIRECTORY,
          "captured-delete-target-kind-mismatch");
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (!dc_posix_identity(&target_stat, target_identity,
                           sizeof(target_identity))) {
      error_result = dc_posix_error(EOVERFLOW);
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (strlen(target_identity) != caml_string_length(target_identity_value) ||
        memcmp(target_identity, String_val(target_identity_value),
               caml_string_length(target_identity_value)) != 0)
      CAMLreturn(dc_ok(Val_false));
    if (strcmp(parent_identity, target_identity) == 0) {
      error_result = dc_error(DC_CONTRACT, DC_OTHER,
                              "captured-delete-target-is-parent");
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (target_stat.st_dev != parent_stat.st_dev) {
      error_result = dc_error(DC_POSIX, DC_UNSUPPORTED,
                              "owner-exclusive-device-unproven");
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    (void)dc_take_test_injection(DC_INJECT_DELETE_BEFORE_COMMIT,
                                 &inject_action, &inject_close);
    if (inject_action || inject_close) {
      error_result =
          dc_injected_action_error(DC_INJECT_DELETE_BEFORE_COMMIT);
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    /* The retained capture name is authoritative only while it still binds
       the live target descriptor's identity.  The window between this check
       and the unlink is open only to a same-effective-user process inside
       the proven envelope; a rebound entry reports Identity_changed and is
       never deleted. */
    if (fstatat(parent->os, String_val(target_leaf_value), &binding_stat,
                AT_SYMLINK_NOFOLLOW) != 0) {
      if (errno == ENOENT) CAMLreturn(dc_ok(Val_false));
      error_result = dc_posix_error(errno);
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (!dc_posix_identity(&binding_stat, binding_identity,
                           sizeof(binding_identity))) {
      error_result = dc_posix_error(EOVERFLOW);
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    if (strcmp(binding_identity, target_identity) != 0)
      CAMLreturn(dc_ok(Val_false));
    if (unlinkat(parent->os, String_val(target_leaf_value),
                 target_kind == 1 ? AT_REMOVEDIR : 0) != 0) {
      if (errno == ENOENT) CAMLreturn(dc_ok(Val_false));
      error_result = dc_posix_error(errno);
      CAMLreturn(dc_raw_delete_not_committed(error_result));
    }
    /* The committed unlink itself proves namespace release; the terminal
       close below only settles the local handle state. */
    (void)dc_take_test_injection(DC_INJECT_DELETE_AFTER_COMMIT,
                                 &inject_action, &inject_close);
    if (inject_action) {
      error_result = dc_injected_action_error(DC_INJECT_DELETE_AFTER_COMMIT);
      failure_result = dc_internal_action_failure(target_value, error_result,
                                                  inject_close);
      target = (struct dc_handle *)Data_custom_val(target_value);
      CAMLreturn(dc_raw_delete_may_have_committed(target->state, 1,
                                                  failure_result));
    }
    cleanup_result = dc_internal_close_terminal(target_value, inject_close);
    if (Tag_val(cleanup_result) == 1) {
      failure_result = dc_raw_cleanup_failure(cleanup_result);
      target = (struct dc_handle *)Data_custom_val(target_value);
      CAMLreturn(dc_raw_delete_may_have_committed(target->state, 1,
                                                  failure_result));
    }
  }
#else
  (void)parent_identity_value;
  (void)target_identity_value;
  (void)target_leaf_value;
  (void)inject_action;
  (void)inject_close;
  error_result = dc_error(DC_POSIX, DC_UNSUPPORTED,
                          "atomic-captured-handle-delete-unavailable");
  CAMLreturn(dc_raw_delete_not_committed(error_result));
#endif
#endif
  result = dc_ok(Val_true);
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_dircap_delete_captured_byte(value *argv,
                                                         int argn) {
  (void)argn;
  return ocaml_mutants_dircap_delete_captured(argv[0], argv[1], argv[2],
                                              argv[3], argv[4], argv[5]);
}

CAMLprim value ocaml_mutants_dircap_try_lock(value parent_value,
                                              value name_value,
                                              value mode_value,
                                              value permissions_value,
                                              value root_identity_value) {
  CAMLparam5(parent_value, name_value, mode_value, permissions_value,
             root_identity_value);
  CAMLlocal5(handle_value, stat_value, payload, option_value, error_result);
  CAMLlocal1(result);
  struct dc_handle *parent = (struct dc_handle *)Data_custom_val(parent_value);
  int problem, mode = Int_val(mode_value);
  int permissions = Int_val(permissions_value);
  int inject_action = 0, inject_close = 0;
#ifdef _WIN32
  HANDLE parent_os;
#else
  int parent_os;
#endif
  if (dc_handle_problem(parent, &problem))
    CAMLreturn(dc_raw_operation_failure(dc_error(
        DC_NATIVE_DOMAIN, problem, problem == DC_WRONG_PROCESS ? "wrong-process"
                                            : "closed-capability")));
  if ((mode != 0 && mode != 1) || permissions < 0 || permissions > 07777)
    CAMLreturn(dc_raw_operation_failure(dc_error(
        DC_CONTRACT, DC_INVALID_NAME, "invalid-lock-arguments")));
  parent_os = parent->os;
  handle_value = dc_alloc_empty_handle();
  (void)dc_take_test_injection(DC_INJECT_LOCK_OPEN, &inject_action,
                               &inject_close);
  if (inject_action)
    CAMLreturn(
        dc_raw_operation_failure(dc_injected_action_error(DC_INJECT_LOCK_OPEN)));
#ifdef _WIN32
  {
    wchar_t *name = NULL;
    size_t name_length = 0;
    HANDLE lock_handle = INVALID_HANDLE_VALUE;
    NTSTATUS status;
    DWORD tag = 0, error = ERROR_SUCCESS;
    char identity[DC_IDENTITY_CAPACITY];
    char root_identity[DC_IDENTITY_CAPACITY];
    int kind, native_permissions;
    int64_t size, mtime_ns;
    OVERLAPPED range;
    DWORD flags = LOCKFILE_FAIL_IMMEDIATELY;
    (void)permissions;
    if (!dc_windows_component(String_val(name_value),
                              caml_string_length(name_value), &name,
                              &name_length))
      CAMLreturn(dc_raw_operation_failure(dc_error(
          DC_WIN32, DC_INVALID_NAME, "invalid-native-component")));
    status =
        dc_windows_open_lock(parent_os, name, name_length, &lock_handle);
    if (status < 0) {
      free(name);
      CAMLreturn(dc_raw_operation_failure(dc_nt_error(status)));
    }
    dc_arm_handle(handle_value, lock_handle);
    free(name);
    if (!dc_windows_identity(parent_os, root_identity,
                             sizeof(root_identity), &error)) {
      error_result = dc_win32_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result, 0));
    }
    if (strlen(root_identity) != caml_string_length(root_identity_value) ||
        memcmp(root_identity, String_val(root_identity_value),
               caml_string_length(root_identity_value)) != 0) {
      error_result = dc_error(DC_CONTRACT, DC_OTHER,
                              "lock-directory-identity-changed");
      CAMLreturn(dc_internal_action_failure(handle_value, error_result, 0));
    }
    problem = dc_reparse(lock_handle, &tag);
    if (problem < 0) {
      error = GetLastError();
      error_result = dc_win32_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result, 0));
    }
    if (problem > 0) {
      error_result =
          dc_error(DC_WIN32, DC_LINK_LIKE, "lock-file-is-reparse-point");
      CAMLreturn(dc_internal_action_failure(handle_value, error_result, 0));
    }
    if (!dc_windows_stat(lock_handle, 0, identity, sizeof(identity), &kind,
                         &size, &native_permissions, &mtime_ns, &error)) {
      error_result = dc_win32_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result, 0));
    }
    if (kind != 1) {
      error_result =
          dc_error(DC_WIN32, DC_NOT_REGULAR, "lock-file-is-not-regular");
      CAMLreturn(dc_internal_action_failure(handle_value, error_result, 0));
    }
    inject_action = 0;
    inject_close = 0;
    (void)dc_take_test_injection(DC_INJECT_LOCK_ACQUIRE, &inject_action,
                                 &inject_close);
    if (inject_action) {
      error_result = dc_injected_action_error(DC_INJECT_LOCK_ACQUIRE);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    memset(&range, 0, sizeof(range));
    if (mode == 1) flags |= LOCKFILE_EXCLUSIVE_LOCK;
    if (!LockFileEx(lock_handle, flags, 0, MAXDWORD, MAXDWORD, &range)) {
      error = GetLastError();
      if (error == ERROR_LOCK_VIOLATION) {
        error_result =
            dc_internal_close_terminal(handle_value, inject_close);
        if (Tag_val(error_result) == 1)
          CAMLreturn(dc_raw_cleanup_failure(error_result));
        CAMLreturn(dc_ok(Val_none));
      }
      error_result = dc_win32_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    stat_value = dc_alloc_raw_stat(identity, kind, size, native_permissions,
                                   mtime_ns);
  }
#else
#ifdef DC_HAVE_POSIX_OFD_LOCK
  {
    int lock_fd;
    int flags = O_RDWR | O_CREAT | O_NOFOLLOW | O_CLOEXEC | O_NONBLOCK;
    struct stat native_stat, root_stat;
    struct flock range;
    char identity[DC_IDENTITY_CAPACITY];
    char root_identity[DC_IDENTITY_CAPACITY];
    int kind, native_permissions;
    int64_t size, mtime_ns;
    lock_fd = openat(parent_os, String_val(name_value), flags,
                     (mode_t)permissions);
    if (lock_fd < 0)
      CAMLreturn(dc_raw_operation_failure(dc_posix_error(errno)));
    dc_arm_handle(handle_value, lock_fd);
    if (fstat(parent_os, &root_stat) != 0) {
      int error = errno;
      error_result = dc_posix_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result, 0));
    }
    if (!dc_posix_identity(&root_stat, root_identity, sizeof(root_identity))) {
      error_result = dc_posix_error(EOVERFLOW);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result, 0));
    }
    if (strlen(root_identity) != caml_string_length(root_identity_value) ||
        memcmp(root_identity, String_val(root_identity_value),
               caml_string_length(root_identity_value)) != 0) {
      error_result = dc_error(DC_CONTRACT, DC_OTHER,
                              "lock-directory-identity-changed");
      CAMLreturn(dc_internal_action_failure(handle_value, error_result, 0));
    }
    if (fstat(lock_fd, &native_stat) != 0) {
      int error = errno;
      error_result = dc_posix_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result, 0));
    }
    if (!S_ISREG(native_stat.st_mode)) {
      error_result =
          dc_error(DC_POSIX, DC_NOT_REGULAR, "lock-file-is-not-regular");
      CAMLreturn(dc_internal_action_failure(handle_value, error_result, 0));
    }
    if (!dc_posix_stat_value(&native_stat, identity, sizeof(identity), &kind,
                             &size, &native_permissions, &mtime_ns)) {
      int error = errno;
      error_result = dc_posix_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result, 0));
    }
    inject_action = 0;
    inject_close = 0;
    (void)dc_take_test_injection(DC_INJECT_LOCK_ACQUIRE, &inject_action,
                                 &inject_close);
    if (inject_action) {
      error_result = dc_injected_action_error(DC_INJECT_LOCK_ACQUIRE);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    memset(&range, 0, sizeof(range));
    range.l_type = mode == 0 ? F_RDLCK : F_WRLCK;
    range.l_whence = SEEK_SET;
    range.l_start = 0;
    range.l_len = 0;
    if (fcntl(lock_fd, F_OFD_SETLK, &range) != 0) {
      int error = errno;
      if (error == EACCES || error == EAGAIN) {
        error_result =
            dc_internal_close_terminal(handle_value, inject_close);
        if (Tag_val(error_result) == 1)
          CAMLreturn(dc_raw_cleanup_failure(error_result));
        CAMLreturn(dc_ok(Val_none));
      }
      error_result = dc_posix_error(error);
      CAMLreturn(dc_internal_action_failure(handle_value, error_result,
                                            inject_close));
    }
    stat_value = dc_alloc_raw_stat(identity, kind, size, native_permissions,
                                   mtime_ns);
  }
#else
  CAMLreturn(dc_raw_operation_failure(dc_error(
      DC_POSIX, DC_UNSUPPORTED, "open-file-description-locks-unavailable")));
#endif
#endif
  payload = caml_alloc_tuple(2);
  Store_field(payload, 0, handle_value);
  Store_field(payload, 1, stat_value);
  option_value = dc_some(payload);
  result = dc_ok(option_value);
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_dircap_release_lock(value handle_value,
                                                  value held_value) {
  CAMLparam2(handle_value, held_value);
  CAMLlocal3(unlock_error_result, close_error_result, result);
  struct dc_handle *handle =
      (struct dc_handle *)Data_custom_val(handle_value);
  int held = Bool_val(held_value);
  int local_only = handle->owner != dc_owner();
  int inject_unlock = 0, inject_close = 0;
  int unlock_failed = 0, unlock_kind = 0, unlock_native_error = 0;
  int close_failed = 0, close_injected = 0, close_native_error = 0;
  int namespace_released = !held && !local_only;
  int final_state;
  unlock_error_result = Val_unit;
  close_error_result = Val_unit;
  if (handle->state == DC_HANDLE_CLOSED)
    CAMLreturn(dc_ok(Val_bool(local_only)));
  if (handle->state == DC_HANDLE_INVALIDATED_UNKNOWN) {
#ifdef _WIN32
    unlock_error_result = dc_win32_error(
        handle->invalidated_error != 0 ? (DWORD)handle->invalidated_error
                                       : ERROR_INVALID_HANDLE);
#else
    unlock_error_result = dc_posix_error(handle->invalidated_error != 0
                                             ? handle->invalidated_error
                                             : EIO);
#endif
    CAMLreturn(dc_lock_cleanup_failure(
        unlock_error_result, DC_HANDLE_INVALIDATED_UNKNOWN,
        namespace_released, 0, Val_unit, DC_HANDLE_INVALIDATED_UNKNOWN,
        namespace_released));
  }
  (void)dc_take_test_injection(DC_INJECT_LOCK_RELEASE, &inject_unlock,
                               &inject_close);
  if (!local_only && held) {
    if (inject_unlock) {
      unlock_failed = 1;
      unlock_kind = 1;
    } else {
#ifdef _WIN32
      OVERLAPPED range;
      memset(&range, 0, sizeof(range));
      if (!UnlockFileEx(handle->os, 0, MAXDWORD, MAXDWORD, &range)) {
        unlock_failed = 1;
        unlock_native_error = (int)GetLastError();
      } else {
        namespace_released = 1;
      }
#else
#ifdef DC_HAVE_POSIX_OFD_LOCK
      struct flock range;
      memset(&range, 0, sizeof(range));
      range.l_type = F_UNLCK;
      range.l_whence = SEEK_SET;
      range.l_start = 0;
      range.l_len = 0;
      if (fcntl(handle->os, F_OFD_SETLK, &range) != 0) {
        unlock_failed = 1;
        unlock_native_error = errno;
      } else {
        namespace_released = 1;
      }
#else
      unlock_failed = 1;
      unlock_kind = 2;
#endif
#endif
    }
  }
#ifdef _WIN32
  if (inject_close) {
    close_failed = 1;
    close_injected = 1;
    final_state = DC_HANDLE_OPEN;
  } else if (!CloseHandle(handle->os)) {
    close_failed = 1;
    close_native_error = (int)GetLastError();
    final_state = DC_HANDLE_OPEN;
  } else {
    handle->os = INVALID_HANDLE_VALUE;
    handle->state = DC_HANDLE_CLOSED;
    final_state = DC_HANDLE_CLOSED;
  }
#else
  if (inject_close) {
    (void)close(handle->os);
    handle->os = -1;
    handle->state = DC_HANDLE_INVALIDATED_UNKNOWN;
    handle->invalidated_error = EIO;
    close_failed = 1;
    close_injected = 1;
    final_state = DC_HANDLE_INVALIDATED_UNKNOWN;
  } else if (close(handle->os) != 0) {
    close_native_error = errno;
    handle->os = -1;
    handle->state = DC_HANDLE_INVALIDATED_UNKNOWN;
    handle->invalidated_error = close_native_error;
    close_failed = 1;
    final_state = DC_HANDLE_INVALIDATED_UNKNOWN;
  } else {
    handle->os = -1;
    handle->state = DC_HANDLE_CLOSED;
    final_state = DC_HANDLE_CLOSED;
  }
#endif
  if (!close_failed && !local_only) namespace_released = 1;
  if (!unlock_failed && !close_failed)
    CAMLreturn(dc_ok(Val_bool(local_only)));
  if (unlock_failed) {
    if (unlock_kind == 1)
      unlock_error_result =
          dc_injected_action_error(DC_INJECT_LOCK_RELEASE);
    else if (unlock_kind == 2)
      unlock_error_result =
          dc_error(DC_POSIX, DC_UNSUPPORTED,
                   "open-file-description-locks-unavailable");
#ifdef _WIN32
    else
      unlock_error_result = dc_win32_error((DWORD)unlock_native_error);
#else
    else
      unlock_error_result = dc_posix_error(unlock_native_error);
#endif
  }
  if (close_failed) {
    if (close_injected)
      close_error_result = dc_error(DC_CONTRACT, DC_OTHER,
                                    "injected-lock-close-failure");
#ifdef _WIN32
    else
      close_error_result = dc_win32_error((DWORD)close_native_error);
#else
    else
      close_error_result = dc_posix_error(close_native_error);
#endif
  }
  if (unlock_failed)
    CAMLreturn(dc_lock_cleanup_failure(
        unlock_error_result, final_state, namespace_released, close_failed,
        close_error_result, final_state, namespace_released));
  result = dc_lock_cleanup_failure(
      close_error_result, final_state, namespace_released, 0, Val_unit,
      final_state, namespace_released);
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_dircap_atomic_rename(value source_value,
                                                   value target_parent_value,
                                                   value name_value,
                                                   value replacement_value,
                                                   value parent_identity_value,
                                                   value source_leaf_value) {
  CAMLparam5(source_value, target_parent_value, name_value, replacement_value,
             parent_identity_value);
  CAMLxparam1(source_leaf_value);
  CAMLlocal4(result, error_result, advisory, advisories);
  struct dc_handle *source =
      (struct dc_handle *)Data_custom_val(source_value);
  struct dc_handle *target_parent =
      (struct dc_handle *)Data_custom_val(target_parent_value);
  int source_problem, target_problem;
  int replacement = Int_val(replacement_value);
  int inject_before_commit = 0, inject_after_commit = 0;
  int consumption_advisory = 0;
  if (dc_handle_problem(source, &source_problem))
    CAMLreturn(dc_error(
        DC_NATIVE_DOMAIN, source_problem,
        source_problem == DC_WRONG_PROCESS ? "wrong-process"
                                           : "closed-capability"));
  if (dc_handle_problem(target_parent, &target_problem))
    CAMLreturn(dc_error(
        DC_NATIVE_DOMAIN, target_problem,
        target_problem == DC_WRONG_PROCESS ? "wrong-process"
                                           : "closed-capability"));
  if (replacement != 0 && replacement != 1)
    CAMLreturn(dc_error(DC_CONTRACT, DC_INVALID_NAME,
                        "invalid-publication-replacement-policy"));
  (void)dc_take_test_injection(DC_INJECT_PUBLISH, &inject_before_commit,
                               &inject_after_commit);
  if (inject_before_commit)
    CAMLreturn(dc_injected_action_error(DC_INJECT_PUBLISH));
#ifdef _WIN32
  {
    struct dc_file_rename_information_ex {
      ULONG flags;
      HANDLE root_directory;
      ULONG file_name_length;
      WCHAR file_name[1];
    };
    dc_nt_set_information_file_fn set_information =
        dc_nt_set_information_file();
    wchar_t *name = NULL;
    size_t name_length = 0;
    size_t name_bytes, copied_name_bytes, allocation_size;
    struct dc_file_rename_information_ex *rename_info;
    IO_STATUS_BLOCK io;
    NTSTATUS status;
    DWORD parent_error = ERROR_SUCCESS;
    char parent_identity[DC_IDENTITY_CAPACITY];
    int parent_kind, parent_permissions;
    int64_t parent_size, parent_mtime_ns;
    if (set_information == NULL)
      CAMLreturn(dc_error(DC_WIN32, DC_UNSUPPORTED,
                          "NtSetInformationFile-unavailable"));
    if (!dc_windows_stat(target_parent->os, 0, parent_identity,
                         sizeof(parent_identity), &parent_kind, &parent_size,
                         &parent_permissions, &parent_mtime_ns,
                         &parent_error))
      CAMLreturn(dc_win32_error(parent_error));
    if (parent_kind != 0)
      CAMLreturn(dc_error(DC_WIN32, DC_NOT_DIRECTORY,
                          "publication-parent-is-not-directory"));
    if (strlen(parent_identity) !=
            caml_string_length(parent_identity_value) ||
        memcmp(parent_identity, String_val(parent_identity_value),
               caml_string_length(parent_identity_value)) != 0)
      CAMLreturn(dc_error(DC_CONTRACT, DC_OTHER,
                          "publication-parent-identity-changed"));
    if (!dc_windows_component(String_val(name_value),
                              caml_string_length(name_value), &name,
                              &name_length))
      CAMLreturn(
          dc_error(DC_WIN32, DC_INVALID_NAME, "invalid-native-component"));
    if (name_length > MAXDWORD / sizeof(wchar_t)) {
      free(name);
      CAMLreturn(dc_error(DC_WIN32, DC_INVALID_NAME,
                          "rename-component-too-large"));
    }
    name_bytes = name_length * sizeof(wchar_t);
    if (name_bytes > SIZE_MAX - sizeof(wchar_t)) {
      free(name);
      CAMLreturn(dc_error(DC_WIN32, DC_INVALID_NAME,
                          "rename-component-too-large"));
    }
    copied_name_bytes = name_bytes + sizeof(wchar_t);
    if (copied_name_bytes >
        SIZE_MAX -
            offsetof(struct dc_file_rename_information_ex, file_name)) {
      free(name);
      CAMLreturn(dc_error(DC_WIN32, DC_INVALID_NAME,
                          "rename-component-too-large"));
    }
    allocation_size =
        offsetof(struct dc_file_rename_information_ex, file_name) +
        copied_name_bytes;
    if (allocation_size < sizeof(struct dc_file_rename_information_ex))
      allocation_size = sizeof(struct dc_file_rename_information_ex);
    if (allocation_size > (size_t)(~(ULONG)0)) {
      free(name);
      CAMLreturn(dc_error(DC_WIN32, DC_INVALID_NAME,
                          "rename-component-too-large"));
    }
    rename_info = (struct dc_file_rename_information_ex *)calloc(
        1, allocation_size);
    if (rename_info == NULL) {
      free(name);
      CAMLreturn(dc_win32_error(ERROR_NOT_ENOUGH_MEMORY));
    }
    rename_info->flags =
        replacement == 0 ? 0 : DC_FILE_RENAME_REPLACE_IF_EXISTS;
    rename_info->root_directory = target_parent->os;
    rename_info->file_name_length = (ULONG)name_bytes;
    memcpy(rename_info->file_name, name, copied_name_bytes);
    free(name);
    io.Status = 0;
    io.Information = 0;
    /* The source is the captured file handle and RootDirectory is the captured
       same-parent handle.  No ambient path is reconstructed. */
    status = set_information(source->os, &io, rename_info,
                             (ULONG)allocation_size,
                             DC_FILE_RENAME_INFORMATION_EX_CLASS);
    free(rename_info);
    if (status < 0) CAMLreturn(dc_nt_error(status));
  }
#else
#ifdef DC_HAVE_POSIX_FD_LINK_PUBLISH
  {
    struct stat parent_stat, source_stat, binding_stat, staged_stat;
    char parent_identity[DC_IDENTITY_CAPACITY];
    char binding_path[64];
    int formatted;
    if (fstat(target_parent->os, &parent_stat) != 0)
      CAMLreturn(dc_posix_error(errno));
    if (!S_ISDIR(parent_stat.st_mode))
      CAMLreturn(dc_error(DC_POSIX, DC_NOT_DIRECTORY,
                          "publication-parent-is-not-directory"));
    if (!dc_posix_identity(&parent_stat, parent_identity,
                           sizeof(parent_identity)))
      CAMLreturn(dc_posix_error(EOVERFLOW));
    if (strlen(parent_identity) !=
            caml_string_length(parent_identity_value) ||
        memcmp(parent_identity, String_val(parent_identity_value),
               caml_string_length(parent_identity_value)) != 0)
      CAMLreturn(dc_error(DC_CONTRACT, DC_OTHER,
                          "publication-parent-identity-changed"));
    if (fstat(source->os, &source_stat) != 0)
      CAMLreturn(dc_posix_error(errno));
    if (!S_ISREG(source_stat.st_mode))
      CAMLreturn(dc_error(DC_POSIX, DC_NOT_REGULAR,
                          "publication-source-is-not-regular"));
    /* The source binding is the descriptor itself: the commit links through
       the process file-descriptor namespace and never re-resolves a source
       name. */
    formatted = snprintf(binding_path, sizeof(binding_path),
                         "/proc/self/fd/%d", source->os);
    if (formatted < 0 || (size_t)formatted >= sizeof(binding_path))
      CAMLreturn(dc_posix_error(EOVERFLOW));
    if (fstatat(AT_FDCWD, binding_path, &binding_stat, 0) != 0 ||
        binding_stat.st_dev != source_stat.st_dev ||
        binding_stat.st_ino != source_stat.st_ino)
      CAMLreturn(dc_error(DC_POSIX, DC_UNSUPPORTED,
                          "proc-fd-binding-unavailable"));
    if (replacement == 0) {
      if (linkat(AT_FDCWD, binding_path, target_parent->os,
                 String_val(name_value), AT_SYMLINK_FOLLOW) != 0)
        CAMLreturn(dc_posix_error(errno));
    } else {
      /* Replace stages the descriptor under a fresh verified temporary name
         inside the destination's owner-exclusive envelope, then commits with
         one atomically replacing rename.  The staging window is open only to
         a same-effective-user process. */
      static atomic_uint dc_replace_counter = ATOMIC_VAR_INIT(0);
      char temp_name[64];
      char staged_identity[DC_IDENTITY_CAPACITY];
      char source_identity[DC_IDENTITY_CAPACITY];
      int attempt, staged = 0;
      if (!dc_posix_owner_exclusive(&parent_stat))
        CAMLreturn(dc_error(DC_POSIX, DC_UNSUPPORTED,
                            "owner-exclusive-parent-unproven"));
      if (!dc_posix_identity(&source_stat, source_identity,
                             sizeof(source_identity)))
        CAMLreturn(dc_posix_error(EOVERFLOW));
      for (attempt = 0; attempt < 32 && !staged; ++attempt) {
        unsigned serial = atomic_fetch_add_explicit(
            &dc_replace_counter, 1u, memory_order_relaxed);
        formatted = snprintf(temp_name, sizeof(temp_name),
                             ".dc-replace-%ju-%u", (uintmax_t)getpid(),
                             serial);
        if (formatted < 0 || (size_t)formatted >= sizeof(temp_name))
          CAMLreturn(dc_posix_error(EOVERFLOW));
        if (linkat(AT_FDCWD, binding_path, target_parent->os, temp_name,
                   AT_SYMLINK_FOLLOW) == 0)
          staged = 1;
        else if (errno != EEXIST)
          CAMLreturn(dc_posix_error(errno));
      }
      if (!staged)
        CAMLreturn(dc_error(DC_POSIX, DC_OTHER,
                            "replace-staging-name-exhausted"));
      if (fstatat(target_parent->os, temp_name, &staged_stat,
                  AT_SYMLINK_NOFOLLOW) != 0 ||
          !dc_posix_identity(&staged_stat, staged_identity,
                             sizeof(staged_identity)) ||
          strcmp(staged_identity, source_identity) != 0)
        /* The entry now at the staging name is not the link this call
           created; removing it would delete third-party work.  The residue
           is left for the path-based garbage collector. */
        CAMLreturn(dc_error(DC_CONTRACT, DC_OTHER,
                            "replace-staging-identity-changed"));
      if (renameat(target_parent->os, temp_name, target_parent->os,
                   String_val(name_value)) != 0) {
        int commit_error = errno;
        if (fstatat(target_parent->os, temp_name, &staged_stat,
                    AT_SYMLINK_NOFOLLOW) == 0 &&
            dc_posix_identity(&staged_stat, staged_identity,
                              sizeof(staged_identity)) &&
            strcmp(staged_identity, source_identity) == 0)
          (void)unlinkat(target_parent->os, temp_name, 0);
        CAMLreturn(dc_posix_error(commit_error));
      }
    }
    /* Post-commit cooperative cleanup: consume the staged source name only
       while it still binds the published inode.  A skipped or failed
       consumption is an ordered advisory and never affects the committed
       destination. */
    if (caml_string_length(source_leaf_value) ==
            caml_string_length(name_value) &&
        memcmp(String_val(source_leaf_value), String_val(name_value),
               caml_string_length(name_value)) == 0) {
      /* The destination name replaced the source's own name; the commit
         itself consumed it. */
    } else if (caml_string_length(source_leaf_value) == 0) {
      consumption_advisory = 1;
    } else if (fstatat(target_parent->os, String_val(source_leaf_value),
                       &staged_stat, AT_SYMLINK_NOFOLLOW) != 0) {
      if (errno != ENOENT) consumption_advisory = 1;
    } else if (staged_stat.st_dev != source_stat.st_dev ||
               staged_stat.st_ino != source_stat.st_ino) {
      consumption_advisory = 1;
    } else if (unlinkat(target_parent->os, String_val(source_leaf_value),
                        0) != 0) {
      if (errno != ENOENT) consumption_advisory = 1;
    }
  }
#else
  (void)name_value;
  (void)parent_identity_value;
  (void)source_leaf_value;
  CAMLreturn(dc_error(DC_POSIX, DC_UNSUPPORTED,
                      "stable-source-binding-for-rename-unavailable"));
#endif
#endif
  advisories = Val_emptylist;
  if (inject_after_commit) {
    error_result =
        dc_error(DC_CONTRACT, DC_OTHER, "injected-post-publish-advisory");
    advisory = caml_alloc(2, 0);
    Store_field(advisory, 0, Field(error_result, 0));
    Store_field(advisory, 1, Val_emptylist);
    advisories = advisory;
  }
  if (consumption_advisory) {
    error_result = dc_error(DC_CONTRACT, DC_OTHER,
                            "publication-source-name-not-consumed");
    advisory = caml_alloc(2, 0);
    Store_field(advisory, 0, Field(error_result, 0));
    Store_field(advisory, 1, advisories);
    advisories = advisory;
  }
  result = dc_ok(advisories);
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_dircap_atomic_rename_byte(value *argv,
                                                       int argn) {
  (void)argn;
  return ocaml_mutants_dircap_atomic_rename(argv[0], argv[1], argv[2],
                                            argv[3], argv[4], argv[5]);
}

CAMLprim value ocaml_mutants_dircap_read(value handle_value,
                                         value limit_value) {
  CAMLparam2(handle_value, limit_value);
  CAMLlocal4(contents_value, stat_value, payload, result);
  CAMLlocal1(buffer_guard);
  struct dc_handle *handle = (struct dc_handle *)Data_custom_val(handle_value);
  uint64_t limit = (uint64_t)Int64_val(limit_value);
  uint64_t maximum_string = ((uint64_t)Max_wosize - 1) * sizeof(value) - 1;
  unsigned char *buffer = NULL;
  size_t length = 0, capacity = 0;
  int problem;
  char identity[DC_IDENTITY_CAPACITY];
  int kind, permissions;
  int64_t size, mtime_ns;
#ifdef _WIN32
  HANDLE handle_os;
#else
  int handle_os;
#endif
  if (dc_handle_problem(handle, &problem))
    CAMLreturn(dc_error(
        DC_NATIVE_DOMAIN, problem, problem == DC_WRONG_PROCESS ? "wrong-process"
                                            : "closed-capability"));
  handle_os = handle->os;
  buffer_guard = dc_alloc_strings_guard();
  if (limit > maximum_string) limit = maximum_string;
#ifdef _WIN32
  {
    DWORD error = ERROR_SUCCESS;
    NTSTATUS status;
    if (!dc_windows_stat(handle_os, 0, identity, sizeof(identity), &kind,
                         &size, &permissions, &mtime_ns, &error))
      CAMLreturn(dc_win32_error(error));
    if (kind != 1) {
      CAMLreturn(dc_error(DC_WIN32, DC_NOT_REGULAR, "not-regular"));
    }
    if (size < 0 || (uint64_t)size > limit) {
      CAMLreturn(dc_error(DC_WIN32, DC_TOO_LARGE, "captured-file-too-large"));
    }
    capacity = size > 0 ? (size_t)size : (limit < BUFSIZ ? (size_t)limit : BUFSIZ);
    buffer = (unsigned char *)malloc(capacity == 0 ? 1 : capacity);
    if (buffer == NULL) CAMLreturn(dc_win32_error(ERROR_NOT_ENOUGH_MEMORY));
    for (;;) {
      ULONG read_count = 0;
      if (length == capacity) {
        if ((uint64_t)length == limit) {
          unsigned char extra;
          status = dc_windows_read_at(handle_os, &extra, 1,
                                      (uint64_t)length, &read_count);
          if (status != 0) {
            free(buffer);
            CAMLreturn(dc_nt_error(status));
          }
          if (read_count != 0) {
            free(buffer);
            CAMLreturn(dc_error(DC_WIN32, DC_TOO_LARGE,
                                "captured-file-grew-past-limit"));
          }
          break;
        }
        {
          uint64_t doubled = capacity == 0 ? BUFSIZ : (uint64_t)capacity * 2;
          size_t next = (size_t)(doubled > limit ? limit : doubled);
          unsigned char *grown = (unsigned char *)realloc(buffer, next);
          if (grown == NULL) {
            free(buffer);
            CAMLreturn(dc_win32_error(ERROR_NOT_ENOUGH_MEMORY));
          }
          buffer = grown;
          capacity = next;
        }
      }
      {
        size_t available = capacity - length;
        ULONG request = available > MAXDWORD ? MAXDWORD : (ULONG)available;
        status = dc_windows_read_at(handle_os, buffer + length, request,
                                    (uint64_t)length, &read_count);
        if (status != 0) {
          free(buffer);
          CAMLreturn(dc_nt_error(status));
        }
      }
      if (read_count == 0) break;
      length += read_count;
    }
    if (!dc_windows_stat(handle_os, 0, identity, sizeof(identity), &kind,
                         &size, &permissions, &mtime_ns, &error)) {
      free(buffer);
      CAMLreturn(dc_win32_error(error));
    }
  }
#else
  {
    struct stat native_stat;
    if (fstat(handle_os, &native_stat) != 0)
      CAMLreturn(dc_posix_error(errno));
    if (!dc_posix_stat_value(&native_stat, identity, sizeof(identity), &kind,
                             &size, &permissions, &mtime_ns))
      CAMLreturn(dc_posix_error(errno));
    if (kind != 1) {
      CAMLreturn(dc_error(DC_POSIX, DC_NOT_REGULAR, "not-regular"));
    }
    if (size < 0 || (uint64_t)size > limit) {
      CAMLreturn(dc_error(DC_POSIX, DC_TOO_LARGE, "captured-file-too-large"));
    }
    capacity = size > 0 ? (size_t)size : (limit < BUFSIZ ? (size_t)limit : BUFSIZ);
    buffer = (unsigned char *)malloc(capacity == 0 ? 1 : capacity);
    if (buffer == NULL) CAMLreturn(dc_posix_error(ENOMEM));
    for (;;) {
      ssize_t read_count;
      off_t offset = (off_t)length;
      if (offset < 0 || (uintmax_t)offset != (uintmax_t)length) {
        free(buffer);
        CAMLreturn(dc_posix_error(EOVERFLOW));
      }
      if (length == capacity) {
        if ((uint64_t)length == limit) {
          unsigned char extra;
          do {
            read_count = pread(handle_os, &extra, 1, offset);
          } while (read_count < 0 && errno == EINTR);
          if (read_count < 0) {
            int error = errno;
            free(buffer);
            CAMLreturn(dc_posix_error(error));
          }
          if (read_count != 0) {
            free(buffer);
            CAMLreturn(dc_error(DC_POSIX, DC_TOO_LARGE,
                                "captured-file-grew-past-limit"));
          }
          break;
        }
        {
          uint64_t doubled = capacity == 0 ? BUFSIZ : (uint64_t)capacity * 2;
          size_t next = (size_t)(doubled > limit ? limit : doubled);
          unsigned char *grown = (unsigned char *)realloc(buffer, next);
          if (grown == NULL) {
            free(buffer);
            CAMLreturn(dc_posix_error(ENOMEM));
          }
          buffer = grown;
          capacity = next;
        }
      }
      {
        size_t available = capacity - length;
        size_t request =
            available > (size_t)SSIZE_MAX ? (size_t)SSIZE_MAX : available;
        do {
          read_count = pread(handle_os, buffer + length, request, offset);
        } while (read_count < 0 && errno == EINTR);
      }
      if (read_count < 0) {
        int error = errno;
        free(buffer);
        CAMLreturn(dc_posix_error(error));
      }
      if (read_count == 0) break;
      length += (size_t)read_count;
    }
    if (fstat(handle_os, &native_stat) != 0) {
      int error = errno;
      free(buffer);
      CAMLreturn(dc_posix_error(error));
    }
    if (!dc_posix_stat_value(&native_stat, identity, sizeof(identity), &kind,
                             &size, &permissions, &mtime_ns)) {
      int error = errno;
      free(buffer);
      CAMLreturn(dc_posix_error(error));
    }
  }
#endif
  if (!dc_strings_guard_push(buffer_guard, (char *)buffer)) {
    free(buffer);
#ifdef _WIN32
    CAMLreturn(dc_win32_error(ERROR_NOT_ENOUGH_MEMORY));
#else
    CAMLreturn(dc_posix_error(ENOMEM));
#endif
  }
  contents_value = caml_alloc_initialized_string(length, (const char *)buffer);
  stat_value = dc_alloc_raw_stat(identity, kind, size, permissions, mtime_ns);
  payload = caml_alloc_tuple(2);
  Store_field(payload, 0, contents_value);
  Store_field(payload, 1, stat_value);
  result = dc_ok(payload);
  dc_strings_guard_free(buffer_guard);
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_dircap_read_link(value parent_value,
                                              value name_value) {
  CAMLparam2(parent_value, name_value);
  (void)parent_value;
  (void)name_value;
  CAMLreturn(dc_error(DC_NATIVE_DOMAIN, DC_UNSUPPORTED,
                      "captured-link-capability-not-implemented"));
#if 0
  CAMLlocal2(target_value, result);
  struct dc_handle *parent = (struct dc_handle *)Data_custom_val(parent_value);
  int problem;
  if (dc_handle_problem(parent, &problem))
    CAMLreturn(dc_error(
        DC_NATIVE_DOMAIN, problem, problem == DC_WRONG_PROCESS ? "wrong-process"
                                            : "closed-capability"));
#ifdef _WIN32
  {
    wchar_t *name = NULL;
    size_t name_length = 0;
    HANDLE link = INVALID_HANDLE_VALUE;
    NTSTATUS status;
    unsigned char *storage;
    DWORD returned = 0, tag = 0;
    dc_reparse_data_buffer *reparse;
    const wchar_t *path = NULL;
    size_t path_length = 0;
    char *encoded;
    if (!dc_windows_component(String_val(name_value),
                              caml_string_length(name_value), &name,
                              &name_length))
      CAMLreturn(dc_error(DC_WIN32, DC_INVALID_NAME,
                          "invalid-native-component"));
    status = dc_windows_open_relative(parent->os, name, name_length, 2,
                                      FILE_OPEN, &link);
    free(name);
    if (status < 0) CAMLreturn(dc_nt_error(status));
    problem = dc_reparse(link, &tag);
    if (problem < 0) {
      DWORD error = GetLastError();
      CloseHandle(link);
      CAMLreturn(dc_win32_error(error));
    }
    if (!problem) {
      CloseHandle(link);
      CAMLreturn(dc_error(DC_WIN32, DC_NOT_LINK, "entry-is-not-reparse"));
    }
    storage = (unsigned char *)malloc(MAXIMUM_REPARSE_DATA_BUFFER_SIZE);
    if (storage == NULL) {
      CloseHandle(link);
      CAMLreturn(dc_win32_error(ERROR_NOT_ENOUGH_MEMORY));
    }
    if (!DeviceIoControl(link, FSCTL_GET_REPARSE_POINT, NULL, 0, storage,
                         MAXIMUM_REPARSE_DATA_BUFFER_SIZE, &returned, NULL)) {
      DWORD error = GetLastError();
      free(storage);
      CloseHandle(link);
      CAMLreturn(dc_win32_error(error));
    }
    CloseHandle(link);
    reparse = (dc_reparse_data_buffer *)storage;
    if (reparse->ReparseTag == IO_REPARSE_TAG_SYMLINK) {
      USHORT offset =
          reparse->payload.SymbolicLinkReparseBuffer.PrintNameOffset;
      USHORT bytes =
          reparse->payload.SymbolicLinkReparseBuffer.PrintNameLength;
      if (bytes == 0) {
        offset =
            reparse->payload.SymbolicLinkReparseBuffer.SubstituteNameOffset;
        bytes =
            reparse->payload.SymbolicLinkReparseBuffer.SubstituteNameLength;
      }
      path = reparse->payload.SymbolicLinkReparseBuffer.PathBuffer +
             offset / sizeof(wchar_t);
      path_length = bytes / sizeof(wchar_t);
    } else if (reparse->ReparseTag == IO_REPARSE_TAG_MOUNT_POINT) {
      USHORT offset = reparse->payload.MountPointReparseBuffer.PrintNameOffset;
      USHORT bytes = reparse->payload.MountPointReparseBuffer.PrintNameLength;
      if (bytes == 0) {
        offset =
            reparse->payload.MountPointReparseBuffer.SubstituteNameOffset;
        bytes =
            reparse->payload.MountPointReparseBuffer.SubstituteNameLength;
      }
      path = reparse->payload.MountPointReparseBuffer.PathBuffer +
             offset / sizeof(wchar_t);
      path_length = bytes / sizeof(wchar_t);
    } else {
      free(storage);
      CAMLreturn(dc_error(DC_WIN32, DC_NOT_LINK,
                          "unsupported-reparse-tag"));
    }
    if ((const unsigned char *)(path + path_length) > storage + returned) {
      free(storage);
      CAMLreturn(dc_error(DC_WIN32, DC_OTHER,
                          "invalid-reparse-target-range"));
    }
    encoded = dc_wtf8_encode(path, path_length);
    free(storage);
    if (encoded == NULL) CAMLreturn(dc_win32_error(ERROR_NOT_ENOUGH_MEMORY));
    target_value = caml_copy_string(encoded);
    free(encoded);
  }
#else
#ifdef AT_SYMLINK_NOFOLLOW
  {
    struct stat stat;
    size_t capacity;
    char *buffer;
    ssize_t length;
    if (fstatat(parent->os, String_val(name_value), &stat,
                AT_SYMLINK_NOFOLLOW) != 0)
      CAMLreturn(dc_posix_error(errno));
    if (!S_ISLNK(stat.st_mode))
      CAMLreturn(dc_error(DC_POSIX, DC_NOT_LINK, "entry-is-not-link"));
    capacity = stat.st_size > 0 ? (size_t)stat.st_size + 1 : BUFSIZ;
    if (capacity == 0) CAMLreturn(dc_error(DC_POSIX, DC_TOO_LARGE,
                                           "link-target-too-large"));
    buffer = (char *)malloc(capacity);
    if (buffer == NULL) CAMLreturn(dc_posix_error(ENOMEM));
    for (;;) {
      length = readlinkat(parent->os, String_val(name_value), buffer, capacity);
      if (length < 0) {
        int error = errno;
        free(buffer);
        CAMLreturn(dc_posix_error(error));
      }
      if ((size_t)length < capacity) break;
      if (capacity > SIZE_MAX / 2) {
        free(buffer);
        CAMLreturn(dc_error(DC_POSIX, DC_TOO_LARGE,
                            "link-target-too-large"));
      }
      capacity *= 2;
      {
        char *grown = (char *)realloc(buffer, capacity);
        if (grown == NULL) {
          free(buffer);
          CAMLreturn(dc_posix_error(ENOMEM));
        }
        buffer = grown;
      }
    }
    target_value = caml_alloc_initialized_string((mlsize_t)length, buffer);
    free(buffer);
  }
#else
  CAMLreturn(dc_error(DC_POSIX, DC_UNSUPPORTED,
                      "readlinkat-no-follow-unavailable"));
#endif
#endif
  result = dc_ok(target_value);
  CAMLreturn(result);
#endif
}

CAMLprim value ocaml_mutants_dircap_close(value handle_value) {
  CAMLparam1(handle_value);
  CAMLlocal1(error_result);
  struct dc_handle *handle = (struct dc_handle *)Data_custom_val(handle_value);
  int local_only = handle->owner != dc_owner();
  if (handle->state == DC_HANDLE_CLOSED)
    CAMLreturn(dc_ok(Val_bool(local_only)));
  if (handle->state == DC_HANDLE_INVALIDATED_UNKNOWN) {
#ifdef _WIN32
    error_result = dc_win32_error(
        handle->invalidated_error != 0 ? (DWORD)handle->invalidated_error
                                       : ERROR_INVALID_HANDLE);
#else
    error_result = dc_posix_error(handle->invalidated_error != 0
                                      ? handle->invalidated_error
                                      : EIO);
#endif
    CAMLreturn(
        dc_close_error(error_result, DC_HANDLE_INVALIDATED_UNKNOWN));
  }
#ifdef _WIN32
  if (!CloseHandle(handle->os)) {
    error_result = dc_win32_error(GetLastError());
    CAMLreturn(dc_close_error(error_result, DC_HANDLE_OPEN));
  }
  handle->os = INVALID_HANDLE_VALUE;
#else
  if (close(handle->os) != 0) {
    int error = errno;
    handle->state = DC_HANDLE_INVALIDATED_UNKNOWN;
    handle->invalidated_error = error;
    handle->os = -1;
    error_result = dc_posix_error(error);
    CAMLreturn(
        dc_close_error(error_result, DC_HANDLE_INVALIDATED_UNKNOWN));
  }
  handle->os = -1;
#endif
  handle->state = DC_HANDLE_CLOSED;
  CAMLreturn(dc_ok(Val_bool(local_only)));
}

CAMLprim value ocaml_mutants_dircap_close_terminal(value handle_value) {
  CAMLparam1(handle_value);
  CAMLlocal1(error_result);
  struct dc_handle *handle = (struct dc_handle *)Data_custom_val(handle_value);
  int local_only = handle->owner != dc_owner();
  if (handle->state == DC_HANDLE_CLOSED)
    CAMLreturn(dc_ok(Val_bool(local_only)));
  if (handle->state == DC_HANDLE_INVALIDATED_UNKNOWN) {
#ifdef _WIN32
    error_result = dc_win32_error(
        handle->invalidated_error != 0 ? (DWORD)handle->invalidated_error
                                       : ERROR_INVALID_HANDLE);
#else
    error_result = dc_posix_error(handle->invalidated_error != 0
                                      ? handle->invalidated_error
                                      : EIO);
#endif
    CAMLreturn(
        dc_close_error(error_result, DC_HANDLE_INVALIDATED_UNKNOWN));
  }
#ifdef _WIN32
  if (!CloseHandle(handle->os)) {
    int error = (int)GetLastError();
    handle->os = INVALID_HANDLE_VALUE;
    handle->state = DC_HANDLE_INVALIDATED_UNKNOWN;
    handle->invalidated_error = error;
    error_result = dc_win32_error((DWORD)error);
    CAMLreturn(
        dc_close_error(error_result, DC_HANDLE_INVALIDATED_UNKNOWN));
  }
  handle->os = INVALID_HANDLE_VALUE;
#else
  if (close(handle->os) != 0) {
    int error = errno;
    handle->os = -1;
    handle->state = DC_HANDLE_INVALIDATED_UNKNOWN;
    handle->invalidated_error = error;
    error_result = dc_posix_error(error);
    CAMLreturn(
        dc_close_error(error_result, DC_HANDLE_INVALIDATED_UNKNOWN));
  }
  handle->os = -1;
#endif
  handle->state = DC_HANDLE_CLOSED;
  CAMLreturn(dc_ok(Val_bool(local_only)));
}
