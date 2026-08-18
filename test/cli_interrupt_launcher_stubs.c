#define CAML_NAME_SPACE

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/osdeps.h>

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <wchar.h>
#else
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <spawn.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/syscall.h>
#endif

extern char **environ;
#endif

enum interrupt_process_state {
  INTERRUPT_PROCESS_EMPTY = 0,
  INTERRUPT_PROCESS_RUNNING = 1,
  INTERRUPT_PROCESS_REAPED = 2,
  INTERRUPT_PROCESS_CLOSED = 3
};

struct interrupt_process {
  enum interrupt_process_state state;
#ifdef _WIN32
  HANDLE process;
  HANDLE job;
  DWORD pid;
#else
  pid_t pid;
#endif
};

static int interrupt_hard_terminate(struct interrupt_process *process) {
  if (process->state != INTERRUPT_PROCESS_RUNNING) return 0;
#ifdef _WIN32
  if (process->job == NULL) return (int)ERROR_INVALID_HANDLE;
  if (!TerminateJobObject(process->job, 1)) return (int)GetLastError();
#else
  if (kill(-process->pid, SIGKILL) != 0 && errno != ESRCH) return errno;
#endif
  return 0;
}

static void interrupt_release(struct interrupt_process *process) {
  if (process->state == INTERRUPT_PROCESS_CLOSED) return;
  (void)interrupt_hard_terminate(process);
#ifdef _WIN32
  if (process->job != NULL) {
    CloseHandle(process->job);
    process->job = NULL;
  }
  if (process->process != NULL) {
    CloseHandle(process->process);
    process->process = NULL;
  }
#else
  if (process->state == INTERRUPT_PROCESS_RUNNING) {
    int status;
    while (waitpid(process->pid, &status, 0) < 0 && errno == EINTR) {
    }
  }
#endif
  process->state = INTERRUPT_PROCESS_CLOSED;
}

static void interrupt_finalize(value process_value) {
  struct interrupt_process *process =
      (struct interrupt_process *)Data_custom_val(process_value);
  interrupt_release(process);
}

static struct custom_operations interrupt_process_operations = {
    .identifier = "ocaml-mutants.test.interrupt-process.v1",
    .finalize = interrupt_finalize,
    .compare = custom_compare_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .compare_ext = custom_compare_ext_default,
    .fixed_length = custom_fixed_length_default};

static value interrupt_alloc_process(void) {
  value process_value =
      caml_alloc_custom(&interrupt_process_operations,
                        sizeof(struct interrupt_process), 0, 1);
  struct interrupt_process *process =
      (struct interrupt_process *)Data_custom_val(process_value);
  memset(process, 0, sizeof(*process));
  process->state = INTERRUPT_PROCESS_EMPTY;
  return process_value;
}

static value interrupt_tagged(int tag, value payload) {
  CAMLparam1(payload);
  CAMLlocal1(result);
  result = caml_alloc(1, tag);
  Store_field(result, 0, payload);
  CAMLreturn(result);
}

static value interrupt_code_pair(int kind, int code) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(kind));
  Store_field(result, 1, Val_int(code));
  CAMLreturn(result);
}

#ifdef _WIN32
static void interrupt_free_wide_arguments(wchar_t **arguments,
                                          mlsize_t argument_count) {
  mlsize_t index;
  if (arguments == NULL) return;
  for (index = 0; index < argument_count; ++index)
    caml_stat_free(arguments[index]);
  caml_stat_free(arguments);
}

static size_t interrupt_quoted_length(const wchar_t *argument) {
  size_t length = 2;
  size_t slashes = 0;
  const wchar_t *cursor;
  for (cursor = argument; *cursor != L'\0'; ++cursor) {
    if (*cursor == L'\\') {
      ++slashes;
    } else if (*cursor == L'"') {
      length += (slashes * 2) + 2;
      slashes = 0;
    } else {
      length += slashes + 1;
      slashes = 0;
    }
  }
  return length + (slashes * 2);
}

static wchar_t *interrupt_append_quoted(wchar_t *destination,
                                        const wchar_t *argument) {
  size_t slashes = 0;
  const wchar_t *cursor;
  *destination++ = L'"';
  for (cursor = argument; *cursor != L'\0'; ++cursor) {
    if (*cursor == L'\\') {
      ++slashes;
    } else if (*cursor == L'"') {
      while (slashes > 0) {
        *destination++ = L'\\';
        *destination++ = L'\\';
        --slashes;
      }
      *destination++ = L'\\';
      *destination++ = L'"';
    } else {
      while (slashes > 0) {
        *destination++ = L'\\';
        --slashes;
      }
      *destination++ = *cursor;
    }
  }
  while (slashes > 0) {
    *destination++ = L'\\';
    *destination++ = L'\\';
    --slashes;
  }
  *destination++ = L'"';
  return destination;
}

static wchar_t *interrupt_command_line(value arguments_value,
                                        mlsize_t argument_count,
                                        wchar_t ***wide_arguments_out,
                                        int *error_out) {
  wchar_t **wide_arguments = NULL;
  wchar_t *command_line = NULL;
  wchar_t *destination;
  size_t command_length = 1;
  mlsize_t index;

  wide_arguments = caml_stat_alloc(argument_count * sizeof(wchar_t *));
  memset(wide_arguments, 0, argument_count * sizeof(wchar_t *));
  for (index = 0; index < argument_count; ++index) {
    value argument_value = Field(arguments_value, index);
    if (memchr(String_val(argument_value), '\0',
               caml_string_length(argument_value)) != NULL) {
      *error_out = ERROR_INVALID_PARAMETER;
      interrupt_free_wide_arguments(wide_arguments, argument_count);
      return NULL;
    }
    wide_arguments[index] =
        caml_stat_strdup_to_utf16(String_val(argument_value));
    if (wide_arguments[index] == NULL) {
      *error_out = ERROR_NOT_ENOUGH_MEMORY;
      interrupt_free_wide_arguments(wide_arguments, argument_count);
      return NULL;
    }
    command_length += interrupt_quoted_length(wide_arguments[index]);
    if (index + 1 < argument_count) ++command_length;
  }
  if (command_length > SIZE_MAX / sizeof(wchar_t)) {
    *error_out = ERROR_NOT_ENOUGH_MEMORY;
    interrupt_free_wide_arguments(wide_arguments, argument_count);
    return NULL;
  }
  command_line = caml_stat_alloc(command_length * sizeof(wchar_t));
  destination = command_line;
  for (index = 0; index < argument_count; ++index) {
    destination = interrupt_append_quoted(destination, wide_arguments[index]);
    if (index + 1 < argument_count) *destination++ = L' ';
  }
  *destination = L'\0';
  *wide_arguments_out = wide_arguments;
  return command_line;
}

static wchar_t *interrupt_wide_path(value path_value, int *error_out) {
  wchar_t *path;
  if (memchr(String_val(path_value), '\0', caml_string_length(path_value)) !=
      NULL) {
    *error_out = ERROR_INVALID_PARAMETER;
    return NULL;
  }
  path = caml_stat_strdup_to_utf16(String_val(path_value));
  if (path == NULL) *error_out = ERROR_NOT_ENOUGH_MEMORY;
  return path;
}

static void interrupt_close_handle(HANDLE *handle) {
  if (*handle != NULL && *handle != INVALID_HANDLE_VALUE) CloseHandle(*handle);
  *handle = NULL;
}
#endif

/* A liveness witness pins the kernel identity of an authenticated PID so the
   later exit proof cannot be confused by PID reuse: Windows retains a real
   process handle, Linux retains a pidfd. Platforms without such a mechanism
   report Unavailable and the caller falls back to PID polling. */

struct interrupt_witness {
  int closed;
#ifdef _WIN32
  HANDLE process;
#else
  int pidfd;
#endif
};

static void interrupt_witness_release(struct interrupt_witness *witness) {
  if (witness->closed) return;
#ifdef _WIN32
  if (witness->process != NULL) {
    CloseHandle(witness->process);
    witness->process = NULL;
  }
#else
  if (witness->pidfd >= 0) {
    close(witness->pidfd);
    witness->pidfd = -1;
  }
#endif
  witness->closed = 1;
}

static void interrupt_witness_finalize(value witness_value) {
  interrupt_witness_release(
      (struct interrupt_witness *)Data_custom_val(witness_value));
}

static struct custom_operations interrupt_witness_operations = {
    .identifier = "ocaml-mutants.test.interrupt-witness.v1",
    .finalize = interrupt_witness_finalize,
    .compare = custom_compare_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .compare_ext = custom_compare_ext_default,
    .fixed_length = custom_fixed_length_default};

/* The kernel's own answer, bypassing any runtime-cached notion of the
   process id: the readiness protocol authenticates this exact value. */
CAMLprim value ocaml_mutants_test_current_pid(value unit) {
  CAMLparam1(unit);
#ifdef _WIN32
  CAMLreturn(Val_int((int)GetCurrentProcessId()));
#else
  CAMLreturn(Val_int((int)getpid()));
#endif
}

CAMLprim value ocaml_mutants_test_witness_open(value pid_value) {
  CAMLparam1(pid_value);
  CAMLlocal1(witness_value);
  struct interrupt_witness *witness;
  witness_value = caml_alloc_custom(&interrupt_witness_operations,
                                    sizeof(struct interrupt_witness), 0, 1);
  witness = (struct interrupt_witness *)Data_custom_val(witness_value);
  memset(witness, 0, sizeof(*witness));
#ifdef _WIN32
  witness->process =
      OpenProcess(SYNCHRONIZE | PROCESS_QUERY_LIMITED_INFORMATION, FALSE,
                  (DWORD)Int_val(pid_value));
  if (witness->process == NULL) {
    DWORD error = GetLastError();
    witness->closed = 1;
    if (error == ERROR_INVALID_PARAMETER)
      CAMLreturn(Val_int(1)); /* Already_exited */
    CAMLreturn(interrupt_tagged(1, Val_int((int)error))); /* Open_failed */
  }
  CAMLreturn(interrupt_tagged(0, witness_value)); /* Witness */
#elif defined(__linux__) && defined(SYS_pidfd_open)
  witness->pidfd = (int)syscall(SYS_pidfd_open, (pid_t)Int_val(pid_value), 0);
  if (witness->pidfd < 0) {
    int error = errno;
    witness->closed = 1;
    if (error == ESRCH) CAMLreturn(Val_int(1));   /* Already_exited */
    if (error == ENOSYS) CAMLreturn(Val_int(0));  /* Unavailable */
    CAMLreturn(interrupt_tagged(1, Val_int(error))); /* Open_failed */
  }
  CAMLreturn(interrupt_tagged(0, witness_value)); /* Witness */
#else
  witness->closed = 1;
  CAMLreturn(Val_int(0)); /* Unavailable */
#endif
}

CAMLprim value ocaml_mutants_test_witness_exited(value witness_value) {
  CAMLparam1(witness_value);
  struct interrupt_witness *witness =
      (struct interrupt_witness *)Data_custom_val(witness_value);
  if (witness->closed) CAMLreturn(Val_true);
#ifdef _WIN32
  CAMLreturn(
      Val_bool(WaitForSingleObject(witness->process, 0) == WAIT_OBJECT_0));
#else
  {
    struct pollfd probe;
    int observed;
    probe.fd = witness->pidfd;
    probe.events = POLLIN;
    probe.revents = 0;
    do {
      observed = poll(&probe, 1, 0);
    } while (observed < 0 && errno == EINTR);
    CAMLreturn(Val_bool(observed > 0 &&
                        (probe.revents & (POLLIN | POLLHUP)) != 0));
  }
#endif
}

/* The image seen through the retained witness handle itself: present only
   while the witness's process object still names an executable. Separating
   this from the fresh by-PID lookup distinguishes a genuinely alive
   descendant from a PID recycled to an unrelated process. */
CAMLprim value ocaml_mutants_test_witness_handle_image(value witness_value) {
  CAMLparam1(witness_value);
  CAMLlocal2(result, image_value);
#ifdef _WIN32
  {
    struct interrupt_witness *witness =
        (struct interrupt_witness *)Data_custom_val(witness_value);
    wchar_t image[MAX_PATH];
    DWORD length = MAX_PATH;
    if (witness->closed || witness->process == NULL) CAMLreturn(Val_int(0));
    if (!QueryFullProcessImageNameW(witness->process, 0, image, &length))
      CAMLreturn(Val_int(0));
    image_value = caml_copy_string_of_os(image);
    result = caml_alloc(1, 0);
    Store_field(result, 0, image_value);
    CAMLreturn(result);
  }
#else
  (void)witness_value;
  (void)image_value;
  CAMLreturn(Val_int(0));
#endif
}

CAMLprim value ocaml_mutants_test_witness_image(value pid_value) {
  CAMLparam1(pid_value);
  CAMLlocal2(result, image_value);
#ifdef _WIN32
  {
    HANDLE process =
        OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE,
                    (DWORD)Int_val(pid_value));
    wchar_t image[MAX_PATH];
    DWORD length = MAX_PATH;
    if (process == NULL) CAMLreturn(Val_int(0));
    if (!QueryFullProcessImageNameW(process, 0, image, &length)) {
      CloseHandle(process);
      CAMLreturn(Val_int(0));
    }
    CloseHandle(process);
    image_value = caml_copy_string_of_os(image);
    result = caml_alloc(1, 0);
    Store_field(result, 0, image_value);
    CAMLreturn(result);
  }
#elif defined(__linux__)
  {
    char link[64];
    char image[4096];
    ssize_t length;
    snprintf(link, sizeof(link), "/proc/%d/exe", Int_val(pid_value));
    length = readlink(link, image, sizeof(image) - 1);
    if (length <= 0) CAMLreturn(Val_int(0));
    image[length] = '\0';
    image_value = caml_copy_string(image);
    result = caml_alloc(1, 0);
    Store_field(result, 0, image_value);
    CAMLreturn(result);
  }
#else
  CAMLreturn(Val_int(0));
#endif
}

CAMLprim value ocaml_mutants_test_interrupt_start(value request_value) {
  CAMLparam1(request_value);
  CAMLlocal2(process_value, code_value);
  value arguments_value = Field(request_value, 0);
  value stdout_path_value = Field(request_value, 1);
  value stderr_path_value = Field(request_value, 2);
  mlsize_t argument_count = Wosize_val(arguments_value);
  struct interrupt_process *process;

  if (argument_count == 0) {
    code_value = Val_int(EINVAL);
    CAMLreturn(interrupt_tagged(2, code_value));
  }
  process_value = interrupt_alloc_process();
  process = (struct interrupt_process *)Data_custom_val(process_value);

#ifdef _WIN32
  {
    wchar_t **wide_arguments = NULL;
    wchar_t *command_line = NULL;
    wchar_t *stdout_path = NULL;
    wchar_t *stderr_path = NULL;
    int conversion_error = ERROR_SUCCESS;
    STARTUPINFOEXW startup;
    PROCESS_INFORMATION info;
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION limits;
    SECURITY_ATTRIBUTES security;
    HANDLE inherited[3] = {NULL, NULL, NULL};
    SIZE_T attribute_bytes = 0;
    LPPROC_THREAD_ATTRIBUTE_LIST attributes = NULL;
    int attributes_initialized = 0;
    int index;
    DWORD error = ERROR_SUCCESS;

    command_line = interrupt_command_line(arguments_value, argument_count,
                                          &wide_arguments, &conversion_error);
    if (command_line == NULL) {
      code_value = Val_int(conversion_error);
      CAMLreturn(interrupt_tagged(2, code_value));
    }
    stdout_path = interrupt_wide_path(stdout_path_value, &conversion_error);
    if (stdout_path == NULL) {
      error = (DWORD)conversion_error;
      goto launch_failed;
    }
    stderr_path = interrupt_wide_path(stderr_path_value, &conversion_error);
    if (stderr_path == NULL) {
      error = (DWORD)conversion_error;
      goto launch_failed;
    }
    ZeroMemory(&startup, sizeof(startup));
    startup.StartupInfo.cb = sizeof(startup);
    startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    ZeroMemory(&info, sizeof(info));
    ZeroMemory(&limits, sizeof(limits));
    ZeroMemory(&security, sizeof(security));
    security.nLength = sizeof(security);
    security.bInheritHandle = TRUE;

    inherited[0] =
        CreateFileW(L"NUL", GENERIC_READ,
                    FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                    &security, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (inherited[0] == INVALID_HANDLE_VALUE) {
      error = GetLastError();
      inherited[0] = NULL;
      goto launch_failed;
    }
    inherited[1] =
        CreateFileW(stdout_path, GENERIC_WRITE,
                    FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                    &security, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    if (inherited[1] == INVALID_HANDLE_VALUE) {
      error = GetLastError();
      inherited[1] = NULL;
      goto launch_failed;
    }
    inherited[2] =
        CreateFileW(stderr_path, GENERIC_WRITE,
                    FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                    &security, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    if (inherited[2] == INVALID_HANDLE_VALUE) {
      error = GetLastError();
      inherited[2] = NULL;
      goto launch_failed;
    }
    startup.StartupInfo.hStdInput = inherited[0];
    startup.StartupInfo.hStdOutput = inherited[1];
    startup.StartupInfo.hStdError = inherited[2];

    InitializeProcThreadAttributeList(NULL, 1, 0, &attribute_bytes);
    if (GetLastError() != ERROR_INSUFFICIENT_BUFFER || attribute_bytes == 0) {
      error = GetLastError();
      goto launch_failed;
    }
    attributes = (LPPROC_THREAD_ATTRIBUTE_LIST)HeapAlloc(
        GetProcessHeap(), 0, attribute_bytes);
    if (attributes == NULL) {
      error = ERROR_NOT_ENOUGH_MEMORY;
      goto launch_failed;
    }
    if (!InitializeProcThreadAttributeList(attributes, 1, 0,
                                           &attribute_bytes)) {
      error = GetLastError();
      goto launch_failed;
    }
    attributes_initialized = 1;
    startup.lpAttributeList = attributes;
    if (!UpdateProcThreadAttribute(attributes, 0,
                                   PROC_THREAD_ATTRIBUTE_HANDLE_LIST, inherited,
                                   sizeof(inherited), NULL, NULL)) {
      error = GetLastError();
      goto launch_failed;
    }

    process->job = CreateJobObjectW(NULL, NULL);
    if (process->job == NULL) {
      error = GetLastError();
      goto launch_failed;
    }
    limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if (!SetInformationJobObject(process->job,
                                 JobObjectExtendedLimitInformation, &limits,
                                 sizeof(limits))) {
      error = GetLastError();
      goto launch_failed;
    }
    if (!CreateProcessW(NULL, command_line, NULL, NULL, TRUE,
                        CREATE_NEW_PROCESS_GROUP | CREATE_SUSPENDED |
                            EXTENDED_STARTUPINFO_PRESENT,
                        NULL, NULL, &startup.StartupInfo, &info)) {
      error = GetLastError();
      goto launch_failed;
    }
    process->process = info.hProcess;
    process->pid = info.dwProcessId;
    if (!AssignProcessToJobObject(process->job, process->process)) {
      error = GetLastError();
      TerminateProcess(process->process, 1);
      WaitForSingleObject(process->process, INFINITE);
      CloseHandle(info.hThread);
      if (error == ERROR_ACCESS_DENIED || error == ERROR_NOT_SUPPORTED)
        goto launch_unsupported;
      goto launch_failed;
    }
    if (ResumeThread(info.hThread) == (DWORD)-1) {
      error = GetLastError();
      TerminateJobObject(process->job, 1);
      WaitForSingleObject(process->process, INFINITE);
      CloseHandle(info.hThread);
      goto launch_failed;
    }
    CloseHandle(info.hThread);
    process->state = INTERRUPT_PROCESS_RUNNING;
    for (index = 0; index < 3; ++index)
      interrupt_close_handle(&inherited[index]);
    DeleteProcThreadAttributeList(attributes);
    HeapFree(GetProcessHeap(), 0, attributes);
    caml_stat_free(stdout_path);
    caml_stat_free(stderr_path);
    caml_stat_free(command_line);
    interrupt_free_wide_arguments(wide_arguments, argument_count);
    CAMLreturn(interrupt_tagged(0, process_value));

  launch_unsupported:
    for (index = 0; index < 3; ++index)
      interrupt_close_handle(&inherited[index]);
    if (attributes_initialized) DeleteProcThreadAttributeList(attributes);
    if (attributes != NULL) HeapFree(GetProcessHeap(), 0, attributes);
    caml_stat_free(stdout_path);
    caml_stat_free(stderr_path);
    caml_stat_free(command_line);
    interrupt_free_wide_arguments(wide_arguments, argument_count);
    interrupt_release(process);
    code_value = Val_int((int)error);
    CAMLreturn(interrupt_tagged(1, code_value));

  launch_failed:
    for (index = 0; index < 3; ++index)
      interrupt_close_handle(&inherited[index]);
    if (attributes_initialized) DeleteProcThreadAttributeList(attributes);
    if (attributes != NULL) HeapFree(GetProcessHeap(), 0, attributes);
    caml_stat_free(stdout_path);
    caml_stat_free(stderr_path);
    caml_stat_free(command_line);
    interrupt_free_wide_arguments(wide_arguments, argument_count);
    interrupt_release(process);
    code_value = Val_int((int)error);
    CAMLreturn(interrupt_tagged(2, code_value));
  }
#else
#ifdef POSIX_SPAWN_SETPGROUP
  {
    char **arguments = caml_stat_alloc((argument_count + 1) * sizeof(char *));
    posix_spawnattr_t attributes;
    posix_spawn_file_actions_t actions;
    int attributes_initialized = 0;
    int actions_initialized = 0;
    int stdout_fd = -1;
    int stderr_fd = -1;
    int error;
    mlsize_t index;

    for (index = 0; index < argument_count; ++index)
      arguments[index] = (char *)String_val(Field(arguments_value, index));
    arguments[argument_count] = NULL;
    stdout_fd = open(String_val(stdout_path_value),
                     O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (stdout_fd < 0) error = errno;
    else {
      stderr_fd = open(String_val(stderr_path_value),
                       O_WRONLY | O_CREAT | O_TRUNC, 0600);
      error = stderr_fd < 0 ? errno : 0;
    }
    if (error == 0) error = posix_spawn_file_actions_init(&actions);
    if (error == 0) {
      actions_initialized = 1;
      error = posix_spawn_file_actions_addopen(&actions, STDIN_FILENO,
                                               "/dev/null", O_RDONLY, 0);
    }
    if (error == 0)
      error =
          posix_spawn_file_actions_adddup2(&actions, stdout_fd, STDOUT_FILENO);
    if (error == 0)
      error =
          posix_spawn_file_actions_adddup2(&actions, stderr_fd, STDERR_FILENO);
    if (error == 0 && stdout_fd != STDOUT_FILENO && stdout_fd != STDERR_FILENO)
      error = posix_spawn_file_actions_addclose(&actions, stdout_fd);
    if (error == 0 && stderr_fd != STDOUT_FILENO && stderr_fd != STDERR_FILENO)
      error = posix_spawn_file_actions_addclose(&actions, stderr_fd);
    if (error == 0) error = posix_spawnattr_init(&attributes);
    if (error == 0) {
      attributes_initialized = 1;
      error = posix_spawnattr_setpgroup(&attributes, 0);
    }
    if (error == 0)
      error = posix_spawnattr_setflags(&attributes, POSIX_SPAWN_SETPGROUP);
    if (error == 0)
      error = posix_spawnp(&process->pid, arguments[0], &actions, &attributes,
                           arguments, environ);
    if (attributes_initialized) posix_spawnattr_destroy(&attributes);
    if (actions_initialized) posix_spawn_file_actions_destroy(&actions);
    if (stdout_fd >= 0) close(stdout_fd);
    if (stderr_fd >= 0) close(stderr_fd);
    caml_stat_free(arguments);
    if (error != 0) {
      code_value = Val_int(error);
      CAMLreturn(interrupt_tagged(2, code_value));
    }
    process->state = INTERRUPT_PROCESS_RUNNING;
    CAMLreturn(interrupt_tagged(0, process_value));
  }
#else
  code_value = Val_int(ENOTSUP);
  CAMLreturn(interrupt_tagged(1, code_value));
#endif
#endif
}

CAMLprim value ocaml_mutants_test_interrupt_pid(value process_value) {
  CAMLparam1(process_value);
  struct interrupt_process *process =
      (struct interrupt_process *)Data_custom_val(process_value);
#ifdef _WIN32
  CAMLreturn(Val_int((int)process->pid));
#else
  CAMLreturn(Val_int((int)process->pid));
#endif
}

CAMLprim value ocaml_mutants_test_interrupt_send(value process_value) {
  CAMLparam1(process_value);
  struct interrupt_process *process =
      (struct interrupt_process *)Data_custom_val(process_value);
  if (process->state != INTERRUPT_PROCESS_RUNNING)
    CAMLreturn(interrupt_code_pair(2, ECHILD));
#ifdef _WIN32
  if (!GenerateConsoleCtrlEvent(CTRL_BREAK_EVENT, process->pid)) {
    DWORD error = GetLastError();
    if (error == ERROR_INVALID_HANDLE)
      CAMLreturn(interrupt_code_pair(1, (int)error));
    CAMLreturn(interrupt_code_pair(2, (int)error));
  }
#else
  if (kill(-process->pid, SIGINT) != 0)
    CAMLreturn(interrupt_code_pair(2, errno));
#endif
  CAMLreturn(interrupt_code_pair(0, 0));
}

CAMLprim value ocaml_mutants_test_interrupt_wait(value process_value,
                                                  value timeout_value,
                                                  value observation_value) {
  CAMLparam3(process_value, timeout_value, observation_value);
  struct interrupt_process *process =
      (struct interrupt_process *)Data_custom_val(process_value);
  int timeout_milliseconds = Int_val(timeout_value);
  int observation_milliseconds = Int_val(observation_value);
  if (process->state != INTERRUPT_PROCESS_RUNNING)
    CAMLreturn(interrupt_code_pair(3, ECHILD));
  if (timeout_milliseconds <= 0 || observation_milliseconds <= 0)
    CAMLreturn(interrupt_code_pair(3, EINVAL));
#ifdef _WIN32
  {
    DWORD waited = WaitForSingleObject(process->process,
                                       (DWORD)timeout_milliseconds);
    DWORD exit_code;
    if (waited == WAIT_TIMEOUT) CAMLreturn(interrupt_code_pair(2, 0));
    if (waited != WAIT_OBJECT_0)
      CAMLreturn(interrupt_code_pair(3, (int)GetLastError()));
    if (!GetExitCodeProcess(process->process, &exit_code))
      CAMLreturn(interrupt_code_pair(3, (int)GetLastError()));
    process->state = INTERRUPT_PROCESS_REAPED;
    CAMLreturn(interrupt_code_pair(0, (int)exit_code));
  }
#else
  {
    int elapsed = 0;
    int status;
    while (1) {
      pid_t waited = waitpid(process->pid, &status, WNOHANG);
      if (waited == process->pid) {
        process->state = INTERRUPT_PROCESS_REAPED;
        if (WIFEXITED(status))
          CAMLreturn(interrupt_code_pair(0, WEXITSTATUS(status)));
        if (WIFSIGNALED(status))
          CAMLreturn(interrupt_code_pair(1, WTERMSIG(status)));
        CAMLreturn(interrupt_code_pair(3, ECHILD));
      }
      if (waited < 0) {
        if (errno == EINTR) continue;
        CAMLreturn(interrupt_code_pair(3, errno));
      }
      if (elapsed >= timeout_milliseconds)
        CAMLreturn(interrupt_code_pair(2, 0));
      {
        int remaining = timeout_milliseconds - elapsed;
        int interval = observation_milliseconds < remaining
                           ? observation_milliseconds
                           : remaining;
        int poll_result;
        do {
          poll_result = poll(NULL, 0, interval);
        } while (poll_result < 0 && errno == EINTR);
        if (poll_result < 0)
          CAMLreturn(interrupt_code_pair(3, errno));
        elapsed += interval;
      }
    }
  }
#endif
}

CAMLprim value ocaml_mutants_test_interrupt_terminate(value process_value) {
  CAMLparam1(process_value);
  struct interrupt_process *process =
      (struct interrupt_process *)Data_custom_val(process_value);
  int error = interrupt_hard_terminate(process);
  CAMLreturn(interrupt_code_pair(error == 0 ? 0 : 1, error));
}

CAMLprim value ocaml_mutants_test_interrupt_close(value process_value) {
  CAMLparam1(process_value);
  struct interrupt_process *process =
      (struct interrupt_process *)Data_custom_val(process_value);
  interrupt_release(process);
  CAMLreturn(Val_unit);
}
