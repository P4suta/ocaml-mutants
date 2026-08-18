#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/osdeps.h>
#include <caml/unixsupport.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#include <wchar.h>
#else
#include <errno.h>
#include <signal.h>
#include <spawn.h>
#include <unistd.h>
#endif

#ifdef _WIN32
static void process_free_wide_array(wchar_t **values, mlsize_t count) {
  mlsize_t index;
  if (values == NULL) return;
  for (index = 0; index < count; ++index) caml_stat_free(values[index]);
  caml_stat_free(values);
}

static size_t process_quoted_length(const wchar_t *argument) {
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

static wchar_t *process_append_quoted(wchar_t *destination,
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

static wchar_t *process_command_line(value arguments_value,
                                     mlsize_t argument_count,
                                     wchar_t ***wide_arguments_out,
                                     DWORD *error_out) {
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
      process_free_wide_array(wide_arguments, argument_count);
      return NULL;
    }
    wide_arguments[index] =
        caml_stat_strdup_to_utf16(String_val(argument_value));
    if (wide_arguments[index] == NULL) {
      *error_out = ERROR_NOT_ENOUGH_MEMORY;
      process_free_wide_array(wide_arguments, argument_count);
      return NULL;
    }
    command_length += process_quoted_length(wide_arguments[index]);
    if (index + 1 < argument_count) ++command_length;
  }
  if (command_length > SIZE_MAX / sizeof(wchar_t)) {
    *error_out = ERROR_NOT_ENOUGH_MEMORY;
    process_free_wide_array(wide_arguments, argument_count);
    return NULL;
  }
  command_line = caml_stat_alloc(command_length * sizeof(wchar_t));
  destination = command_line;
  for (index = 0; index < argument_count; ++index) {
    destination = process_append_quoted(destination, wide_arguments[index]);
    if (index + 1 < argument_count) *destination++ = L' ';
  }
  *destination = L'\0';
  *wide_arguments_out = wide_arguments;
  return command_line;
}

static int process_compare_environment(const void *left, const void *right) {
  const wchar_t *left_value = *(const wchar_t *const *)left;
  const wchar_t *right_value = *(const wchar_t *const *)right;
  return _wcsicmp(left_value, right_value);
}

static wchar_t *process_environment_block(value environment_value,
                                          mlsize_t environment_count,
                                          wchar_t ***wide_environment_out,
                                          DWORD *error_out) {
  wchar_t **wide_environment = NULL;
  wchar_t *block = NULL;
  wchar_t *destination;
  size_t block_length = environment_count == 0 ? 2 : 1;
  mlsize_t index;

  wide_environment = caml_stat_alloc(
      (environment_count == 0 ? 1 : environment_count) * sizeof(wchar_t *));
  memset(wide_environment, 0,
         (environment_count == 0 ? 1 : environment_count) *
             sizeof(wchar_t *));
  for (index = 0; index < environment_count; ++index) {
    value binding_value = Field(environment_value, index);
    if (memchr(String_val(binding_value), '\0',
               caml_string_length(binding_value)) != NULL) {
      *error_out = ERROR_INVALID_PARAMETER;
      process_free_wide_array(wide_environment, environment_count);
      return NULL;
    }
    wide_environment[index] =
        caml_stat_strdup_to_utf16(String_val(binding_value));
    if (wide_environment[index] == NULL) {
      *error_out = ERROR_NOT_ENOUGH_MEMORY;
      process_free_wide_array(wide_environment, environment_count);
      return NULL;
    }
    block_length += wcslen(wide_environment[index]) + 1;
  }
  qsort(wide_environment, environment_count, sizeof(wchar_t *),
        process_compare_environment);
  if (block_length > SIZE_MAX / sizeof(wchar_t)) {
    *error_out = ERROR_NOT_ENOUGH_MEMORY;
    process_free_wide_array(wide_environment, environment_count);
    return NULL;
  }
  block = caml_stat_alloc(block_length * sizeof(wchar_t));
  destination = block;
  for (index = 0; index < environment_count; ++index) {
    size_t length = wcslen(wide_environment[index]);
    memcpy(destination, wide_environment[index], length * sizeof(wchar_t));
    destination += length;
    *destination++ = L'\0';
  }
  *destination = L'\0';
  *wide_environment_out = wide_environment;
  return block;
}

static void process_close_handle(HANDLE *handle) {
  if (*handle != NULL && *handle != INVALID_HANDLE_VALUE) CloseHandle(*handle);
  *handle = NULL;
}
#endif

CAMLprim value ocaml_mutants_posix_spawn(value arguments) {
  CAMLparam1(arguments);
#ifdef _WIN32
  CAMLreturn(Val_int(-1));
#else
  value program_value = Field(arguments, 0);
  value argv_value = Field(arguments, 1);
  value environment_value = Field(arguments, 2);
  value stdin_value = Field(arguments, 3);
  value stdout_value = Field(arguments, 4);
  value stderr_value = Field(arguments, 5);
  mlsize_t argument_count = Wosize_val(argv_value);
  mlsize_t environment_count = Wosize_val(environment_value);
  char **arguments_copy =
      caml_stat_alloc((argument_count + 1) * sizeof(char *));
  char **environment_copy =
      caml_stat_alloc((environment_count + 1) * sizeof(char *));
  for (mlsize_t index = 0; index < argument_count; ++index)
    arguments_copy[index] = (char *)String_val(Field(argv_value, index));
  arguments_copy[argument_count] = NULL;
  for (mlsize_t index = 0; index < environment_count; ++index)
    environment_copy[index] =
        (char *)String_val(Field(environment_value, index));
  environment_copy[environment_count] = NULL;

  posix_spawn_file_actions_t actions;
  int error = posix_spawn_file_actions_init(&actions);
  int actions_initialized = error == 0;
  if (error == 0)
    error = posix_spawn_file_actions_adddup2(
        &actions, Int_val(stdin_value), STDIN_FILENO);
  if (error == 0)
    error = posix_spawn_file_actions_adddup2(
        &actions, Int_val(stdout_value), STDOUT_FILENO);
  if (error == 0)
    error = posix_spawn_file_actions_adddup2(
        &actions, Int_val(stderr_value), STDERR_FILENO);

  pid_t pid = 0;
  if (error == 0)
    error = posix_spawnp(&pid, String_val(program_value), &actions, NULL,
                         arguments_copy, environment_copy);
  if (actions_initialized) posix_spawn_file_actions_destroy(&actions);
  caml_stat_free(arguments_copy);
  caml_stat_free(environment_copy);
  CAMLreturn(Val_int(error == 0 ? pid : -error));
#endif
}

CAMLprim value ocaml_mutants_windows_spawn_process_group(value arguments) {
  CAMLparam1(arguments);
#ifdef _WIN32
  value program_value = Field(arguments, 0);
  value argv_value = Field(arguments, 1);
  value environment_value = Field(arguments, 2);
  value stdin_value = Field(arguments, 3);
  value stdout_value = Field(arguments, 4);
  value stderr_value = Field(arguments, 5);
  mlsize_t argument_count = Wosize_val(argv_value);
  mlsize_t environment_count = Wosize_val(environment_value);
  wchar_t **wide_arguments = NULL;
  wchar_t **wide_environment = NULL;
  wchar_t *command_line = NULL;
  wchar_t *environment_block = NULL;
  wchar_t *program = NULL;
  HANDLE inherited[3] = {NULL, NULL, NULL};
  HANDLE current = GetCurrentProcess();
  STARTUPINFOEXW startup;
  PROCESS_INFORMATION info;
  SIZE_T attribute_bytes = 0;
  LPPROC_THREAD_ATTRIBUTE_LIST attributes = NULL;
  DWORD error = ERROR_SUCCESS;
  DWORD creation_flags = CREATE_NEW_PROCESS_GROUP | CREATE_UNICODE_ENVIRONMENT |
                         EXTENDED_STARTUPINFO_PRESENT;
  int index;

  if (argument_count == 0) CAMLreturn(Val_int(-ERROR_INVALID_PARAMETER));
  if (memchr(String_val(program_value), '\0',
             caml_string_length(program_value)) != NULL)
    CAMLreturn(Val_int(-ERROR_INVALID_PARAMETER));
  program = caml_stat_strdup_to_utf16(String_val(program_value));
  if (program == NULL) CAMLreturn(Val_int(-ERROR_NOT_ENOUGH_MEMORY));
  command_line = process_command_line(argv_value, argument_count,
                                      &wide_arguments, &error);
  if (command_line == NULL) goto spawn_failed;
  environment_block = process_environment_block(
      environment_value, environment_count, &wide_environment, &error);
  if (environment_block == NULL) goto spawn_failed;

  ZeroMemory(&startup, sizeof(startup));
  startup.StartupInfo.cb = sizeof(startup);
  startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
  ZeroMemory(&info, sizeof(info));
  if (!DuplicateHandle(current, Handle_val(stdin_value), current,
                       &inherited[0], 0, TRUE, DUPLICATE_SAME_ACCESS) ||
      !DuplicateHandle(current, Handle_val(stdout_value), current,
                       &inherited[1], 0, TRUE, DUPLICATE_SAME_ACCESS) ||
      !DuplicateHandle(current, Handle_val(stderr_value), current,
                       &inherited[2], 0, TRUE, DUPLICATE_SAME_ACCESS)) {
    error = GetLastError();
    goto spawn_failed;
  }
  startup.StartupInfo.hStdInput = inherited[0];
  startup.StartupInfo.hStdOutput = inherited[1];
  startup.StartupInfo.hStdError = inherited[2];

  InitializeProcThreadAttributeList(NULL, 1, 0, &attribute_bytes);
  if (GetLastError() != ERROR_INSUFFICIENT_BUFFER || attribute_bytes == 0) {
    error = GetLastError();
    goto spawn_failed;
  }
  attributes = (LPPROC_THREAD_ATTRIBUTE_LIST)HeapAlloc(
      GetProcessHeap(), 0, attribute_bytes);
  if (attributes == NULL) {
    error = ERROR_NOT_ENOUGH_MEMORY;
    goto spawn_failed;
  }
  if (!InitializeProcThreadAttributeList(attributes, 1, 0,
                                         &attribute_bytes)) {
    error = GetLastError();
    goto spawn_failed;
  }
  startup.lpAttributeList = attributes;
  if (!UpdateProcThreadAttribute(attributes, 0,
                                 PROC_THREAD_ATTRIBUTE_HANDLE_LIST, inherited,
                                 sizeof(inherited), NULL, NULL)) {
    error = GetLastError();
    goto spawn_failed;
  }
  if (!CreateProcessW(program, command_line, NULL, NULL, TRUE, creation_flags,
                      environment_block, NULL, &startup.StartupInfo, &info)) {
    error = GetLastError();
    goto spawn_failed;
  }
  CloseHandle(info.hThread);
  for (index = 0; index < 3; ++index) process_close_handle(&inherited[index]);
  DeleteProcThreadAttributeList(attributes);
  HeapFree(GetProcessHeap(), 0, attributes);
  caml_stat_free(program);
  caml_stat_free(command_line);
  caml_stat_free(environment_block);
  process_free_wide_array(wide_arguments, argument_count);
  process_free_wide_array(wide_environment, environment_count);
  CAMLreturn(Val_long((intnat)info.hProcess));

spawn_failed:
  for (index = 0; index < 3; ++index) process_close_handle(&inherited[index]);
  if (attributes != NULL) {
    if (startup.lpAttributeList != NULL)
      DeleteProcThreadAttributeList(attributes);
    HeapFree(GetProcessHeap(), 0, attributes);
  }
  caml_stat_free(program);
  caml_stat_free(command_line);
  caml_stat_free(environment_block);
  process_free_wide_array(wide_arguments, argument_count);
  process_free_wide_array(wide_environment, environment_count);
  CAMLreturn(Val_int(-(int)error));
#else
  (void)arguments;
  CAMLreturn(Val_int(-1));
#endif
}

CAMLprim value ocaml_mutants_atomic_replace(value source_value,
                                            value destination_value) {
  CAMLparam2(source_value, destination_value);
#ifdef _WIN32
  wchar_t *source = caml_stat_strdup_to_utf16(String_val(source_value));
  wchar_t *destination =
      caml_stat_strdup_to_utf16(String_val(destination_value));
  BOOL replaced =
      MoveFileExW(source, destination,
                  MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH);
  caml_stat_free(source);
  caml_stat_free(destination);
  CAMLreturn(Val_bool(replaced));
#else
  CAMLreturn(Val_bool(rename(String_val(source_value),
                           String_val(destination_value)) == 0));
#endif
}

CAMLprim value ocaml_mutants_windows_process_id(value pid_value) {
  CAMLparam1(pid_value);
#ifdef _WIN32
  DWORD pid = GetProcessId((HANDLE)Long_val(pid_value));
  CAMLreturn(Val_int(pid));
#else
  CAMLreturn(pid_value);
#endif
}
CAMLprim value ocaml_mutants_process_is_alive(value pid_value) {
  CAMLparam1(pid_value);
#ifdef _WIN32
  HANDLE process =
      OpenProcess(SYNCHRONIZE | PROCESS_QUERY_LIMITED_INFORMATION, FALSE,
                  (DWORD)Int_val(pid_value));
  if (process == NULL) CAMLreturn(Val_false);
  DWORD exit_code = 0;
  BOOL running =
      GetExitCodeProcess(process, &exit_code) && exit_code == STILL_ACTIVE;
  CloseHandle(process);
  CAMLreturn(Val_bool(running));
#else
  int result = kill(Int_val(pid_value), 0);
  CAMLreturn(Val_bool(result == 0 || errno == EPERM));
#endif
}
CAMLprim value ocaml_mutants_job_create(value unit) {
  CAMLparam1(unit);
#ifdef _WIN32
  HANDLE job = CreateJobObjectW(NULL, NULL);
  if (job != NULL) {
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION info;
    ZeroMemory(&info, sizeof(info));
    info.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if (!SetInformationJobObject(job, JobObjectExtendedLimitInformation, &info,
                                 sizeof(info))) {
      CloseHandle(job);
      job = NULL;
    }
  }
  CAMLreturn(caml_copy_nativeint((intnat)job));
#else
  CAMLreturn(caml_copy_nativeint(0));
#endif
}

CAMLprim value ocaml_mutants_job_assign(value job_value, value pid_value) {
  CAMLparam2(job_value, pid_value);
#ifdef _WIN32
  HANDLE job = (HANDLE)Nativeint_val(job_value);
  HANDLE process = (HANDLE)Long_val(pid_value);
  BOOL assigned =
      job != NULL && process != NULL && AssignProcessToJobObject(job, process);
  CAMLreturn(Val_bool(assigned));
#else
  CAMLreturn(Val_false);
#endif
}

CAMLprim value ocaml_mutants_job_terminate(value job_value) {
  CAMLparam1(job_value);
#ifdef _WIN32
  HANDLE job = (HANDLE)Nativeint_val(job_value);
  BOOL terminated = job != NULL && TerminateJobObject(job, 1);
  CAMLreturn(Val_bool(terminated));
#else
  CAMLreturn(Val_false);
#endif
}

CAMLprim value ocaml_mutants_job_close(value job_value) {
  CAMLparam1(job_value);
#ifdef _WIN32
  HANDLE job = (HANDLE)Nativeint_val(job_value);
  if (job != NULL) CloseHandle(job);
#endif
  CAMLreturn(Val_unit);
}

/* A liveness witness pins one observed process while it is provably alive.
   Windows recycles PIDs aggressively, so polling a bare PID can report an
   unrelated newborn process as the observed one; a retained handle always
   answers for the original process.  Waiting with a zero timeout also avoids
   the STILL_ACTIVE exit-code ambiguity. */
CAMLprim value ocaml_mutants_process_open_witness(value pid_value) {
  CAMLparam1(pid_value);
#ifdef _WIN32
  HANDLE process =
      OpenProcess(SYNCHRONIZE, FALSE, (DWORD)Int_val(pid_value));
  CAMLreturn(caml_copy_nativeint((intnat)process));
#else
  (void)pid_value;
  CAMLreturn(caml_copy_nativeint(0));
#endif
}

CAMLprim value ocaml_mutants_process_witness_is_alive(value handle_value) {
  CAMLparam1(handle_value);
#ifdef _WIN32
  HANDLE process = (HANDLE)Nativeint_val(handle_value);
  BOOL running =
      process != NULL && WaitForSingleObject(process, 0) == WAIT_TIMEOUT;
  CAMLreturn(Val_bool(running));
#else
  (void)handle_value;
  CAMLreturn(Val_false);
#endif
}

/* Diagnostic only: whether the calling process is inside any Job Object.
   1 = yes, 0 = no, -1 = unknown (query failed or non-Windows).  CI runners
   may wrap every step in their own Job, so a "yes" is inconclusive while a
   "no" is a definitive escape from every Job including the supervisor's. */
CAMLprim value ocaml_mutants_process_in_any_job(value unit) {
  CAMLparam1(unit);
#ifdef _WIN32
  BOOL in_job = FALSE;
  if (!IsProcessInJob(GetCurrentProcess(), NULL, &in_job))
    CAMLreturn(Val_int(-1));
  CAMLreturn(Val_int(in_job ? 1 : 0));
#else
  CAMLreturn(Val_int(-1));
#endif
}

CAMLprim value ocaml_mutants_process_witness_close(value handle_value) {
  CAMLparam1(handle_value);
#ifdef _WIN32
  HANDLE process = (HANDLE)Nativeint_val(handle_value);
  if (process != NULL) CloseHandle(process);
#endif
  CAMLreturn(Val_unit);
}
