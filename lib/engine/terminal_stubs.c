#define CAML_NAME_SPACE

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <errno.h>
#endif

CAMLprim value ocaml_mutants_windows_console_set_raw(value descriptor_value) {
  CAMLparam1(descriptor_value);
  CAMLlocal2(result, mode_value);
  int error = 0;
  int64_t original = 0;
#ifdef _WIN32
  HANDLE handle = Handle_val(descriptor_value);
  DWORD mode = 0;
  DWORD raw;
  if (!GetConsoleMode(handle, &mode)) {
    error = (int)GetLastError();
  } else {
    original = (int64_t)mode;
    raw = mode;
    raw &= ~(ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT |
             ENABLE_QUICK_EDIT_MODE);
    raw |= ENABLE_EXTENDED_FLAGS | ENABLE_VIRTUAL_TERMINAL_INPUT;
    if (!SetConsoleMode(handle, raw)) error = (int)GetLastError();
  }
#else
  (void)descriptor_value;
  error = ENOTSUP;
#endif
  mode_value = caml_copy_int64(original);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(error));
  Store_field(result, 1, mode_value);
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_windows_console_restore(value descriptor_value,
                                                      value mode_value) {
  CAMLparam2(descriptor_value, mode_value);
  int error = 0;
#ifdef _WIN32
  HANDLE handle = Handle_val(descriptor_value);
  DWORD mode = (DWORD)Int64_val(mode_value);
  if (!SetConsoleMode(handle, mode)) error = (int)GetLastError();
#else
  (void)descriptor_value;
  (void)mode_value;
  error = ENOTSUP;
#endif
  CAMLreturn(Val_int(error));
}

CAMLprim value ocaml_mutants_windows_console_prepare_output(
    value descriptor_value) {
  CAMLparam1(descriptor_value);
  CAMLlocal2(result, mode_value);
  int error = 0;
  int64_t original = 0;
  int code_page = 0;
#ifdef _WIN32
  HANDLE handle = Handle_val(descriptor_value);
  DWORD mode = 0;
  code_page = (int)GetConsoleOutputCP();
  if (!GetConsoleMode(handle, &mode)) {
    error = (int)GetLastError();
  } else {
    original = (int64_t)mode;
    if (!SetConsoleMode(handle,
                        mode | ENABLE_PROCESSED_OUTPUT |
                            ENABLE_VIRTUAL_TERMINAL_PROCESSING)) {
      error = (int)GetLastError();
    } else if (!SetConsoleOutputCP(CP_UTF8)) {
      error = (int)GetLastError();
      (void)SetConsoleMode(handle, mode);
    }
  }
#else
  (void)descriptor_value;
  error = ENOTSUP;
#endif
  mode_value = caml_copy_int64(original);
  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_int(error));
  Store_field(result, 1, mode_value);
  Store_field(result, 2, Val_int(code_page));
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_windows_console_restore_output(
    value descriptor_value, value mode_value, value code_page_value) {
  CAMLparam3(descriptor_value, mode_value, code_page_value);
  int error = 0;
#ifdef _WIN32
  HANDLE handle = Handle_val(descriptor_value);
  DWORD mode = (DWORD)Int64_val(mode_value);
  UINT code_page = (UINT)Int_val(code_page_value);
  if (!SetConsoleMode(handle, mode)) error = (int)GetLastError();
  if (code_page != 0 && !SetConsoleOutputCP(code_page) && error == 0)
    error = (int)GetLastError();
#else
  (void)descriptor_value;
  (void)mode_value;
  (void)code_page_value;
  error = ENOTSUP;
#endif
  CAMLreturn(Val_int(error));
}

CAMLprim value ocaml_mutants_windows_console_size(value descriptor_value) {
  CAMLparam1(descriptor_value);
  CAMLlocal1(result);
  int error = 0;
  int width = 80;
  int height = 24;
#ifdef _WIN32
  HANDLE handle = Handle_val(descriptor_value);
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo(handle, &info)) {
    error = (int)GetLastError();
  } else {
    width = info.srWindow.Right - info.srWindow.Left + 1;
    height = info.srWindow.Bottom - info.srWindow.Top + 1;
  }
#else
  (void)descriptor_value;
  error = ENOTSUP;
#endif
  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_int(error));
  Store_field(result, 1, Val_int(width));
  Store_field(result, 2, Val_int(height));
  CAMLreturn(result);
}

CAMLprim value ocaml_mutants_windows_console_flush_input(
    value descriptor_value) {
  CAMLparam1(descriptor_value);
  int error = 0;
#ifdef _WIN32
  HANDLE handle = Handle_val(descriptor_value);
  if (!FlushConsoleInputBuffer(handle)) error = (int)GetLastError();
#else
  (void)descriptor_value;
  error = ENOTSUP;
#endif
  CAMLreturn(Val_int(error));
}
