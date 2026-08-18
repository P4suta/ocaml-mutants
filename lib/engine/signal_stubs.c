#define CAML_NAME_SPACE

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <errno.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

/* The console dispatcher runs on a Windows-owned thread. It must not enter the
   OCaml runtime or invoke an OCaml closure. This process-lifetime event is the
   only object touched by that dispatcher; an ordinary OCaml thread performs
   the callback after [WaitForSingleObject] returns. */
static HANDLE interrupt_event = NULL;
static volatile LONG interrupt_active = 0;
static volatile LONG interrupt_pending = 0;

static BOOL WINAPI interrupt_console_handler(DWORD event) {
  if (event != CTRL_C_EVENT && event != CTRL_BREAK_EVENT) return FALSE;
  if (InterlockedCompareExchange(&interrupt_active, 1, 1) != 1) return FALSE;
  /* Coalesce console events until the ordinary OCaml worker consumes the
     pending notification.  The process-lifetime worker then rearms this latch
     so a later command can receive a later interrupt without reinstalling the
     OS handler. */
  if (InterlockedCompareExchange(&interrupt_pending, 1, 0) == 0) {
    (void)SetEvent(interrupt_event);
  }
  return TRUE;
}
#endif

CAMLprim value ocaml_mutants_console_interrupt_install(value unit) {
  CAMLparam1(unit);
#ifdef _WIN32
  DWORD error;
  if (InterlockedCompareExchange(&interrupt_active, -1, 0) != 0)
    CAMLreturn(Val_int((int)ERROR_BUSY));
  if (interrupt_event == NULL) {
    interrupt_event = CreateEventW(NULL, FALSE, FALSE, NULL);
    if (interrupt_event == NULL) {
      error = GetLastError();
      InterlockedExchange(&interrupt_active, 0);
      CAMLreturn(Val_int((int)error));
    }
  }
  InterlockedExchange(&interrupt_pending, 0);
  (void)ResetEvent(interrupt_event);
  InterlockedExchange(&interrupt_active, 1);
  if (!SetConsoleCtrlHandler(interrupt_console_handler, TRUE)) {
    error = GetLastError();
    InterlockedExchange(&interrupt_active, 0);
    CAMLreturn(Val_int((int)error));
  }
  CAMLreturn(Val_int(0));
#else
  CAMLreturn(Val_int(ENOTSUP));
#endif
}

CAMLprim value ocaml_mutants_console_interrupt_wait(value unit) {
  CAMLparam1(unit);
#ifdef _WIN32
  DWORD waited;
  if (interrupt_event == NULL) CAMLreturn(Val_int(-ERROR_INVALID_HANDLE));
  caml_enter_blocking_section();
  waited = WaitForSingleObject(interrupt_event, INFINITE);
  caml_leave_blocking_section();
  if (waited != WAIT_OBJECT_0)
    CAMLreturn(Val_int(-(int)GetLastError()));
  if (InterlockedCompareExchange(&interrupt_active, 1, 1) != 1)
    CAMLreturn(Val_int(0));
  if (InterlockedExchange(&interrupt_pending, 0) != 1)
    CAMLreturn(Val_int(-ERROR_INVALID_DATA));
  CAMLreturn(Val_int(1));
#else
  CAMLreturn(Val_int(-ENOTSUP));
#endif
}

CAMLprim value ocaml_mutants_console_interrupt_restore(value unit) {
  CAMLparam1(unit);
#ifdef _WIN32
  DWORD error = ERROR_SUCCESS;
  if (InterlockedCompareExchange(&interrupt_active, 1, 1) != 1)
    CAMLreturn(Val_int((int)ERROR_INVALID_STATE));
  if (!SetConsoleCtrlHandler(interrupt_console_handler, FALSE))
    error = GetLastError();
  InterlockedExchange(&interrupt_active, 0);
  (void)SetEvent(interrupt_event);
  CAMLreturn(Val_int((int)error));
#else
  CAMLreturn(Val_int(ENOTSUP));
#endif
}
