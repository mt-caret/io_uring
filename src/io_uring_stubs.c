#include "config.h"
#define _FILE_OFFSET_BITS 64
#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <poll.h>
#ifndef ENOATTR
# define ENOATTR ENODATA
#endif
#include <arpa/inet.h>
#include <assert.h>

// TOIMPL: move this to jst-config?
#include <liburing.h>

#include "ocaml_utils.h"
#include "unix_utils.h"

/* Bytes_val is only available from 4.06 */
#ifndef Bytes_val
#define Bytes_val String_val
#endif

/** Core io_uring methods **/

#define POLL_FLAG(FLAG) DEFINE_INT63_CONSTANT (poll_##FLAG##_flag, FLAG)

POLL_FLAG(POLLIN)
POLL_FLAG(POLLOUT)
POLL_FLAG(POLLPRI)
POLL_FLAG(POLLERR)
POLL_FLAG(POLLHUP)

CAMLprim value io_uring_sizeof_io_uring_cqe(value __unused v_unit)
{
  return Val_int(sizeof(struct io_uring_cqe));
}

CAMLprim value io_uring_offsetof_user_data(value __unused v_unit)
{
  return Val_int(offsetof(struct io_uring_cqe, user_data));
}

CAMLprim value io_uring_offsetof_res(value __unused v_unit)
{
  return Val_int(offsetof(struct io_uring_cqe, res));
}

CAMLprim value io_uring_offsetof_flags(value __unused v_unit)
{
  return Val_int(offsetof(struct io_uring_cqe, flags));
}

#define Io_uring_val(v) (*((struct io_uring **) Data_abstract_val(v)))
#define Io_uring_cqe_val(v) ((struct io_uring_cqe *) Data_abstract_val(v))

CAMLprim value io_uring_queue_init_stub(value v_submission_entries, value v_completion_entries)
{
  struct io_uring_params p;
  CAMLparam2(v_submission_entries, v_completion_entries);
  CAMLlocal1(v_io_uring);

  memset(&p, 0, sizeof(p));
  p.flags = IORING_SETUP_CQSIZE;
  p.cq_entries = Int_val(v_completion_entries);

  int retcode;
  struct io_uring *io_uring = caml_stat_alloc(sizeof(struct io_uring));
  v_io_uring = caml_alloc_small(1, Abstract_tag);

  // TOIMPL : make it possible to set IORING_SETUP_IOPOLL and IORING_SETUP_SQPOLL here.
  retcode = io_uring_queue_init(Int_val(v_submission_entries),
                               io_uring,
                               0);

  if (retcode < 0) uerror("io_uring_queue_init", Nothing);

  Io_uring_val(v_io_uring) = io_uring;
  CAMLreturn(v_io_uring);
}

CAMLprim value io_uring_queue_exit_stub(value v_io_uring)
{
  CAMLparam1(v_io_uring);

  io_uring_queue_exit(Io_uring_val(v_io_uring));
  caml_stat_free(Io_uring_val(v_io_uring));

  CAMLreturn(Val_unit);
}

#define Val_none_user_data Val_int(0)
#define Val_some_user_data(user_data) (Val_long((uintptr_t) user_data))
#define User_data_val(v_user_data) ((void *) (uintptr_t) Unsigned_long_val(v_user_data))

// user data will just be a pointer (outside of the OCaml heap) so can never
// be LIBURING_UDATA_TIMEOUT (i.e. 18446744073709551615), the value
// forbidden by liburing.
//
// it's tempting to keep v_a "unboxed" by not allocating when it is an
// immediate value, but then it becomes an issue when the user passes the same
// value multiple times to io_uring_prep_poll_add_stub and then tries to cancel
// via io_uring_prep_poll_remove_stub.
void *create_user_data(value v_a) {
  value *v_a_p = caml_stat_alloc(sizeof(value));
  *v_a_p = v_a;
  caml_register_generational_global_root(v_a_p);
  // debug: printf("create_user_data: %llx, %llx, %llx\n", Val_some_user_data(v_a_p), v_a_p);
  return (void *) v_a_p;
}

CAMLprim value io_uring_prep_nop_stub(value v_io_uring, value v_a)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  if (sqe == NULL) {
    return Val_none_user_data;
  } else {
    void *v_a_p = create_user_data(v_a);
    io_uring_prep_nop(sqe);
    io_uring_sqe_set_data(sqe, v_a_p);
    return Val_some_user_data(v_a_p);
  }
}

CAMLprim value io_uring_prep_poll_add_stub(value v_io_uring, value v_fd, value v_flags, value v_a)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  if (sqe == NULL) {
    return Val_none_user_data;
  } else {
    void *v_a_p = create_user_data(v_a);
    io_uring_prep_poll_add(sqe,
                          (int) Long_val(v_fd),
                          (short) Int63_val(v_flags));
    io_uring_sqe_set_data(sqe, v_a_p);
    return Val_some_user_data(v_a_p);
  }
}

CAMLprim value io_uring_prep_poll_remove_stub(value v_io_uring, value v_a)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  // debug: puts("entered io_uring_prep_poll_remove");

  if (sqe == NULL) {
    return Val_bool(false);
  } else {
    // debug: printf("poll_remove: tag: %llx, %llx\n", v_a, User_data_val(v_a));
    value *v_a_p = User_data_val(v_a);
    io_uring_prep_poll_remove((struct io_uring_sqe *) Data_abstract_val(sqe), v_a_p);
    io_uring_sqe_set_data(sqe, NULL);
    return Val_bool(false);
  }
}

//CAMLprim value io_uring_prep_writev_stub(value v_io_uring, value v_fd, value v_iovecs) {
//  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
//}

CAMLprim value io_uring_submit_stub(value v_io_uring)
{
  return Val_int(io_uring_submit(Io_uring_val(v_io_uring)));
}

#define NSECS_IN_SEC 1000000000LL

// TODO: possibly release runtime lock for longer periods of time?
CAMLprim value io_uring_wait_stub(value v_io_uring, value v_array, value v_timeout)
{
  CAMLparam3(v_io_uring, v_array, v_timeout);
  int retcode;
  struct io_uring_cqe *cqe;
  long long timeout = Long_val(v_timeout);
  struct io_uring *io_uring = Io_uring_val(v_io_uring);

  /*
   * timeout, in nanoseconds returns immediately if 0 is given, waits
   * forever with -1 (similar to epoll_wait()).
   */
  if (timeout == 0) {
    /* returns immediately, skip enter()/leave() pair */
    retcode = io_uring_peek_cqe(io_uring, &cqe);

    // TOIMPL: under heavy load, we sometimes seem to get ETIME; should investigate
    //
    if (retcode != -EAGAIN && retcode != -ETIME && retcode < 0) {
      printf("error %d (%s)\n", -retcode, strerror(-retcode));
      printf("cqe ptr: %lu\n", (uint64_t) cqe);
      uerror("io_uring_peek_cqe", Nothing);
    }
  } else if (timeout < 0) {

    caml_enter_blocking_section();
    retcode = io_uring_wait_cqe(io_uring, &cqe);
    caml_leave_blocking_section();

    if (retcode < 0) uerror("io_uring_wait_cqe", Nothing);
  } else {
    struct __kernel_timespec ts = {
      .tv_sec = timeout / NSECS_IN_SEC,
      .tv_nsec = timeout % NSECS_IN_SEC
    };

    caml_enter_blocking_section();
    retcode = io_uring_wait_cqe_timeout(io_uring, &cqe, &ts);
    caml_leave_blocking_section();

    if (retcode != -ETIME && retcode < 0) {
      printf("error %d (%s)\n", -retcode, strerror(-retcode));
      printf("cqe ptr: %lu\n", (uint64_t) cqe);
      uerror("io_uring_wait_cqe_timeout", Nothing);
    }
  }

  struct io_uring_cqe *buffer = (struct io_uring_cqe *) Caml_ba_data_val(v_array);
  int num_seen = 0;
  int max_cqes = Caml_ba_array_val(v_array)->dim[0] / sizeof(struct io_uring_cqe);

  while (cqe != NULL && num_seen < max_cqes) {
    memcpy(buffer, cqe, sizeof(struct io_uring_cqe));

    // debug: puts("io_uring_wait: in loop");

    io_uring_cqe_seen(Io_uring_val(v_io_uring), cqe);

    retcode = io_uring_peek_cqe(Io_uring_val(v_io_uring), &cqe);

    if (retcode != -EAGAIN && retcode != -ETIME && retcode < 0) {
      printf("error %d (%s)\n", -retcode, strerror(-retcode));
      printf("cqe ptr: %lu\n", (uint64_t) cqe);
      uerror("io_uring_peek_cqe", Nothing);
    }

    // skip results from io_uring_prep_poll_remove
    if ((void *) buffer->user_data != NULL) {
      num_seen++;
      buffer++;
    }
  }

  CAMLreturn(Val_int(num_seen));
}

#define Index_user_data(bs, i) (((struct io_uring_cqe *) Caml_ba_data_val(bs) + i)->user_data)

CAMLprim value io_uring_get_user_data(value v_array, value v_index) {
  value *v_a_p = (value *) Index_user_data(v_array, Int_val(v_index));
  // debug: printf("io_uring_get_user_data: %llx, %llx\n", Val_some_user_data(v_a_p), v_a_p);
  return *v_a_p;
}

// cqe->user_data doesn't contain OCaml values which are passed in, but rather
// a pointer to it which is located on the heap (so it doesn't become invalid
// while the kernel is processing the sqe) and registered as a global root (so
// what it points to is updated accordingly).
CAMLprim value io_uring_clear_completions(value v_array, value v_n) {
  int n = Int_val(v_n);
  for (int i = 0; i < n; i++) {
    value *v_a_p = (value *) Index_user_data(v_array, i);
    caml_remove_generational_global_root(v_a_p);
    caml_stat_free(v_a_p);
    Index_user_data(v_array, i) = 0;
  }
}
