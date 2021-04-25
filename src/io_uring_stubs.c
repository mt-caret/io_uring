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

CAMLprim value io_uring_prep_nop_stub(value v_io_uring, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    io_uring_prep_nop(sqe);
    // debug: printf("user_data: %d\n", v_user_data);
    io_uring_sqe_set_data(sqe, (void *)(uintptr_t) v_user_data);
    return Val_bool(false);
  }
}

CAMLprim value io_uring_prep_write_stub(value v_io_uring, value v_fd, value v_pos, value v_len, value v_bstr, value v_offset, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    io_uring_prep_write(sqe,
                        (int) Long_val(v_fd),
                        get_bstr(v_bstr, v_pos),
                        (unsigned) Long_val(v_len),
                        (off_t) Long_val(v_offset));
    // debug: printf("user_data: %d\n", v_user_data);
    io_uring_sqe_set_data(sqe, (void *)(uintptr_t) v_user_data);
    return Val_bool(false);
  }
}

CAMLprim value io_uring_prep_write_bytecode_stub(value *argv, int argn)
{
  return io_uring_prep_write_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value io_uring_prep_read_stub(value v_io_uring, value v_fd, value v_pos, value v_len, value v_bstr, value v_offset, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    io_uring_prep_read(sqe,
                        (int) Long_val(v_fd),
                        get_bstr(v_bstr, v_pos),
                        (unsigned) Long_val(v_len),
                        (off_t) Long_val(v_offset));
    // debug: printf("user_data: %d\n", v_user_data);
    io_uring_sqe_set_data(sqe, (void *)(uintptr_t) v_user_data);
    return Val_bool(false);
  }
}

CAMLprim value io_uring_prep_read_bytecode_stub(value *argv, int argn)
{
  return io_uring_prep_read_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

struct iovecs_and_immediate {
  struct iovec *iovecs;
  value immediate;
};

CAMLprim value io_uring_prep_writev_stub(value v_io_uring, value v_fd, value v_iovecs, value v_count, value v_offset, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    int count = Int_val(v_count);
    size_t total_len = 0;

    assert(Is_long(v_user_data));
    struct iovecs_and_immediate* user_data =
      caml_stat_alloc(sizeof(struct iovecs_and_immediate));
    user_data->immediate = v_user_data;
    user_data->iovecs = copy_iovecs(&total_len, v_iovecs, count);
    assert(Is_block((intptr_t)(void *)user_data));

    io_uring_prep_writev(sqe,
                        (int) Long_val(v_fd),
                        user_data->iovecs,
                        count,
                        (off_t) Long_val(v_offset));
    // debug: printf("user_data: %d\n", v_user_data);
    io_uring_sqe_set_data(sqe, (void *) user_data);
    return Val_bool(false);
  }
}

CAMLprim value io_uring_prep_writev_bytecode_stub(value *argv, int argn) {
  return io_uring_prep_writev_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value io_uring_prep_readv_stub(value v_io_uring, value v_fd, value v_iovecs, value v_count, value v_offset, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    int count = Int_val(v_count);
    size_t total_len = 0;

    assert(Is_long(v_user_data));
    struct iovecs_and_immediate* user_data =
      caml_stat_alloc(sizeof(struct iovecs_and_immediate));
    user_data->immediate = v_user_data;
    user_data->iovecs = copy_iovecs(&total_len, v_iovecs, count);
    assert(Is_block((intptr_t)(void *)user_data));

    io_uring_prep_readv(sqe,
                        (int) Long_val(v_fd),
                        user_data->iovecs,
                        count,
                        (off_t) Long_val(v_offset));
    // debug: printf("user_data: %d\n", v_user_data);
    io_uring_sqe_set_data(sqe, (void *) user_data);
    return Val_bool(false);
  }
}

CAMLprim value io_uring_prep_readv_bytecode_stub(value *argv, int argn) {
  return io_uring_prep_readv_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value io_uring_prep_poll_add_stub(value v_io_uring, value v_fd, value v_flags, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    io_uring_prep_poll_add(sqe,
                          (int) Long_val(v_fd),
                          (short) Int63_val(v_flags));
    // debug: printf("user_data: %d\n", v_user_data);
    io_uring_sqe_set_data(sqe, (void *)(uintptr_t) v_user_data);
    return Val_bool(false);
  }
}

CAMLprim value io_uring_prep_poll_remove_stub(value v_io_uring, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  // debug: puts("entered io_uring_prep_poll_remove");

  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    // debug: printf("poll_remove: tag: %llx, %llx\n", v_a, User_data_val(v_a));
    io_uring_prep_poll_remove((struct io_uring_sqe *) Data_abstract_val(sqe),
                              (void *)(uintptr_t) v_user_data);
    io_uring_sqe_set_data(sqe, NULL);
    return Val_bool(false);
  }
}

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

    // TOIMPL: not sure why we get ETIMEs here
    if (retcode != -EAGAIN && retcode != -ETIME && retcode < 0) {
      printf("error %d (%s)\n", -retcode, strerror(-retcode));
      printf("cqe ptr: %lu\n", (uint64_t) cqe);
      uerror("io_uring_peek_cqe (if branch)", Nothing);
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

  while (cqe != NULL && num_seen < max_cqes && retcode != -EAGAIN && retcode != -ETIME) {
    memcpy(buffer, cqe, sizeof(struct io_uring_cqe));

    // debug: puts("io_uring_wait: in loop");

    io_uring_cqe_seen(Io_uring_val(v_io_uring), cqe);

    retcode = io_uring_peek_cqe(Io_uring_val(v_io_uring), &cqe);

    if (retcode != -EAGAIN && retcode != -ETIME && retcode < 0) {
      printf("error %d (%s)\n", -retcode, strerror(-retcode));
      printf("cqe ptr: %lu\n", (uint64_t) cqe);
      uerror("io_uring_peek_cqe (loop)", Nothing);
    }

    // skip results from io_uring_prep_poll_remove
    if ((void *) buffer->user_data != NULL) {
      if (Is_block(buffer->user_data)) {
        struct iovecs_and_immediate *p =
          (struct iovecs_and_immediate *) buffer->user_data;
        buffer->user_data = p->immediate;
        caml_stat_free(p->iovecs);
        caml_stat_free(p);
        assert(Is_long(buffer->user_data));
      }

      num_seen++;
      buffer++;
    }
  }

  CAMLreturn(Val_int(num_seen));
}

CAMLprim value io_uring_get_user_data(value v_array, value v_index) {
  return ((struct io_uring_cqe *) Caml_ba_data_val(v_array) + Int_val(v_index))->user_data;
}
