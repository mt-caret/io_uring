#include "io_uring_stubs.h"
#include <fcntl.h>

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

CAMLprim value io_uring_queue_init_stub(value v_submission_entries, value v_completion_entries)
{
  struct io_uring_params p;
  CAMLparam2(v_submission_entries, v_completion_entries);
  CAMLlocal1(v_io_uring);

  memset(&p, 0, sizeof(p));
  p.flags = IORING_SETUP_CQSIZE | IORING_FEAT_RW_CUR_POS;
  p.cq_entries = Int_val(v_completion_entries);

  struct io_uring *io_uring = caml_stat_alloc(sizeof(struct io_uring));
  v_io_uring = caml_alloc_small(1, Abstract_tag);

  // TOIMPL : make it possible to set IORING_SETUP_IOPOLL and IORING_SETUP_SQPOLL here.
  int retcode = io_uring_queue_init(Int_val(v_submission_entries), io_uring, 0);

  if (retcode < 0) {
    uerror("io_uring_queue_init", Nothing);
  }

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

CAMLprim value io_uring_prep_nop_stub(value v_io_uring, value v_sqe_flags, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));

  if (sqe == NULL) {
    return Val_bool(true);
  }

  io_uring_prep_nop(sqe);
  io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));
  debug("user_data: %d\n", v_user_data);
  io_uring_sqe_set_data(sqe, (void *)(uintptr_t) v_user_data);
  return Val_bool(false);
}

CAMLprim value io_uring_prep_open_stub(value v_io_uring, value v_sqe_flags, value v_path, value v_flags, value v_mode, value v_bstr, value v_bstr_pos, value v_bstr_len, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));

  if (sqe == NULL) {
    return Val_bool(true);
  }

  if (Long_val(v_bstr_len) < sizeof(struct open_how)) {
    uerror("bstr memory", Nothing);
  }

  struct open_how* flags = (struct open_how*) get_bstr(v_bstr, v_bstr_pos);

  flags->flags = Long_val(v_flags);
  flags->mode = Long_val(v_mode);

  debug("open parameters: %li %li\n", flags->flags, flags->mode);

  io_uring_prep_openat2(sqe, AT_FDCWD, String_val(v_path), flags);
  io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));

  debug("user_data: %d\n", v_user_data);

  io_uring_sqe_set_data(sqe, (void *)(uintptr_t) v_user_data);

  return Val_bool(false);
}

CAMLprim value io_uring_prep_open_bytecode_stub(value *argv, int argn)
{
  return io_uring_prep_open_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8]);
}

CAMLprim value io_uring_prep_write_stub(value v_io_uring, value v_sqe_flags, value v_fd, value v_pos, value v_len, value v_bstr, value v_offset, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));

  if (sqe == NULL) {
    return Val_bool(true);
  }

  io_uring_prep_write(sqe,
                      (int) Long_val(v_fd),
                      get_bstr(v_bstr, v_pos),
                      (unsigned) Long_val(v_len),
                      (off_t) Long_val(v_offset));

  io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));

  debug("user_data: %d\n", v_user_data);

  io_uring_sqe_set_data(sqe, (void *)(uintptr_t) v_user_data);

  return Val_bool(false);
}

CAMLprim value io_uring_prep_write_bytecode_stub(value *argv, int argn)
{
  return io_uring_prep_write_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7]);
}

CAMLprim value io_uring_prep_read_stub(value v_io_uring, value v_sqe_flags, value v_fd, value v_pos, value v_len, value v_bstr, value v_offset, value v_user_data) {

  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  
  if (sqe == NULL) {
    return Val_bool(true);
  }

  io_uring_prep_read(sqe,
                      (int) Long_val(v_fd),
                      get_bstr(v_bstr, v_pos),
                      (unsigned) Long_val(v_len),
                      (off_t) Long_val(v_offset));
  io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));
  debug("user_data: %d\n", v_user_data);
  io_uring_sqe_set_data(sqe, (void *)(uintptr_t) v_user_data);
  return Val_bool(false);
}

CAMLprim value io_uring_prep_read_bytecode_stub(value *argv, int argn)
{
  return io_uring_prep_read_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7]);
}

typedef enum {
  IOVECS,
  SOCKADDR,
  MSGHDR
} tag_type;

struct queued_sockaddr {
  union sock_addr_union addr;
  socklen_t addr_len;
  bool completed;
  int retcode;
};

struct tagged_immediate {
  tag_type tag_type;
  union {
    struct iovec *iovecs;
    struct queued_sockaddr *sockaddr;
    struct msghdr *msghdr;
  };
  value immediate;
};

// TODO: we should CAMLparam here
CAMLprim value io_uring_prep_writev_stub(value v_io_uring, value v_sqe_flags, value v_fd, value v_iovecs, value v_count, value v_offset, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    int count = Int_val(v_count);
    size_t total_len = 0;

    assert(Is_long(v_user_data));
    struct tagged_immediate* user_data =
      caml_stat_alloc(sizeof(struct tagged_immediate));
    assert(Is_block((uintptr_t)user_data));
    user_data->tag_type = IOVECS;
    user_data->iovecs = copy_iovecs(&total_len, v_iovecs, count);
    user_data->immediate = v_user_data;

    io_uring_prep_writev(sqe,
                        (int) Long_val(v_fd),
                        user_data->iovecs,
                        count,
                        (off_t) Long_val(v_offset));
    io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));
    debug("user_data: %d\n", v_user_data);
    io_uring_sqe_set_data(sqe, (void *) user_data);
    return Val_bool(false);
  }
}

CAMLprim value io_uring_prep_writev_bytecode_stub(value *argv, int argn) {
  return io_uring_prep_writev_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value io_uring_prep_readv_stub(value v_io_uring, value v_sqe_flags, value v_fd, value v_iovecs, value v_count, value v_offset, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    int count = Int_val(v_count);
    size_t total_len = 0;

    assert(Is_long(v_user_data));
    struct tagged_immediate* user_data =
      caml_stat_alloc(sizeof(struct tagged_immediate));
    assert(Is_block((uintptr_t)user_data));
    user_data->tag_type = IOVECS;
    user_data->iovecs = copy_iovecs(&total_len, v_iovecs, count);
    user_data->immediate = v_user_data;

    io_uring_prep_readv(sqe,
                        (int) Long_val(v_fd),
                        user_data->iovecs,
                        count,
                        (off_t) Long_val(v_offset));
    io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));
    debug("user_data: %d\n", v_user_data);
    io_uring_sqe_set_data(sqe, (void *) user_data);
    return Val_bool(false);
  }
}

CAMLprim value io_uring_prep_readv_bytecode_stub(value *argv, int argn) {
  return io_uring_prep_readv_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value io_uring_prep_send_stub(value v_io_uring, value v_sqe_flags, value v_fd, value v_pos, value v_len, value v_bstr, value v_user_data) {
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  debug("io_uring_prep_send_stub");
  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    // TODO: possibly pass some flags to send()?
    io_uring_prep_send(sqe,
                        (int) Long_val(v_fd),
                        get_bstr(v_bstr, v_pos),
                        (unsigned) Long_val(v_len),
                        0);
    io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));
    debug("user_data: %d\n", v_user_data);
    io_uring_sqe_set_data(sqe, (void *)(uintptr_t) v_user_data);
    return Val_bool(false);
  }
}

CAMLprim value io_uring_prep_send_bytecode_stub(value *argv, int argn) {
  return io_uring_prep_send_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value io_uring_prep_recv_stub(value v_io_uring, value v_sqe_flags, value v_fd, value v_pos, value v_len, value v_bstr, value v_user_data) {
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  debug("io_uring_prep_recv_stub\n");
  if (!sqe) {
    return Val_bool(true);
  }

  // TODO: possibly pass some flags to recv()?
  io_uring_prep_recv(sqe,
                      (int) Long_val(v_fd),
                      get_bstr(v_bstr, v_pos),
                      (unsigned) Long_val(v_len),
                      0);
  io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));
  debug("user_data: %d\n", v_user_data);
  io_uring_sqe_set_data(sqe, (void *)(uintptr_t) v_user_data);

  return Val_bool(false);
}

CAMLprim value io_uring_prep_recv_bytecode_stub(value *argv, int argn) {
  return io_uring_prep_recv_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value io_uring_prep_sendmsg_stub(value v_io_uring, value v_sqe_flags, value v_fd, value v_iovecs, value v_count, value v_user_data) {
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  debug("io_uring_prep_sendmsg_stub");
  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    int count = Int_val(v_count);
    size_t total_len = 0;

    struct msghdr *msghdr = caml_stat_alloc(sizeof(struct msghdr));
    msghdr->msg_name = (void *) NULL;
    msghdr->msg_namelen = 0;
    msghdr->msg_iov = copy_iovecs(&total_len, v_iovecs, count);
    msghdr->msg_iovlen = count;
    msghdr->msg_control = (void *) NULL;
    msghdr->msg_controllen = 0;
    msghdr->msg_flags = 0;

    assert(Is_long(v_user_data));
    struct tagged_immediate* user_data =
      caml_stat_alloc(sizeof(struct tagged_immediate));
    assert(Is_block((uintptr_t)user_data));
    user_data->tag_type = MSGHDR;
    user_data->msghdr = msghdr;
    user_data->immediate = v_user_data;

    // TODO: possibly pass some flags to send()?
    io_uring_prep_sendmsg(sqe,
                        (int) Long_val(v_fd),
                        msghdr,
                        0);
    io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));
    debug("user_data: %d\n", v_user_data);
    io_uring_sqe_set_data(sqe, (void *) user_data);
    return Val_bool(false);
  }
}

CAMLprim value io_uring_prep_sendmsg_bytecode_stub(value *argv, int argn) {
  return io_uring_prep_sendmsg_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value io_uring_prep_recvmsg_stub(value v_io_uring, value v_sqe_flags, value v_fd, value v_iovecs, value v_count, value v_user_data) {
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  debug("io_uring_prep_recvmsg_stub\n");
  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    int count = Int_val(v_count);
    size_t total_len = 0;

    struct msghdr *msghdr = caml_stat_alloc(sizeof(struct msghdr));
    msghdr->msg_name = (void *) NULL;
    msghdr->msg_namelen = 0;
    msghdr->msg_iov = copy_iovecs(&total_len, v_iovecs, count);
    msghdr->msg_iovlen = count;
    msghdr->msg_control = (void *) NULL;
    msghdr->msg_controllen = 0;
    msghdr->msg_flags = 0;

    assert(Is_long(v_user_data));
    struct tagged_immediate* user_data =
      caml_stat_alloc(sizeof(struct tagged_immediate));
    assert(Is_block((uintptr_t)user_data));
    user_data->tag_type = MSGHDR;
    user_data->msghdr = msghdr;
    user_data->immediate = v_user_data;

    // TODO: possibly pass some flags to recv()?
    io_uring_prep_recvmsg(sqe,
                        (int) Long_val(v_fd),
                        msghdr,
                        0);
    io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));
    debug("user_data: %d\n", v_user_data);
    io_uring_sqe_set_data(sqe, (void *) user_data);
    return Val_bool(false);
  }
}

CAMLprim value io_uring_prep_recvmsg_bytecode_stub(value *argv, int argn) {
  return io_uring_prep_recvmsg_stub(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value io_uring_prep_close_stub(value v_io_uring, value v_sqe_flags, value v_fd, value v_user_data) {
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    io_uring_prep_close(sqe, (int) Long_val(v_fd));
    io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));
    io_uring_sqe_set_data(sqe, (void *)(uintptr_t) v_user_data);
    return Val_bool(false);
  }
}

#define Queued_sockaddr_val(v) *((struct tagged_immediate **) Data_abstract_val(v))

CAMLprim value io_uring_prep_accept_stub(value v_io_uring, value v_sqe_flags, value v_fd, value v_user_data)
{
  CAMLparam2(v_io_uring, v_user_data);
  CAMLlocal1(v);

  // debug: puts("io_uring_prep_accept_stub");

  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  if (sqe == NULL) {
    CAMLreturn(Val_none);
  } else {
    struct queued_sockaddr* p = caml_stat_alloc(sizeof(struct queued_sockaddr));
    assert (Is_block((uintptr_t) p));
    p->addr_len = sizeof(union sock_addr_union);
    p->completed = false;

    assert(Is_long(v_user_data));
    struct tagged_immediate* user_data =
      caml_stat_alloc(sizeof(struct tagged_immediate));
    assert(Is_block((uintptr_t)user_data));
    user_data->tag_type = SOCKADDR;
    user_data->sockaddr = p;
    user_data->immediate = v_user_data;

    // TODO: support accept4() flags?
    io_uring_prep_accept(sqe, (int) Long_val(v_fd), &(p->addr.s_gen), &(p->addr_len), 0);
    io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));
    io_uring_sqe_set_data(sqe, (void *) user_data);

    v = caml_alloc(1, Abstract_tag);
    Queued_sockaddr_val(v) = user_data;

    CAMLreturn(Val_some(v));
  }
}

CAMLprim value io_uring_get_sockaddr(value v_queued_sockaddr) {
  CAMLparam1(v_queued_sockaddr);
  CAMLlocal1(sockaddr);

  debug("io_uring_get_sockaddr\n");

  struct tagged_immediate *p = Queued_sockaddr_val(v_queued_sockaddr);
  assert(p->tag_type == SOCKADDR);

  struct queued_sockaddr *q = p->sockaddr;
  if (q->completed) {
    CAMLreturn(Val_some(alloc_sockaddr(&(q->addr), q->addr_len, q->retcode)));
  } else {
    CAMLreturn(Val_none);
  }
}

CAMLprim value io_uring_free_sockaddr(value v_queued_sockaddr) {
  debug("io_uring_free_sockaddr\n");
  struct tagged_immediate *p = Queued_sockaddr_val(v_queued_sockaddr);
  assert(p->tag_type == SOCKADDR);
  caml_stat_free(p->sockaddr);
  p->sockaddr = NULL;
  caml_stat_free(p);
}

CAMLprim value io_uring_prep_poll_add_stub(value v_io_uring, value v_sqe_flags, value v_fd, value v_flags, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  if (sqe == NULL) {
    return Val_bool(true);
  } else {
    io_uring_prep_poll_add(sqe,
                          (int) Long_val(v_fd),
                          (short) Int63_val(v_flags));
    io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));
    debug("user_data: %d\n", v_user_data);
    io_uring_sqe_set_data(sqe, (void *)(uintptr_t) v_user_data);
    return Val_bool(false);
  }
}

CAMLprim value io_uring_prep_poll_add_bytecode_stub(value *argv, int argn){
  return io_uring_prep_poll_add_stub(argv[0], argv[1], argv[2], argv[3], argv[4]);
}

CAMLprim value io_uring_prep_poll_remove_stub(value v_io_uring, value v_sqe_flags, value v_user_data)
{
  struct io_uring_sqe *sqe = io_uring_get_sqe(Io_uring_val(v_io_uring));
  debug("entered io_uring_prep_poll_remove\n");

  if (sqe) {
    // debug: printf("poll_remove: tag: %llx, %llx\n", v_a, User_data_val(v_a));
    io_uring_prep_poll_remove((struct io_uring_sqe *) Data_abstract_val(sqe),
                              (void *)(uintptr_t) v_user_data);
    io_uring_sqe_set_flags(sqe, Int63_val(v_sqe_flags));
    io_uring_sqe_set_data(sqe, NULL);
    return Val_bool(false);
  } else {
    debug("returning none\n");
    return Val_bool(true);
  }
}

CAMLprim value io_uring_submit_stub(value v_io_uring)
{
  int retcode = io_uring_submit(Io_uring_val(v_io_uring));

  if (retcode < 0) {
    uerror("io_uring_submit", Nothing);
  }

  return Val_int(retcode);
}

#define NSECS_IN_SEC 1000000000LL

// TODO: possibly release runtime lock for longer periods of time?
CAMLprim value io_uring_wait_stub(value v_io_uring, value v_array, value v_timeout) {
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
    retcode = io_uring_wait_cqe_nr(io_uring, &cqe, -timeout);
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

    debug("io_uring_wait: in loop\n");

    io_uring_cqe_seen(Io_uring_val(v_io_uring), cqe);

    retcode = io_uring_peek_cqe(Io_uring_val(v_io_uring), &cqe);

    if (retcode != -EAGAIN && retcode != -ETIME && retcode < 0) {
      printf("error %d (%s)\n", -retcode, strerror(-retcode));
      printf("cqe ptr: %lu\n", (uint64_t) cqe);
      uerror("io_uring_peek_cqe (loop)", Nothing);
    }

    // skip results from io_uring_prep_poll_remove
    if ((void *) buffer->user_data != NULL) {
      // debug: printf("handling user_data: %lld\n", buffer->user_data);

      if (Is_block(buffer->user_data)) {
        struct tagged_immediate *p = (struct tagged_immediate *) buffer->user_data;
        buffer->user_data = p->immediate;
        assert(Is_long(buffer->user_data));

        switch (p->tag_type) {
          case IOVECS:
            caml_stat_free(p->iovecs);
            caml_stat_free(p);
            break;
          case SOCKADDR:
            // debug: puts("found sockaddr!");
            p->sockaddr->completed = true;
            p->sockaddr->retcode = retcode;
            // debug: puts("finished handling sockaddr");
            break;
          case MSGHDR:
            caml_stat_free(p->msghdr->msg_iov);
            caml_stat_free(p->msghdr);
            break;
          default:
            assert(false);
        }
      }

      num_seen++;
      buffer++;
    }
  }

  debug("out of loop\n");

  CAMLreturn(Val_int(num_seen));
}

CAMLprim value io_uring_get_user_data(value v_array, value v_index) {
  debug("io_uring_get_user_data\n");

  value user_data = ((struct io_uring_cqe *) Caml_ba_data_val(v_array) + Int_val(v_index))->user_data;
  return
    Is_block(user_data) ?
    ((struct tagged_immediate *) user_data)->immediate :
    user_data;
}
